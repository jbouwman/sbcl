;;;; The fragment writer: lay a sealed fragment out at its planned
;;;; addresses, packed by page type, and write it as a fragment file.
;;;; Loaded into a build process after corefile.lisp, as editcore is.
;;;;
;;;; A fragment file holds page runs, one per page type (cons, mixed,
;;;; code) plus one per large object, each run page-aligned and described
;;;; page by page as the runtime's page table describes a core's pages:
;;;; words used, the large-object flag, the scan start offset, and the
;;;; allocation bitmap with one bit per 16 bytes at each object start.
;;;; A pointer map marks every word of a run that holds a pointer into
;;;; the fragment, which is how an activator relocates it without a walk.
;;;; References to objects outside the fragment are left as the builder
;;;; had them and listed as imports; resolving them by name is the
;;;; activator's business, not the writer's.  The references the
;;;; fragment's code makes through linkage cells are listed by site, with
;;;; the name each cell is for: the builder's cells are its own, and the
;;;; activator resolves the names again and patches the references.

(defpackage "SB-COREFRAG-WRITER"
  (:use "CL")
  (:export #:write-fragment #:verify-fragment-file #:fragment-file-stats
           #:fragment-file-error))

(in-package "SB-COREFRAG-WRITER")

(defvar *verbose* nil "Print the writer's stages to *TRACE-OUTPUT*; :OBJECTS prints every object and import too.")
(defun stage (name) (when *verbose* (format *trace-output* "~&;; writer: ~A~%" name) (finish-output *trace-output*)))

(defconstant +magic+ #x53424652)        ; "SBFR"
(defconstant +version+ 2)
(defconstant +page-bytes+ sb-vm:gencgc-page-bytes)
(defconstant +word-bytes+ sb-vm:n-word-bytes)
;;; As collector_alloc_fallback decides: three quarters of a page or more
;;; takes pages of its own.
(defconstant +large-object-bytes+ (* 3 (floor +page-bytes+ 4)))
;;; One allocation bit per cons-sized unit, as the collector's bitmap.
(defconstant +alloc-unit+ (* 2 +word-bytes+))

;;; The runtime's page types (pmrgc-impl.h).
(defconstant +type-mixed+ 3)
(defconstant +type-cons+ 5)
(defconstant +type-code+ 7)

;;; Section ids.
(defconstant +section-identity+ 1)
(defconstant +section-pages+ 2)
(defconstant +section-pointer-map+ 3)
(defconstant +section-exports+ 4)
(defconstant +section-imports+ 5)
(defconstant +section-records+ 6)
(defconstant +section-provenance+ 7)
(defconstant +section-linkage+ 8)

;;; The linkage index an fname carries in its second word, which the
;;; builder assigned: FNAME-LINKAGE-INDEX reads it from bit 3 up, the low
;;; three bits being flags, of a symbol and of an fdefn alike.  Zero on a
;;; target without linkage space.
(defconstant +fname-index-mask+ (ash (1- (ash 1 sb-vm:n-linkage-index-bits)) 3))

(define-condition fragment-file-error (error)
  ((message :initarg :message :reader fragment-file-error-message))
  (:report (lambda (c s) (write-string (fragment-file-error-message c) s))))

(defun fail (control &rest args)
  (error 'fragment-file-error :message (apply #'format nil control args)))

;;;; Runs

(defstruct (run (:constructor make-run (type base large)))
  (type 0 :type fixnum)                 ; a page type
  (base 0 :type sb-vm:word)             ; planned address of the first page
  (large nil)                           ; a single object on its own pages
  (fill 0 :type fixnum)                 ; bytes laid out so far
  (objects '())                         ; (old-raw new-raw nbytes object), reversed
  (buffer nil)                          ; (unsigned-byte 8) vector once copied
  (pointer-bits nil)                    ; one bit per word
  (alloc-bits nil))                     ; one bit per +alloc-unit+

(defun run-npages (run)
  (ceiling (run-fill run) +page-bytes+))

(defun run-nbytes (run)
  (* (run-npages run) +page-bytes+))

(defun object-page-type (x)
  (cond ((consp x) +type-cons+)
        ((typep x 'sb-kernel:code-component) +type-code+)
        (t +type-mixed+)))

(defun lowtag-of (x) (logand (sb-kernel:get-lisp-obj-address x) sb-vm:lowtag-mask))

;;; Place NBYTES at the run's fill pointer; an object never crosses a page.
(defun run-place (run nbytes)
  (let* ((fill (run-fill run))
         (page-end (* (ceiling (1+ fill) +page-bytes+) +page-bytes+)))
    (when (and (> (+ fill nbytes) page-end) (<= nbytes +page-bytes+))
      (setf fill page-end))
    (setf (run-fill run) (+ fill nbytes))
    fill))

;;;; Layout

(defstruct (layout (:constructor %make-layout))
  (runs '())                            ; in address order
  (forward (make-hash-table))           ; old tagged address -> new tagged address
  (forwarded (make-hash-table))         ; the new tagged addresses, to make forwarding idempotent
  (code-ranges '())                     ; (old-raw-start old-raw-end delta) per code object
  (heap-id 0)
  (members 0)
  (member-bytes 0))

(defun collect-members (recorder)
  (let ((members '()))
    (sb-fiber:map-fragment-members (lambda (x) (push x members)) recorder)
    (nreverse members)))

;;; Assign every member a planned address: the small objects of each page
;;; type packed into one run per type, in the order cons, mixed, code,
;;; and each large object on pages of its own after them.
(defun lay-out (members base heap-id)
  (let* ((layout (%make-layout :heap-id heap-id))
         (small (list (make-run +type-cons+ 0 nil)
                      (make-run +type-mixed+ 0 nil)
                      (make-run +type-code+ 0 nil)))
         (large '()))
    (dolist (x members)
      (let* ((nbytes (sb-ext:primitive-object-size x))
             (type (object-page-type x))
             (run (if (>= nbytes +large-object-bytes+)
                      (let ((r (make-run type 0 t))) (push r large) r)
                      (find type small :key #'run-type))))
        (push (list (- (sb-kernel:get-lisp-obj-address x) (lowtag-of x))
                    (run-place run nbytes) nbytes x)
              (run-objects run))
        (incf (layout-members layout))
        (incf (layout-member-bytes layout) nbytes)))
    ;; Planned addresses: runs follow one another from BASE, page-aligned.
    (let ((address base))
      (dolist (run (append (remove-if (lambda (r) (null (run-objects r))) small)
                           (nreverse large)))
        (setf (run-base run) address)
        (incf address (run-nbytes run))
        (setf (run-objects run) (nreverse (run-objects run)))
        (dolist (entry (run-objects run))
          (destructuring-bind (old-raw offset nbytes x) entry
            (let ((new-raw (+ (run-base run) offset)))
              (setf (second entry) new-raw)
              (setf (gethash (+ old-raw (lowtag-of x)) (layout-forward layout))
                    (+ new-raw (lowtag-of x)))
              (setf (gethash (+ new-raw (lowtag-of x)) (layout-forwarded layout)) t)
              (when (typep x 'sb-kernel:code-component)
                (push (list old-raw (+ old-raw nbytes) (- new-raw old-raw))
                      (layout-code-ranges layout))))))
        (push run (layout-runs layout))))
    (setf (layout-runs layout) (nreverse (layout-runs layout)))
    layout))

;;;; Copying and relocation

(defun pointerp (word)
  ;; Every pointer lowtag on this platform is odd and ends in #b11.
  (= (logand word 3) 3))

(defun forward-code-address (layout raw)
  ;; An entry address inside a code object, forwarded with that object.
  (dolist (range (layout-code-ranges layout) nil)
    (destructuring-bind (start end delta) range
      (when (and (>= raw start) (< raw end))
        (return (+ raw delta))))))

;;; The planned address of the object WORD points at: a member, or a
;;; simple-fun inside a member code object, which moves with it.
(defun forward-address (layout word)
  (or (gethash word (layout-forward layout))
      (and (= (logand word sb-vm:lowtag-mask) sb-vm:fun-pointer-lowtag)
           (let ((raw (forward-code-address layout (- word sb-vm:fun-pointer-lowtag))))
             (and raw (+ raw sb-vm:fun-pointer-lowtag))))))

(defstruct (relocation-stats (:conc-name stats-))
  (pointers 0) (imports '()) (dangling '()))

;;; Relocate the word at byte OFFSET of RUN's buffer, which object X owns.
;;; TAGGED is false for a word that is only known to hold a pointer if it
;;; matches a member: then a miss records nothing.
(defun forward-word (layout run offset x stats &optional (tagged t))
  (let* ((buffer (run-buffer run))
         (word (sb-sys:sap-ref-word (sb-sys:vector-sap buffer) offset)))
    (when (and (pointerp word) (not (gethash word (layout-forwarded layout))))
      (let ((new (forward-address layout word)))
        (cond (new
               (setf (sb-sys:sap-ref-word (sb-sys:vector-sap buffer) offset) new)
               (setf (sbit (run-pointer-bits run) (floor offset +word-bytes+)) 1)
               (incf (stats-pointers stats)))
              ((not tagged))
              ((and (/= (layout-heap-id layout) 0)
                    (eql (ignore-errors (sb-vm::object-owner (sb-kernel:%make-lisp-obj word)))
                         (layout-heap-id layout)))
               ;; Into the build heap but not a member: the trace missed it.
               (push (list (+ (run-base run) offset) word (type-of x) (type-of (sb-kernel:%make-lisp-obj word))) (stats-dangling stats)))
              (t
               (push (list (+ (run-base run) offset) word (type-of x)) (stats-imports stats))))))))

(defun unboxed-p (x)
  (or (typep x '(or bignum float (complex float) sb-sys:system-area-pointer))
      (and (arrayp x) (sb-kernel:simple-array-p x) (not (simple-vector-p x)))))

(defun set-word-index-tagged (layout run offset x stats)
  ;; The layout word, then the slots the layout's bitmap marks as tagged.
  (forward-word layout run (+ offset (* sb-vm:instance-slots-offset +word-bytes+)) x stats)
  (sb-kernel:do-instance-tagged-slot (i x)
    (forward-word layout run (+ offset (* (+ i sb-vm:instance-slots-offset) +word-bytes+)) x stats)))

;;; Forward the tagged words of X, copied at byte OFFSET of RUN.
(defun relocate-object (layout run offset nbytes x stats)
  (let ((buffer (run-buffer run))
        (nwords (floor nbytes +word-bytes+)))
    (flet ((word-at (i) (sb-sys:sap-ref-word (sb-sys:vector-sap buffer) (+ offset (* i +word-bytes+))))
           (set-word (i v) (setf (sb-sys:sap-ref-word (sb-sys:vector-sap buffer) (+ offset (* i +word-bytes+))) v))
           (forward (i) (forward-word layout run (+ offset (* i +word-bytes+)) x stats))
           (forward-maybe (i) (forward-word layout run (+ offset (* i +word-bytes+)) x stats nil)))
      (typecase x
        (cons (forward 0) (forward 1))
        (simple-vector
         (loop for i from sb-vm:vector-data-offset below nwords do (forward i)))
        (sb-kernel:code-component
         ;; The boxed header, then each simple-fun's self word, which is
         ;; the absolute address of its first instruction.
         (loop for i from 1 below (sb-kernel:code-header-words x) do (forward i))
         (let ((old-raw (- (sb-kernel:get-lisp-obj-address x) sb-vm:other-pointer-lowtag)))
           (dotimes (k (sb-kernel:code-n-entries x))
             (let* ((fun (sb-kernel:%code-entry-point x k))
                    (fun-raw (- (sb-kernel:get-lisp-obj-address fun) sb-vm:fun-pointer-lowtag))
                    (self-index (1+ (floor (- fun-raw old-raw) +word-bytes+)))
                    (self (word-at self-index))
                    (new (forward-code-address layout self)))
               (if new
                   (set-word self-index new)
                   (fail "simple-fun self word ~X of ~S points outside its code object" self x))))))
        (sb-kernel:instance
         (set-word-index-tagged layout run offset x stats))
        (sb-kernel:funcallable-instance
         (let ((old-raw (- (sb-kernel:get-lisp-obj-address x) sb-vm:fun-pointer-lowtag)))
           (loop for i from 1 below nwords
                 do (let ((w (word-at i)))
                      (cond ((pointerp w)
                             ;; The function and layout slots and the data slots;
                             ;; a tagged pointer wherever it sits.
                             (if (>= i sb-vm:funcallable-instance-info-offset) (forward i) (forward-maybe i)))
                            ((and (>= w old-raw) (< w (+ old-raw nbytes)))
                             ;; The trampoline, an address inside the instance.
                             (set-word i (+ w (- (+ (run-base run) offset) old-raw))))
                            ((forward-code-address layout w)
                             ;; A raw entry address of a member function.
                             (set-word i (forward-code-address layout w))))))))
        (function                       ; a closure
         (let* ((w (word-at 1)) (new (forward-code-address layout w)))
           (when new (set-word 1 new)))
         (loop for i from 2 below nwords do (forward i)))
        (symbol
         ;; The builder's linkage index is cleared: the activating process
         ;; assigns its own when it resolves the name.
         (set-word sb-vm:symbol-hash-slot (logandc2 (word-at sb-vm:symbol-hash-slot) +fname-index-mask+))
         (forward sb-vm:symbol-value-slot) (forward sb-vm:symbol-fdefn-slot)
         (forward sb-vm:symbol-info-slot) (forward sb-vm:symbol-name-slot))
        (sb-kernel:fdefn
         (set-word sb-vm:symbol-hash-slot (logandc2 (word-at sb-vm:symbol-hash-slot) +fname-index-mask+))
         (forward sb-vm:fdefn-name-slot) (forward sb-vm:fdefn-fun-slot))
        (sb-ext:weak-pointer
         (forward sb-vm:weak-pointer-value-slot))
        ((and array (not simple-array)) ; an array header: data vector, displaced-from
         (forward sb-vm:array-data-slot) (forward sb-vm:array-displaced-from-slot))
        (t
         ;; Ratios, complexes, value cells and whatever else: a word is
         ;; forwarded when it names a member, and nothing is inferred otherwise.
         (unless (unboxed-p x)
           (loop for i from 1 below nwords do (forward-maybe i))))))))

(defun copy-and-relocate (layout)
  (let ((stats (make-relocation-stats)))
    (dolist (run (layout-runs layout))
      (let ((nbytes (run-nbytes run)))
        (setf (run-buffer run) (make-array nbytes :element-type '(unsigned-byte 8) :initial-element 0)
              (run-pointer-bits run) (make-array (floor nbytes +word-bytes+) :element-type 'bit :initial-element 0)
              (run-alloc-bits run) (make-array (floor nbytes +alloc-unit+) :element-type 'bit :initial-element 0))
        (sb-sys:without-gcing
          (dolist (entry (run-objects run))
            (destructuring-bind (old-raw new-raw size x) entry
              (let ((offset (- new-raw (run-base run))))
                (sb-kernel::copy-ub8-from-system-area (sb-sys:int-sap old-raw) 0 (run-buffer run) offset size)
                (setf (sbit (run-alloc-bits run) (floor offset +alloc-unit+)) 1)
                (relocate-object layout run offset size x stats)))))
        (when *verbose*
          (format *trace-output* ";;   run type ~D: ~D objects, ~D bytes filled, ~D allocation bits~%"
                  (run-type run) (length (run-objects run)) (run-fill run) (count 1 (run-alloc-bits run)))
          (dolist (e (run-objects run))
            (format *trace-output* ";;     object off=~D size=~D ~S~%"
                    (- (second e) (run-base run)) (third e) (type-of (fourth e)))))))
    stats))

;;;; Page table entries

;;; (words-used large-p scan-start-offset) per page of RUN.
(defun run-ptes (run)
  (let ((used (make-array (run-npages run) :initial-element 0)))
    (dolist (entry (run-objects run))
      (destructuring-bind (old-raw new-raw size x) entry
        (declare (ignore old-raw x))
        (let ((offset (- new-raw (run-base run))))
          (if (run-large run)
              (loop for page from (floor offset +page-bytes+)
                    for remaining = size then (- remaining +page-bytes+)
                    while (plusp remaining)
                    do (incf (aref used page) (min remaining +page-bytes+)))
              (incf (aref used (floor offset +page-bytes+)) size)))))
    (loop for page below (run-npages run)
          collect (list (floor (aref used page) +word-bytes+)
                        (if (run-large run) t nil)
                        (if (run-large run) (* page +page-bytes+) 0)))))

;;;; Exports, imports, records

;;; True of an object of the core, at the same address in every process
;;; started from it: in read-only or static space, or pseudo-static in
;;; dynamic space.
(defun core-object-p (x)
  (let ((address (sb-kernel:get-lisp-obj-address x)))
    (or (and (>= address sb-vm:read-only-space-start)
             (< address (sb-sys:sap-int sb-vm:*read-only-space-free-pointer*)))
        (and (>= address sb-vm:static-space-start)
             (< address (sb-sys:sap-int sb-vm:*static-space-free-pointer*)))
        (eql (ignore-errors (sb-kernel:generation-of x)) sb-vm:+pseudo-static-generation+))))

;;; A reference to X as the activator reads it: a member by its planned
;;; address; a symbol, string, number or character as itself; a list
;;; element by element; a package or fdefn by name; a ctype by its
;;; specifier and a source location by its parts, derived state the
;;; activator rebuilds; any other object of the core by its address, the
;;; same in a process started from this core; and anything else by its
;;; type alone, which the activator cannot resolve.
(defun object-reference (layout x)
  (let ((new (and (not (sb-int:fixnump x)) (not (characterp x))
                  (forward-address layout (sb-kernel:get-lisp-obj-address x)))))
    (cond (new (list :member new))
          ((or (symbolp x) (stringp x) (numberp x) (characterp x)) (list :global x))
          ((consp x) (list :list (mapcar (lambda (e) (object-reference layout e)) x)))
          ((packagep x) (list :global :package (package-name x)))
          ((typep x 'sb-kernel:fdefn)
           (list :global :fdefn (object-reference layout (sb-kernel:fdefn-name x))))
          ((typep x 'sb-kernel:classoid)
           (list :global :classoid (object-reference layout (sb-kernel:classoid-name x))))
          ((typep x 'sb-kernel:ctype)
           (list :global :type (object-reference layout (sb-kernel:type-specifier x))))
          ((typep x 'sb-c:definition-source-location)
           (list :global :source-location
                 (sb-c:definition-source-location-namestring x)
                 (sb-c::definition-source-location-indices x)
                 (object-reference layout (sb-c::definition-source-location-plist x))))
          ((core-object-p x)
           (list :global :address (sb-kernel:get-lisp-obj-address x) (princ-to-string (type-of x))))
          (t (list :global :unnamed (princ-to-string (type-of x)))))))

(defun exports (layout)
  (let ((result '()))
    (dolist (run (layout-runs layout))
      (dolist (entry (run-objects run))
        (destructuring-bind (old-raw new-raw size x) entry
          (declare (ignore old-raw size))
          (typecase x
            (symbol (push (list :symbol (symbol-name x) (and (symbol-package x) (package-name (symbol-package x)))
                                (+ new-raw sb-vm:other-pointer-lowtag))
                          result)
                    ;; The symbol's function when it is a simple-fun of a member
                    ;; code object: callable from the activated fragment directly.
                    (let ((f (and (fboundp x) (symbol-function x))))
                      (when (and f (sb-kernel:simple-fun-p f))
                        (let ((new (forward-address layout (sb-kernel:get-lisp-obj-address f))))
                          (when new (push (list :function (symbol-name x) (and (symbol-package x) (package-name (symbol-package x))) new) result))))))
            (sb-kernel:fdefn (push (list :fdefn (prin1-to-string (sb-kernel:fdefn-name x))
                                         (+ new-raw sb-vm:other-pointer-lowtag))
                                   result))))))
    (sort result #'string< :key #'second)))

(defun records (layout recorder)
  (mapcar (lambda (record)
            (list (sb-fiber:fragment-record-kind record)
                  (object-reference layout (sb-fiber:fragment-record-target record))
                  (mapcar (lambda (a) (object-reference layout a)) (sb-fiber:fragment-record-args record))
                  (mapcar (lambda (v) (object-reference layout v)) (sb-fiber:fragment-record-replay-values record))))
          ;; Oldest first, the order the activator replays them in.
          (sb-fiber:fragment-records recorder)))

;;;; Linkage sites

;;; The name linkage cell INDEX is for, on a target with linkage space,
;;; where alone there are sites.
(defun linkage-name (index)
  (let ((reader (find-symbol "LINKAGE-ADDR->NAME" "SB-VM")))
    (and reader (funcall reader index :index))))

;;; The references the fragment's code makes through linkage cells, as
;;; the activator reads them: per code object its planned address, then
;;; per site the byte offset of the reference into the instructions, the
;;; fixup kind that wrote it, and a reference to the name the cell is
;;; for, as OBJECT-REFERENCE makes it: a member by its planned address,
;;; a global symbol as itself, a global fdefn by name.  Returns the list
;;; and the number of sites.
(defun linkage-sites (layout recorder)
  (let ((sites (sb-fiber:fragment-linkage-sites recorder))
        (result '())
        (nsites 0))
    (dolist (run (layout-runs layout))
      (dolist (entry (run-objects run))
        (destructuring-bind (old-raw new-raw size x) entry
          (declare (ignore size))
          (when (typep x 'sb-kernel:code-component)
            (let ((code-sites (gethash (+ old-raw sb-vm:other-pointer-lowtag) sites)))
              (cond (code-sites
                     (push (cons (+ new-raw sb-vm:other-pointer-lowtag)
                                 (mapcar (lambda (site)
                                           (destructuring-bind (offset kind index) site
                                             (let ((fname (or (linkage-name index)
                                                              (fail "linkage cell ~D, referenced by ~S, is for no name" index x))))
                                               (incf nsites)
                                               (list offset kind (object-reference layout fname)))))
                                         (sort (copy-list code-sites) #'< :key #'first)))
                           result))
                    ((sb-c:unpack-code-fixup-locs (sb-vm::%code-fixups x))
                     (fail "~S references linkage cells but no site was noted for it: was it loaded with the recorder bound?" x))))))))
    (values (nreverse result) nsites)))

;;;; The file

(defun write-word (stream word)
  (dotimes (i +word-bytes+) (write-byte (ldb (byte 8 (* 8 i)) word) stream)))

(defun read-word (stream)
  (let ((word 0))
    (dotimes (i +word-bytes+ word) (setf (ldb (byte 8 (* 8 i)) word) (read-byte stream)))))

(defun bits-octets (bits)
  (let ((octets (make-array (ceiling (length bits) 8) :element-type '(unsigned-byte 8) :initial-element 0)))
    (dotimes (i (length bits) octets)
      (when (= 1 (sbit bits i)) (setf (ldb (byte 1 (mod i 8)) (aref octets (floor i 8))) 1)))))

(defun octets-bits (octets nbits)
  (let ((bits (make-array nbits :element-type 'bit :initial-element 0)))
    (dotimes (i nbits bits)
      (setf (sbit bits i) (ldb (byte 1 (mod i 8)) (aref octets (floor i 8)))))))

(defun printed-octets (form)
  (sb-ext:string-to-octets
   (with-standard-io-syntax
     (let ((*print-readably* nil) (*print-pretty* nil) (*package* (find-package "KEYWORD")))
       (prin1-to-string form)))
   :external-format :utf-8))

(defun build-id ()
  ;; The runtime's build id, when the symbol is visible to Lisp.
  (let ((address (sb-sys:find-foreign-symbol-address "build_id")))
    (if address
        (let ((sap (sb-sys:int-sap address)))
          (with-output-to-string (s)
            (loop for i from 0 below 256
                  for byte = (sb-sys:sap-ref-8 sap i)
                  until (or (zerop byte) (= byte 10))
                  do (write-char (code-char byte) s))))
        (lisp-implementation-version))))

(defun pad-to-page (octets)
  (let ((n (* (ceiling (length octets) +page-bytes+) +page-bytes+)))
    (if (= n (length octets))
        octets
        (let ((v (make-array n :element-type '(unsigned-byte 8) :initial-element 0)))
          (replace v octets)
          v))))

(defun pages-octets (layout)
  (progn
    ;; Build the section in memory: a header of words, then the runs' bytes,
    ;; each run page-aligned within the section.
    (let* ((runs (layout-runs layout))
           (header (list (length runs)))
           (data '())
           (position 0))
      (dolist (run runs)
        (let ((ptes (run-ptes run)))
          (setf header (append header
                               (list (run-type run) (run-base run) (run-nbytes run) (run-npages run)
                                     (length (run-objects run)))
                               ;; As a core's page table entry: the large-object flag in the
                               ;; low bit of the words used, the page type in the low three
                               ;; bits of the scan start offset.
                               (loop for (words large sso) in ptes
                                     append (list (logior (ash words 1) (if large 1 0))
                                                  (logior sso (run-type run))))))
          (push (bits-octets (run-alloc-bits run)) data)
          (push (run-buffer run) data)))
      ;; The header is padded to a page so that the first run's bytes start
      ;; page-aligned; each alloc bitmap is followed by its run, which is a
      ;; multiple of a page long, so the bitmaps are padded to a page too.
      (let* ((header-octets (let ((v (make-array (* +word-bytes+ (length header)) :element-type '(unsigned-byte 8))))
                              (loop for w in header for i from 0
                                    do (dotimes (k +word-bytes+)
                                         (setf (aref v (+ (* i +word-bytes+) k)) (ldb (byte 8 (* 8 k)) w))))
                              v))
             (parts (list (pad-to-page header-octets))))
        (dolist (d (nreverse data))
          (push (pad-to-page d) parts))
        (setf parts (nreverse parts))
        (setf position (reduce #'+ parts :key #'length))
        (let ((out (make-array position :element-type '(unsigned-byte 8))) (at 0))
          (dolist (p parts out)
            (replace out p :start1 at)
            (incf at (length p))))))))

(defun fnv1a-64 (octets)
  (let ((hash #xcbf29ce484222325))
    (loop for byte across octets
          do (setf hash (logand (* (logxor hash byte) #x100000001b3) #xFFFFFFFFFFFFFFFF)))
    hash))

(defun write-fragment (recorder pathname &key base (name "fragment"))
  "Write the fragment RECORDER was recording, sealed, to PATHNAME, laid out
from the planned address BASE. Return a plist of what was written."
  (let* ((heap-id (sb-vm::fragment-recorder-heap-id recorder))
         (members (collect-members recorder))
         (base (or base (fail "a planned base address is required")))
         (layout (progn (stage "layout") (lay-out members base heap-id)))
         (stats (progn (stage "copy and relocate") (copy-and-relocate layout))))
    (when (stats-dangling stats)
      (fail "~D references from members to objects of the build heap that are not members, such as ~{~S~^, ~}"
            (length (stats-dangling stats))
            (mapcar (lambda (d) (list (third d) :-> (fourth d))) (subseq (stats-dangling stats) 0 (min 8 (length (stats-dangling stats)))))))
    (stage "pages")
    (let* ((pages (pages-octets layout))
           (pointer-map (let ((parts (mapcar (lambda (run) (bits-octets (run-pointer-bits run))) (layout-runs layout))))
                          (let ((out (make-array (reduce #'+ parts :key #'length) :element-type '(unsigned-byte 8))) (at 0))
                            (dolist (p parts out) (replace out p :start1 at) (incf at (length p))))))
           (exports (progn (stage "exports") (printed-octets (exports layout))))
           (.s-imports. (stage "imports"))
           (imports (printed-octets (mapcar (lambda (i)
                                              (when (eq *verbose* :objects) (format *trace-output* ";;   import ~X (~S) from ~S at ~X~%" (second i) (type-of (sb-kernel:%make-lisp-obj (second i))) (third i) (first i)))
                                              (list (first i) (object-reference layout (sb-kernel:%make-lisp-obj (second i)))))
                                            (reverse (stats-imports stats)))))
           (records (progn (stage "records") (printed-octets (records layout recorder))))
           (.s-linkage. (stage "linkage"))
           (linkage (multiple-value-bind (sites nsites) (linkage-sites layout recorder)
                      (cons nsites (printed-octets sites))))
           (provenance (printed-octets (list :name name :members (layout-members layout)
                                             :member-bytes (layout-member-bytes layout)
                                             :runs (mapcar (lambda (r) (list (run-type r) (run-base r) (run-nbytes r) (run-npages r)))
                                                           (layout-runs layout))
                                             :pointers (stats-pointers stats)
                                             :imports (length (stats-imports stats))
                                             :linkage-sites (car linkage)
                                             :written (get-universal-time))))
           (sections (list (cons +section-pages+ pages) (cons +section-pointer-map+ pointer-map)
                           (cons +section-exports+ exports) (cons +section-imports+ imports)
                           (cons +section-records+ records) (cons +section-linkage+ (cdr linkage))
                           (cons +section-provenance+ provenance)))
           (.s-identity. (stage "identity"))
           (identity (printed-octets (list :build-id (build-id)
                                           :lisp (lisp-implementation-version)
                                           :page-bytes +page-bytes+ :word-bytes +word-bytes+
                                           :base base
                                           :hash :fnv1a-64
                                           :id (fnv1a-64 (let ((all (make-array (reduce #'+ sections :key (lambda (s) (length (cdr s))))
                                                                                  :element-type '(unsigned-byte 8))) (at 0))
                                                           (dolist (s sections all) (replace all (cdr s) :start1 at) (incf at (length (cdr s))))))))))
      (declare (ignorable .s-imports. .s-linkage. .s-identity.))
      (push (cons +section-identity+ identity) sections)
      (stage "file")
      ;; Header: magic, version, section count, then (id offset length) per section.
      (with-open-file (stream pathname :direction :output :element-type '(unsigned-byte 8) :if-exists :supersede)
        (let* ((header-words (+ 3 (* 3 (length sections))))
               (header-bytes (* (ceiling (* header-words +word-bytes+) +page-bytes+) +page-bytes+))
               (offset header-bytes))
          (write-word stream +magic+) (write-word stream +version+) (write-word stream (length sections))
          (dolist (s sections)
            (write-word stream (car s)) (write-word stream offset) (write-word stream (length (cdr s)))
            (incf offset (* (ceiling (length (cdr s)) +page-bytes+) +page-bytes+)))
          (dotimes (i (- header-bytes (* header-words +word-bytes+))) (write-byte 0 stream))
          (dolist (s sections)
            (write-sequence (cdr s) stream)
            (dotimes (i (- (* (ceiling (length (cdr s)) +page-bytes+) +page-bytes+) (length (cdr s)))) (write-byte 0 stream)))))
      (list :members (layout-members layout) :member-bytes (layout-member-bytes layout)
            :runs (length (layout-runs layout))
            :bytes (reduce #'+ (layout-runs layout) :key #'run-nbytes)
            :pointers (stats-pointers stats) :imports (length (stats-imports stats))
            :linkage-sites (car linkage)))))

;;;; Reading back

(defun read-sections (stream)
  (unless (= (read-word stream) +magic+) (fail "not a fragment file"))
  (let ((version (read-word stream)))
    (unless (= version +version+) (fail "fragment file version ~D, expected ~D" version +version+)))
  (let ((n (read-word stream)) (table '()))
    (dotimes (i n)
      (let* ((id (read-word stream)) (offset (read-word stream)) (length (read-word stream)))
        (push (list id offset length) table)))
    (mapcar (lambda (entry)
              (destructuring-bind (id offset length) entry
                (file-position stream offset)
                (let ((octets (make-array length :element-type '(unsigned-byte 8))))
                  (read-sequence octets stream)
                  (cons id octets))))
            (nreverse table))))

(defun section (sections id)
  (or (cdr (assoc id sections)) (fail "section ~D missing" id)))

(defun read-printed (octets)
  (with-standard-io-syntax
    (let ((*package* (find-package "KEYWORD")) (*read-eval* nil))
      (read-from-string (sb-ext:octets-to-string octets :external-format :utf-8)))))

(defun octets-word (octets index)
  (let ((word 0))
    (dotimes (k +word-bytes+ word)
      (setf (ldb (byte 8 (* 8 k)) word) (aref octets (+ (* index +word-bytes+) k))))))

;;; Parse the pages section into (type base nbytes npages nobjects ptes alloc-bits bytes) per run.
(defun parse-runs (pages)
  (let* ((nruns (octets-word pages 0)) (index 1) (runs '()) (position 0))
    (dotimes (i nruns)
      (let* ((type (octets-word pages index)) (base (octets-word pages (+ index 1)))
             (nbytes (octets-word pages (+ index 2))) (npages (octets-word pages (+ index 3)))
             (nobjects (octets-word pages (+ index 4))))
        (incf index 5)
        (let ((ptes (loop repeat npages
                          collect (prog1 (list (ash (octets-word pages index) -1)
                                               (logbitp 0 (octets-word pages index))
                                               (logandc2 (octets-word pages (1+ index)) 7)
                                               (logand (octets-word pages (1+ index)) 7))
                                    (incf index 2)))))
          (push (list type base nbytes npages nobjects ptes position) runs))))
    (setf position (* (ceiling (* index +word-bytes+) +page-bytes+) +page-bytes+))
    ;; Now the data: for each run an alloc bitmap padded to a page, then the bytes.
    (mapcar (lambda (run)
              (destructuring-bind (type base nbytes npages nobjects ptes pos) run
                (declare (ignore pos))
                (let* ((nalloc (floor nbytes +alloc-unit+))
                       (alloc (octets-bits (subseq pages position (+ position (ceiling nalloc 8))) nalloc)))
                  (incf position (* (ceiling (ceiling nalloc 8) +page-bytes+) +page-bytes+))
                  (let ((bytes (subseq pages position (+ position nbytes))))
                    (incf position nbytes)
                    (list type base nbytes npages nobjects ptes alloc bytes)))))
            (nreverse runs))))

(defun fragment-file-stats (pathname)
  (with-open-file (stream pathname :element-type '(unsigned-byte 8))
    (let ((sections (read-sections stream)))
      (list :identity (read-printed (section sections +section-identity+))
            :provenance (read-printed (section sections +section-provenance+))
            :exports (read-printed (section sections +section-exports+))))))

(defun verify-fragment-file (pathname)
  "Read the fragment file at PATHNAME back and check its relocation
invariants: every pointer-map bit marks a word holding a pointer to an
object start inside the fragment's runs, no other word of a data run
points into them, the allocation bitmap counts as many objects as the
provenance says, every export names an object start, and every linkage
site lies in a code object of the fragment and names an object start in
it or an address outside it. Return a plist of counts; signal
FRAGMENT-FILE-ERROR on the first violation."
  (with-open-file (stream pathname :element-type '(unsigned-byte 8))
    (let* ((sections (read-sections stream))
           (identity (read-printed (section sections +section-identity+)))
           (provenance (read-printed (section sections +section-provenance+)))
           (runs (parse-runs (section sections +section-pages+)))
           (pointer-map (section sections +section-pointer-map+))
           (exports (read-printed (section sections +section-exports+)))
           (linkage (read-printed (section sections +section-linkage+)))
           (total-objects 0) (npointers 0) (nwords 0) (map-offset 0) (nsites 0))
      (unless (= (getf identity :page-bytes) +page-bytes+) (fail "page size ~D" (getf identity :page-bytes)))
      (flet ((object-start-p (address)
               (dolist (run runs nil)
                 (destructuring-bind (type base nbytes npages nobjects ptes alloc bytes) run
                   (declare (ignore type npages nobjects ptes bytes))
                   (when (and (>= address base) (< address (+ base nbytes)))
                     (return (= 1 (sbit alloc (floor (- address base) +alloc-unit+)))))))))
        (dolist (run runs)
          (destructuring-bind (type base nbytes npages nobjects ptes alloc bytes) run
            (declare (ignore type npages ptes))
            (incf total-objects (count 1 alloc))
            (unless (= (count 1 alloc) nobjects)
              (fail "run at ~X: ~D allocation bits for ~D objects" base (count 1 alloc) nobjects))
            (let* ((run-words (floor nbytes +word-bytes+))
                   (bits (octets-bits (subseq pointer-map map-offset (+ map-offset (ceiling run-words 8))) run-words)))
              (incf map-offset (ceiling run-words 8))
              (dotimes (i run-words)
                (let ((word (octets-word bytes i)))
                  (cond ((= 1 (sbit bits i))
                         (incf npointers)
                         ;; A function pointer may point inside a code object, at a
                         ;; simple-fun; anything else points at an object start.
                         (unless (and (pointerp word)
                                      (if (= (logand word sb-vm:lowtag-mask) sb-vm:fun-pointer-lowtag)
                                          (some (lambda (r) (and (>= word (second r)) (< word (+ (second r) (third r))))) runs)
                                          (object-start-p (- word (logand word sb-vm:lowtag-mask)))))
                           (fail "pointer-map word ~X of run ~X does not point at an object start in the fragment"
                                 (+ base (* i +word-bytes+)) base)))
                        ((and (pointerp word)
                              (some (lambda (r) (and (>= word (second r)) (< word (+ (second r) (third r))))) runs))
                         (fail "word ~X of run ~X points into the fragment without a pointer-map bit"
                               (+ base (* i +word-bytes+)) base))))
                (incf nwords)))))
        (unless (= total-objects (getf provenance :members))
          (fail "~D allocation bits, ~D members written" total-objects (getf provenance :members)))
        (unless (= npointers (getf provenance :pointers))
          (fail "~D pointer-map bits, ~D pointers written" npointers (getf provenance :pointers)))
        (dolist (e exports)
          (let ((address (car (last e))))
            ;; A function export names a simple-fun inside a code object.
            (unless (if (eq (first e) :function)
                        (some (lambda (r) (and (>= address (second r)) (< address (+ (second r) (third r))))) runs)
                        (object-start-p (- address (logand address sb-vm:lowtag-mask))))
              (fail "export ~S names no object in the fragment" e))))
        (dolist (entry linkage)
          (destructuring-bind (code-address &rest sites) entry
            (let ((run (find-if (lambda (r) (and (>= code-address (second r)) (< code-address (+ (second r) (third r))))) runs)))
              (unless (and run (= (first run) +type-code+)
                           (object-start-p (- code-address (logand code-address sb-vm:lowtag-mask))))
                (fail "linkage sites at ~X name no code object in the fragment" code-address))
              (dolist (site sites)
                (destructuring-bind (offset kind name) site
                  (declare (ignore kind))
                  (unless (< (+ code-address offset) (+ (second run) (third run)))
                    (fail "linkage site ~D of the code object at ~X lies outside its run" offset code-address))
                  (when (and (eq (first name) :member)
                             (not (object-start-p (- (second name) (logand (second name) sb-vm:lowtag-mask)))))
                    (fail "linkage site ~D of the code object at ~X names no object in the fragment" offset code-address))
                  (incf nsites))))))
        (unless (= nsites (getf provenance :linkage-sites))
          (fail "~D linkage sites, ~D written" nsites (getf provenance :linkage-sites))))
      (list :members total-objects :pointers npointers :words nwords :runs (length runs) :exports (length exports)
            :linkage-sites nsites))))
