;;;; -*-  Lisp -*-

(error "Can't build contribs with ASDF")

(defsystem "sb-fiber"
  :components ((:file "package")
               (:file "x86-64-vops"
                :if-feature (:and :sb-fiber :x86-64)
                :depends-on ("package"))
               (:file "fiber" :depends-on ("package"))))
