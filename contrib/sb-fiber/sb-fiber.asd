;;;; -*-  Lisp -*-

(error "Can't build contribs with ASDF")

(defsystem "sb-fiber"
  :components ((:file "package")
               (:file "fiber-vops-x86-64"
                :if-feature :x86-64
                :depends-on ("package"))
               (:file "fiber-vops-arm64"
                :if-feature :arm64
                :depends-on ("package"))
               (:file "fiber" :depends-on ("package"))))
