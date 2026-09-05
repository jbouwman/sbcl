;;;; -*-  Lisp -*-

(error "Can't build contribs with ASDF")

(defsystem "sb-process"
  :depends-on ("sb-fiber")
  :components ((:file "package")
               (:file "scheduler"  :depends-on ("package"))
               (:file "process"    :depends-on ("package" "scheduler"))
               (:file "server"     :depends-on ("package" "process"))
               (:file "supervisor" :depends-on ("package" "process" "server"))))
