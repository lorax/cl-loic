(defsystem loic
  :serial t
  :components ((:file "package")
               (:file "trivial-utf-8")
               (:file "flooder"))
  :depends-on ("sb-bsd-sockets"))

; vim: set lisp softtabstop=2 expandtab filetype=lisp:
