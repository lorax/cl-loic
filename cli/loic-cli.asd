(defsystem loic-cli
  :serial t
  :components ((:file "package")
               (:file "cli"))
  :depends-on ("loic"
               "unix-options"))

; vim: set lisp softtabstop=2 expandtab filetype=lisp:
