
(defsystem loic-hive
  :serial t
  :components ((:file "package")
               (:file "hive")
               (:file "cli"))
  :depends-on ("loic"
               "split-sequence"
               "unix-options"
               "trivial-irc"))

; vim: set lisp softtabstop=2 expandtab filetype=lisp:
