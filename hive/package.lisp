
(in-package common-lisp-user)

(defpackage :loic-hive
  (:use :common-lisp
        :unix-options
        :trivial-irc
        :split-sequence
        :loic)
  (:export :main))

; vim: set lisp softtabstop=2 expandtab filetype=lisp:
