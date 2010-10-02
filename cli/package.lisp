
(in-package common-lisp-user)

(defpackage :loic-cli
  (:use :common-lisp
        :unix-options
        :loic)
  (:export :main))

; vim: set lisp softtabstop=2 expandtab filetype=lisp:
