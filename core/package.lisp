
(in-package common-lisp-user)

(defpackage :loic
  (:use :common-lisp
        :usocket)
  (:export tcp-flooder udp-flooder http-flooder
           start-flooder stop-flooder
           target-ip target-port delay connections
           data random-data
           subsite add-random-subsite timeout
           random-string make-thread))

; vim: set lisp softtabstop=2 expandtab filetype=lisp:
