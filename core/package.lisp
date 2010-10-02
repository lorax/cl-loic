
(in-package common-lisp-user)

(defpackage :loic
  (:use :common-lisp
        :sb-bsd-sockets)
  (:export tcp-flooder udp-flooder http-flooder
           start-flooder stop-flooder
           target-ip target-port delay connections
           data
           subsite add-random-subsite timeout))

; vim: set lisp softtabstop=2 expandtab filetype=lisp:
