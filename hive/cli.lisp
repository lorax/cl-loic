
(in-package loic-hive)

(defparameter *options* '((port (port "80" "IRC server's port"))
                          (logfile (logfile ".log" "Where to put logs"))
                          (channel (channel "#loic" "Channel to wait for instructions on"))))

(defparameter *desc* "Usage: loic-hive [OPTIONS] <server> ~%Summary: ~%~@{~A~%~}~%")

(defvar *hivemind* nil)

(defun princln (s)
  (fresh-line) (princ s))

(defun main (&optional (params (cli-options)))
  (setf *random-state*  (make-random-state t))
  (ignore-errors
  (eval `(with-cli-options (',params ,*desc*) ,*options*
                (if (null free)
                  (print-usage-summary *desc* (mapcar #'cadr *options*))
                  (let ((hivemind (make-instance 'hivemind
                                                 :username "Loic.hivemind"
                                                 :nickname (format nil "LOIC_~A" (random-string 2 2))
                                                 :logger #'princln
                                                 :log-pathname ".log"
                                                 :server (car free))))
                    (setf *hivemind* hivemind)
                    (when logfile
                      (setf (slot-value hivemind 'trivial-irc::log-pathname) logfile))
                    (when port
                      (setf (slot-value hivemind 'port) port))
                    (when channel
                      (setf (slot-value hivemind 'channel) channel))
                    (start hivemind)
                    (read-char)
                    (stop hivemind)))))
  )
  (fresh-line))

; vim: set lisp softtabstop=2 expandtab filetype=lisp:
