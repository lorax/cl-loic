;;; Minimal utf-8 decoding and encoding library.
;;;
;;; See http://common-lisp.net/project/trivial-utf-8/

;;; Altered for loic's own purposes by a1024

(in-package :loic)

(defmacro as-utf-8-bytes (char writer)
  "Given a character, calls the writer function for every byte in the
 encoded form of that character."
  (let ((char-code (gensym)))
    `(let ((,char-code (char-code ,char)))
       (declare (type fixnum ,char-code))
       (cond ((< ,char-code 128)
              (,writer ,char-code))
             ((< ,char-code 2048)
              (,writer (logior #b11000000 (ldb (byte 5 6) ,char-code)))
              (,writer (logior #b10000000 (ldb (byte 6 0) ,char-code))))
             ((< ,char-code 65536)
              (,writer (logior #b11100000 (ldb (byte 4 12) ,char-code)))
              (,writer (logior #b10000000 (ldb (byte 6 6) ,char-code)))
              (,writer (logior #b10000000 (ldb (byte 6 0) ,char-code))))
             (t
              (,writer (logior #b11110000 (ldb (byte 3 18) ,char-code)))
              (,writer (logior #b10000000 (ldb (byte 6 12) ,char-code)))
              (,writer (logior #b10000000 (ldb (byte 6 6) ,char-code)))
              (,writer (logior #b10000000 (ldb (byte 6 0) ,char-code))))))))

(defun utf-8-byte-length (string)
  "Calculate the amount of bytes needed to encode a string."
  (declare (type string string))
  (let ((length (length string))
        (string (coerce string 'simple-string)))
    (loop :for char :across string
       :do (let ((code (char-code char)))
             (when (> code 127)
               (incf length
                     (cond ((< code 2048) 1)
                           ((< code 65536) 2)
                           (t 3))))))
    length))


(defun string-to-utf-8-bytes (string &key null-terminate)
  "Convert a string into an array of unsigned bytes containing its
utf-8 representation."
  (declare (type string string))
  (let ((buffer (make-array (+ (the fixnum (utf-8-byte-length string))
                               (if null-terminate 1 0))
                            :element-type '(unsigned-byte 8)))
        (position 0)
        (string (coerce string 'simple-string)))
    (declare (type (array (unsigned-byte 8)) buffer)
             (type fixnum position))
    (loop :for char :across string
       :do (as-utf-8-bytes char (lambda (byte)
                                  (setf (aref buffer position) byte)
                                  (incf position))))
    (when null-terminate
      (setf (elt buffer (1- (length buffer))) 0))
    buffer))

;;; Copyright (c) Marijn Haverbeke
;;;
;;; This software is provided 'as-is', without any express or implied
;;; warranty. In no event will the authors be held liable for any
;;; damages arising from the use of this software.
;;;
;;; Permission is granted to anyone to use this software for any
;;; purpose, including commercial applications, and to alter it and
;;; redistribute it freely, subject to the following restrictions:
;;;
;;; 1. The origin of this software must not be misrepresented; you must
;;;    not claim that you wrote the original software. If you use this
;;;    software in a product, an acknowledgment in the product
;;;    documentation would be appreciated but is not required.
;;;
;;; 2. Altered source versions must be plainly marked as such, and must
;;;    not be misrepresented as being the original software.
;;;
;;; 3. This notice may not be removed or altered from any source
;;;    distribution.

; vim: set lisp softtabstop=2 expandtab filetype=lisp:
