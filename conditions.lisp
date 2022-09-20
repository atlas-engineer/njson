;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3 Clause

(in-package #:njson)

(define-condition decode-from-stream-not-implemented (error) ()
  (:report "DECODE-FROM-STREAM is not specialized.
You need to specialize it to use NJSON. Example:

(defmethod njson:decode-from-stream ((stream stream))
  (some-json-parsing-library:decode-json-from-stream stream))

Alternatively, load a system with this method already defined, like :njson/cl-json."))

(define-condition encode-to-stream-not-implemented (error) ()
  (:report "ENCODE-TO-STREAM is not specialized.
You need to specialize it to use NJSON. Example:

(defmethod njson:encode-to-stream ((object t) (stream stream))
  (some-json-parsing-library:encode-json-to-stream object stream))

Alternatively, load a system with this method already defined, like :njson/cl-json."))

(define-condition invalid-key (error)
  ((object :initarg :object
           :accessor object)
   (key :initarg :key
        :accessor key)))

(defun type-num (object)
  (typecase object
    (hash-table 0)
    (sequence 1)
    (t 2)))

(define-condition jget-invalid-key (invalid-key) ()
  (:report (lambda (condition stream)
             (format stream "Cannot index JSON ~[object~;array~;value~] ~a with key ~s.
~[Use string keys instead.~;~
Use integer indices instead.~;~
Are you sure you're indexing the right thing?~]"
                     (type-num (object condition)) (encode (object condition))
                     (key condition) (type-num (object condition))))))

(define-condition setf-jget-invalid-key (invalid-key)
  ((value :initarg :value
          :accessor value))
  (:report (lambda (condition stream)
             (format stream "Cannot set value in JSON ~[object~;array~;value~] ~a to ~s with key ~s.
~[Use string keys instead.~;~
Use integer indices instead.~;~
Are you sure you're modifying the right thing?~]"
                     (type-num (object condition)) (encode (object condition))
                     (value condition) (key condition) (type-num (object condition))))))

(define-condition jrem-invalid-key (invalid-key) ()
  (:report (lambda (condition stream)
             (format stream "Cannot remove a value from JSON ~[object~;array~;value~] ~a using key ~s.
~[Use string keys instead.~;~
Use integer indices instead.~;~
Are you sure you're removing value from the right thing?~]"
                     (type-num (object condition)) (encode (object condition))
                     (key condition) (type-num (object condition))))))
