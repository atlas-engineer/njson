;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3 Clause

(in-package #:njson)

(define-condition decode-json-from-stream-not-implemented (error) ()
  (:report "DECODE-JSON-FROM-STREAM is not specialized.
You need to specialize it to use NJSON. Example:

(defmethod njson:decode-json-from-stream ((stream stream))
  (some-json-parsing-library:decode-json-from-stream stream))

Alternatively, load a system with this method already defined, like :njson/cl-json."))

(define-condition encode-json-to-stream-not-implemented (error) ()
  (:report "ENCODE-JSON-TO-STREAM is not specialized.
You need to specialize it to use NJSON. Example:

(defmethod njson:encode-json-to-stream ((object t) (stream stream))
  (some-json-parsing-library:encode-json-to-stream object stream))

Alternatively, load a system with this method already defined, like :njson/cl-json."))

(define-condition jget-invalid-key (error)
  ((object :initarg :object
           :accessor object)
   (key :initarg :key
        :accessor key))
  (:report (lambda (condition stream)
             (let ((type-num (typecase (object condition)
                               (hash-table 0)
                               (sequence 1)
                               (t 2))))
               (format stream "Invalid key used for indexing JSON ~[object~;array~;value~]:
cannot index ~a with key ~s.
~[Use string keys instead.~;~
Use integer indices instead.~;~
Are you sure you're indexing the right thing?~]"
                       type-num (encode (object condition))
                       (key condition) type-num)))))
