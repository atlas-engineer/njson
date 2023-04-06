;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3 Clause

(in-package #:njson)

(define-condition decode-from-stream-not-implemented (error) ()
  (:documentation "Incomplete decoding implementation error.")
  (:report "DECODE-FROM-STREAM is not specialized.
You need to specialize it to use NJSON. Example:

(defmethod njson:decode-from-stream ((stream stream))
  (some-json-parsing-library:decode-json-from-stream stream))

Alternatively, load a system with this method already defined, like :njson/cl-json."))

(define-condition encode-to-stream-not-implemented (error) ()
  (:documentation "Incomplete encoding implementation error.")
  (:report "ENCODE-TO-STREAM is not specialized.
You need to specialize it to use NJSON. Example:

(defmethod njson:encode-to-stream ((object t) (stream stream))
  (some-json-parsing-library:encode-json-to-stream object stream))

Alternatively, load a system with this method already defined, like :njson/cl-json."))

(define-condition invalid-key (error)
  ((object :initarg :object
           :accessor object)
   (key :initarg :key
        :accessor key))
  (:documentation "The condition thrown on using wrong key with object/array.")
  (:report (lambda (condition stream)
             (format stream "Cannot index JSON ~[object~;array~;value~] ~a with key ~s.
~[Use string keys instead.~;~
Use integer indices instead.~;~
Are you sure you're indexing the right thing?~]"
                     (type-num (object condition)) (encode (object condition))
                     (key condition) (type-num (object condition))))))

(defun type-num (object)
  (typecase object
            (hash-table 0)
            (sequence 1)
            (t 2)))

(define-condition non-indexable (error)
  ((value :initarg :value
          :accessor value))
  (:documentation "The condition thrown on trying to index non-object/array.")
  (:report (lambda (condition stream)
             (format stream "Non-indexable ~a."
                     (encode (value condition))))))

(define-condition deprecated (warning)
  ((deprecated :initarg :deprecated
               :accessor deprecated)
   (replacement :initarg :replacement
                :accessor replacement))
  (:documentation "Deprecation warning.")
  (:report (lambda (condition stream)
             (format stream "~a is deprecated. It will be removed in the next major release.
Use ~a instead." (deprecated condition) (replacement condition)))))
