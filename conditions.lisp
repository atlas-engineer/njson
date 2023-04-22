;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3 Clause

(in-package #:njson)

(defun read-new-value ()
  (format *query-io* "Input the new value (evaluated)~%")
  (list (eval (uiop:safe-read-from-string (read-line *query-io* nil nil)))))

(defun read-new-key ()
  (format *query-io* "Input the new key (literal number or string)~%")
  (list (uiop:safe-read-from-string (read-line *query-io* nil nil))))

(defun read-new-pointer ()
  (format *query-io* "Input the new JSON Pointer~%")
  (list (pathname (read-line *query-io* nil nil))))

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

(defun json-short-print (object)
  "Produce a string with a short object representation for debugging.

May actually produce long results for objects/arrays with many
members. But it's implied that these are rare cases and don't need
special treatment."
  (with-output-to-string (*standard-output*)
    (flet ((nested-print (value)
             (princ (typecase value
                      (hash-table "{}")
                      ((and array (not string)) "[]")
                      (t (json-short-print value))))))
      (typecase object
        (string (prin1 object))
        (hash-table
         (princ "{")
         (maphash
          (lambda (key value)
            (princ key) (princ ": ")
            (nested-print value) (princ ", "))
          object)
         (princ "}"))
        (array
         (princ "[")
         (map nil (lambda (value)
                    (nested-print value)
                    (princ ", "))
              object)
         (princ "]"))
        (t (princ (encode object)))))))

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
                     (type-num (object condition)) (json-short-print (object condition))
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
                     (json-short-print (value condition))))))

(define-condition invalid-pointer (error)
  ((pointer :initarg :pointer
            :accessor pointer))
  (:documentation "Condition thrown when trying to index an object with invalid pointer.")
  (:report (lambda (condition stream)
             (format stream "Pointer ~S is invalid."
                     (pointer condition)))))

(define-condition value-mismatch (error)
  ((expected :initarg :expected
             :accessor expected)
   (actual :initarg :actual
           :accessor actual)
   (object :initarg :object
           :accessor object))
  (:documentation "Condition thrown when getting a value not matching `jbind'/`jmatch' specification.")
  (:report (lambda (condition stream)
             (format stream "Expected ~a in object ~a and got ~a."
                     (json-short-print (expected condition))
                     (json-short-print (object condition))
                     (json-short-print (actual condition))))))

(define-condition deprecated (warning)
  ((deprecated :initarg :deprecated
               :accessor deprecated)
   (replacement :initarg :replacement
                :accessor replacement))
  (:documentation "Deprecation warning.")
  (:report (lambda (condition stream)
             (format stream "~a is deprecated. It will be removed in the next major release.
Use ~a instead." (deprecated condition) (replacement condition)))))
