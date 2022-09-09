;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3 Clause

(in-package #:njson)

(defvar *json-object-accumulator* (make-hash-table :test 'equal)
  "Our own object accumulator to override the default `cl-json:decode-json' object->alist behavior.
Objects are transformed to the hash-tables instead.")

(defvar *json-last-object-key* nil
  "The last key used in `*json-object-accumulator*'.")

(defun json-object-init ()
  (setf *json-object-accumulator* (make-hash-table :test 'equal)))

(defun json-object-add-key (key)
  (setf (gethash key *json-object-accumulator*) nil
        *json-last-object-key* key))

(defun json-object-add-value (value)
  (setf (gethash *json-last-object-key* *json-object-accumulator*) value))

(defun json-object-get ()
  *json-object-accumulator*)

(defmacro with-cl-json-settings (&body body)
  `(let ((json::+json-lisp-symbol-tokens+
           '(("true" . t)
             ("false" . nil)
             ("null" . :null)
             ("undefined" . :undefined)))
         (json:*object-scope-variables* '(json:*internal-decoder* *json-object-accumulator* *json-last-object-key*))
         (json:*beginning-of-object-handler* #'json-object-init)
         (json:*object-key-handler* #'json-object-add-key)
         (json:*object-value-handler* #'json-object-add-value)
         (json:*end-of-object-handler* #'json-object-get))
     ,@body))

(defmethod decode-json-from-stream ((stream stream))
  (with-cl-json-settings
      (cl-json:decode-json stream)))

(defmethod decode-json-from-string ((string string))
  (with-cl-json-settings
      (cl-json:decode-json-from-string string)))

(defmethod decode-json-from-file ((file pathname))
  (with-cl-json-settings
      (cl-json:decode-json-from-source file)))

(defmethod encode-json-to-stream ((object t) (stream stream))
  (with-cl-json-settings
      (cl-json:encode-json object stream)))

(defmethod encode-json-to-string ((object t))
  (with-cl-json-settings
      (cl-json:encode-json-to-string object)))

;; NOTE: `encode-json-to-file' is not specialized, because CL-JSON
;; doesn't have a specialized function for that. We rather rely on the
;; default `encode-json-to-file' method of NJSON that opens a stream
;; from file and uses `encode-json-to-stream' with this stream.
