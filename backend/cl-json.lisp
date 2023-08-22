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

(defun json::read-json-token (stream)
  "Read a JSON token (literal name, number or punctuation char) from
the given STREAM, and return 2 values: the token category (a symbol)
and the token itself, as a string or character."
  (let ((c (peek-char t stream)))
    (case c
      ((#\{ #\[ #\] #\} #\" #\: #\,)
       (values :punct (read-char stream)))
      ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\-)
       (json::read-json-number-token stream))
      ;; Modified to ignore comments.
      ((#\/)
       (read-line stream)
       (json::read-json-token stream))
      (t (if (alpha-char-p c)
             (json::read-json-name-token stream)
             (json:json-syntax-error stream "Invalid char on JSON input: `~C'"
                                c))))))

(defun peek-json-token (stream)
  "Return 2 values: the category and the first character of the next
token available in the given STREAM.  Unlike READ-JSON-TOKEN, this
function can not discriminate between integers and reals (hence, it
returns a single :NUMBER category), and cannot check whether the next
available symbol is a valid boolean or not (hence, the category for
such tokens is :SYMBOL)."
  (let ((c (peek-char t stream)))
    (values
     (case c
       ((#\{ #\[ #\] #\} #\" #\: #\,) :punct)
       ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\-) :number)
       ;; Modified to ignore comments.
       ((#\/)
        (read-line stream)
        (json::peek-json-token stream))
       (t (if (alpha-char-p c)
              :symbol
              (json::json-syntax-error stream "Invalid char on JSON input: `~C'"
                                 c))))
     c)))

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
         (json:*end-of-object-handler* #'json-object-get)
         (json:*json-array-type* 'vector)
         (json:*use-strict-json-rules* nil))
     ,@body))

(defmethod decode-from-stream ((stream stream))
  (with-cl-json-settings
    (cl-json:decode-json stream)))

(defmethod decode-from-string ((string string))
  (with-cl-json-settings
    (cl-json:decode-json-from-string string)))

(defmethod decode-from-file ((file pathname))
  (with-cl-json-settings
    (cl-json:decode-json-from-source file)))

(defmethod encode-to-stream ((object t) (stream stream))
  (with-cl-json-settings
    (cl-json:encode-json object stream)))

(defmethod encode-to-string ((object t))
  (with-cl-json-settings
    (cl-json:encode-json-to-string object)))

;; NOTE: `encode-to-file' is not specialized, because CL-JSON doesn't
;; have a specialized function for that. We rather rely on the default
;; `encode-to-file' method of NJSON that opens a stream from file and
;; uses `encode-to-stream' with this stream.
