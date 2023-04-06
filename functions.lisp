;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3 Clause

(in-package #:njson)

;; FIXME: CL pathname parsing on SBCL+Linux parses #p"/i\\j" as
;; #p"/ij", which breaks one of JSON Pointer RFC example. It's bad,
;; but avoidable with e.g. `make-pathname'.
(defun parse-pointer-pathname (pointer-pathname)
  "Parse POINTER-PATHNAME per JSON Pointer rules (https://www.rfc-editor.org/rfc/rfc6901).
Only supports JSON string representation, not the URL one."
  (flet ((resolve-tildes (string)
           (uiop:frob-substrings
            string '("~1" "~0")
            (lambda (match frob)
              (funcall frob (case (elt match 1)
                              (#\1 "/")
                              (#\0 "~"))))))
         (read-until (char stream)
           "Read from STREAM until encountering CHAR.
CHAR is left unread on STREAM after returning."
           (coerce (loop for peeked = (peek-char nil stream nil nil)
                         until (or (eql char peeked)
                                   (null peeked))
                         collect (read-char stream nil nil))
                   'string))
         (parse-if-number (string)
           (if (and (not (uiop:emptyp string))
                    (every #'digit-char-p string))
               (parse-integer string)
               string)))
    (let* ((name (namestring pointer-pathname)))
      (with-input-from-string (s name)
        (loop for char = (read-char s nil nil)
              while char
              when (eq #\/ char)
                collect (parse-if-number (resolve-tildes (read-until #\/ s)))
              else do (error 'invalid-pointer :pointer pointer-pathname))))))

(defgeneric jget (key-or-index object)
  (:method ((keys sequence) (object t))
    (if (= 1 (length keys))
        (jget (elt keys 0) object)
        (jget (subseq keys 1)
              (jget (elt keys 0) object))))
  (:method ((index integer) (object array))
    (values (aref object index)
            (<= 0 index (1- (length object)))))
  (:method ((key string) (object hash-table))
    (gethash key object))
  (:method ((pointer pathname) object)
    (if (equal #p"" pointer)
        (values object t)
        (jget (parse-pointer-pathname pointer) object)))
  (:method (key (object null))
    (declare (ignore key))
    (error 'non-indexable :value object))
  (:method (key (object string))
    (declare (ignore key))
    (error 'non-indexable :value object))
  (:method ((index string) (object array))
    (error 'invalid-key :key index :object object))
  (:method ((key integer) (object hash-table))
    (error 'invalid-key :key key :object object))
  (:documentation "Get the value at KEY-OR-INDEX in OBJECT.

KEY-OR-INDEX can be
- an integer (for array indexing),
- a string (for object keying),
- a pathname (with JSON Pointer syntax),
- or a sequence of integers and strings (to index the nested
  structures).

Throws `invalid-key' if using the wrong index type.
Throws `non-indexable' when trying to index something other than JSON
arrays or objects.
Throws `invalid-pointer' when using JSON Pointer with invalid syntax
as key.

For example, to get the data from a structure like
{\"data\": [1, 2, {\"three\": 3}]}
you can use
(jget #(\"data\" 2 \"three\") data)
;; => 3

OBJECT can be JSON array or object, which in Lisp translates to
`array' or `hash-table'."))

(defgeneric (setf jget) (value key-or-index object)
  (:method (value (keys sequence) (object t))
    (setf (jget (elt keys (1- (length keys)))
                (jget (subseq keys 0 (1- (length keys))) object))
          value))
  (:method (value (index integer) (object array))
    (setf (aref object index) value))
  (:method (value (key string) (object hash-table))
    (setf (gethash key object) value))
  (:method (value (pointer pathname) object)
    (if (equal #p"" pointer)
        (error 'invalid-key :key pointer :object object)
        (setf (jget (parse-pointer-pathname pointer) object)
              value)))
  (:method (value key (object string))
    (declare (ignore value key))
    (error 'non-indexable :value object))
  (:method (value key (object null))
    (declare (ignore value key))
    (error 'non-indexable :value object))
  (:method (value (index string) (object array))
    (declare (ignore value))
    (error 'invalid-key :key index :object object))
  (:method (value (key integer) (object hash-table))
    (declare (ignore value))
    (error 'invalid-key :key key :object object))
  (:documentation "Set the value at KEY-OR-INDEX in OBJECT.

The arguments are the same as in `jget'.

Throws `invalid-key' if using the wrong index type.
Throws `non-indexable' when trying to index something other than JSON
arrays or objects.
Throws `invalid-pointer' when using JSON Pointer with invalid syntax
as key.

OBJECT can be JSON array or object, which in Lisp translates to
`array' or `hash-table'."))

(defgeneric jrem (key-or-index object)
  (:method ((keys sequence) (object t))
    (jrem (elt keys (1- (length keys)))
          (jget (subseq keys 0 (1- (length keys))) object)))
  (:method ((index integer) (object array))
    (setf (subseq object index)
          (subseq object (1+ index))))
  (:method ((key string) (object hash-table))
    (remhash key object))
  (:method ((pointer pathname) object)
    (if (equal #p"" pointer)
        (error 'invalid-key :key pointer :object object)
        (jrem (parse-pointer-pathname pointer) object)))
  (:method (key (object null))
    (declare (ignore key))
    (error 'non-indexable :value object))
  (:method (key (object string))
    (declare (ignore key))
    (error 'non-indexable :value object))
  (:method ((index string) (object array))
    (error 'invalid-key :key index :object object))
  (:method ((key integer) (object hash-table))
    (error 'invalid-key :key key :object object))
  (:documentation "Remove the value at KEY-OR-INDEX of OBJECT.

The arguments are the same as in `jget'.

Throws `invalid-key' if using the wrong index type.
Throws `non-indexable' when trying to index something other than JSON
arrays or objects.
Throws `invalid-pointer' when using JSON Pointer with invalid syntax
as key."))

(defgeneric jcopy (object)
  (:method ((object real)) object)
  (:method ((object (eql :null))) object)
  (:method ((object (eql :undefined))) object)
  (:method ((object (eql t))) object)
  (:method ((object null)) object)
  (:method ((object string)) object)
  (:method ((object array))
    (map 'vector #'jcopy object))
  (:method ((object hash-table))
    (let ((new (make-hash-table :test 'equal)))
      (maphash (lambda (key val)
                 (setf (gethash key new) val))
               object)
      new))
  (:documentation "Copy the OBJECT, potentially creating an identical one."))

(defgeneric jtruep (object)
  (:method (object)
    (declare (ignore object))
    t)
  ;; TODO: A method on numbers, checking for zero?
  (:method ((object null))
    nil)
  (:method ((object array))
    (not (uiop:emptyp object)))
  (:method ((object hash-table))
    (plusp (hash-table-count object)))
  (:method ((object symbol))
    (not (member object (list nil :null))))
  (:documentation "Test OBJECT for truthiness in JSON terms."))

(dolist (symbol '(jtrue-p jtrue?))
  (setf (symbol-function symbol) #'jtruep))

(defun jnot (arg)
  "JSON-aware version of `cl:not'."
  (not (jtruep arg)))
