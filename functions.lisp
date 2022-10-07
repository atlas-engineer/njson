;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3 Clause

(in-package #:njson)

(defgeneric jget (key-or-index object)
  (:method ((keys sequence) (object t))
    (if (= 1 (length keys))
        (jget (elt keys 0) object)
        (jget (subseq keys 1)
              (jget (elt keys 0) object))))
  (:method ((index integer) (object sequence))
    (elt object index))
  (:method ((key string) (object hash-table))
    (gethash key object))
  (:method (key (object string))
    (error 'jget-non-indexable :value object))
  (:method ((index string) (object sequence))
    (error 'jget-invalid-key :key index :object object))
  (:method ((key integer) (object hash-table))
    (error 'jget-invalid-key :key key :object object))
  (:documentation "Get the value at KEY-OR-INDEX in OBJECT.

KEY-OR-INDEX can be
- an integer (for array indexing),
- a string (for object keying),
- or a sequence of integers and strings (to index the nested
  structures).

Throws `jget-invalid-key' if using the wrong index.

For example, to get the data from a structure like
{\"data\": [1, 2, {\"three\": 3}]}
you can use
(jget #(\"data\" 2 \"three\") data)
;; => 3

OBJECT can be JSON array or object, which in Lisp translates to any
valid `sequence' ot `hash-table'."))

(defgeneric (setf jget) (value key-or-index object)
  (:method (value (keys sequence) (object t))
    (setf (jget (elt keys (1- (length keys)))
                (jget (subseq keys 0 (1- (length keys))) object))
          value))
  (:method (value (index integer) (object sequence))
    (setf (elt object index) value))
  (:method (value (key string) (object hash-table))
    (setf (gethash key object) value))
  (:method (value key (object string))
    (error 'jget-non-indexable :value object))
  (:method (value (index string) (object sequence))
    (error 'setf-jget-invalid-key :key index :object object))
  (:method (value (key integer) (object hash-table))
    (error 'setf-jget-invalid-key :key key :object object))
  (:documentation "Set the value at KEY-OR-INDEX in OBJECT.

KEY-OR-INDEX can be
- an integer (for array indexing),
- a string (for object keying),
- or a sequence of integers and strings (to modify the nested
  structures).

Throws `setf-jget-invalid-key' if using the wrong index type.

OBJECT can be JSON array or object, which in Lisp translates to any
valid `sequence' ot `hash-table'."))

(defun get_ (key-or-index object)
  "Get the value at KEY-OR-INDEX in OBJECT.

OBJECT can be JSON array or object, which in Lisp translates to any
valid `sequence' ot `hash-table'.

For generic implementation and getails, see `jget'."
  (jget key-or-index object))

(defun (setf get_) (value key-or-index object)
  "Set the value at KEY-OR-INDEX in OBJECT.

For generic implementation and getails, see `jget'."
  (setf (jget key-or-index object) value))

(defgeneric jrem (key-or-index object)
  (:method ((keys sequence) (object t))
    (jrem (elt keys (1- (length keys)))
          (jget (subseq keys 0 (1- (length keys))) object)))
  (:method ((index integer) (object sequence))
    (setf (subseq object index)
          (subseq object (1+ index))))
  (:method ((key string) (object hash-table))
    (remhash key object))
  (:method ((index string) (object sequence))
    (error 'jrem-invalid-key :key index :object object))
  (:method ((key integer) (object hash-table))
    (error 'jrem-invalid-key :key key :object object))
  (:documentation "Remove the value at KEY-OR-INDEX of OBJECT.

The arguments are the same as in `jget'.

Throws `jrem-invalid-key' if using the wrong type index."))

(defun rem_ (key-or-index object)
  "Remove the value at KEY-OR-INDEX of OBJECT.

For generic implementation and getails, see `jrem'."
  (jrem key-or-index object))

(defgeneric jtruep (object)
  (:method (object)
    t)
  ;; TODO: A method on numbers, checking for zero?
  (:method ((object sequence))
    (not (uiop:emptyp object)))
  (:method ((object hash-table))
    (plusp (hash-table-count object)))
  (:method ((object symbol))
    (not (member object (list nil :null))))
  (:documentation "Test OBJECT for truthiness in JSON terms."))

(macrolet ((defalias (name)
             `(defun ,name (object)
                "Test OBJECT for truthiness in JSON terms.
For generic implementation, see `jtruep'."
                (jtruep object))))
  (defalias jtrue-p)
  (defalias jtrue?)
  (defalias truep)
  (defalias true-p)
  (defalias true?))
