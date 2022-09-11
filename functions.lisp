;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3 Clause

(in-package #:njson)

(defgeneric jget (key-or-index object)
  (:method ((keys sequence) (object t))
    (reduce (lambda (data key) (jget key data))
            keys :initial-value object))
  (:method ((index integer) (object sequence))
    (elt object index))
  (:method ((key string) (object hash-table))
    (gethash key object))
  (:documentation "Get the value at KEY-OR-INDEX in OBJECT.

KEY-OR-INDEX can be
- an integer (for array indexing),
- a string (for object keying),
- or a sequence of integers and strings (to index the nested
  structures).

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
  (:documentation "Set the value at KEY-OR-INDEX in OBJECT.

KEY-OR-INDEX can be
- an integer (for array indexing),
- a string (for object keying),
- or a sequence of integers and strings (to modify the nested
  structures).

OBJECT can be JSON array or object, which in Lisp translates to any
valid `sequence' ot `hash-table'."))

(defun get** (key-or-index object)
  "Get the value at KEY-OR-INDEX in OBJECT.

OBJECT can be JSON array or object, which in Lisp translates to any
valid `sequence' ot `hash-table'.

For generic implementation and getails, see `jget'."
  (jget key-or-index object))

(defun (setf get**) (value key-or-index object)
  "Set the value at KEY-OR-INDEX in OBJECT.

For generic implementation and getails, see `jget'."
  (setf (jget key-or-index object) value))

(defgeneric jtruep (object)
  (:method (object)
    t)
  ;; TODO: A method on numbers, checking for zero?
  (:method ((object sequence))
    (not (uiop:emptyp object)))
  (:method ((object symbol))
    (and (not (member object (list nil :null)))))
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
