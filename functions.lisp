;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3 Clause

(defgeneric jget (key-or-index object)
  (:method ((index integer) (object sequence))
    (elt object index))
  (:method (key (object hash-table))
    (gethash key object))
  (:documentation "Get the value at KEY-OR-INDEX in OBJECT.

OBJECT can be JSON array or object, which in Lisp translates to any
valid `sequence' ot `hash-table'."))

(defgeneric (setf jget) (value key-or-index object)
  (:method (value (index integer) (object sequence))
    (setf (elt object index) value))
  (:method (value key (object hash-table))
    (setf (gethash key object) value))
  (:documentation "Set the value at KEY-OR-INDEX in OBJECT.

OBJECT can be JSON array or object, which in Lisp translates to any
valid `sequence' ot `hash-table'."))

(defun get** (key-or-index object)
  "Get the value at KEY-OR-INDEX in OBJECT.

OBJECT can be JSON array or object, which in Lisp translates to any
valid `sequence' ot `hash-table'.

For generic implementation, see `jget'."
  (jget key-or-index object))

(defun (setf get**) (value key-or-index object)
  "Set the value at KEY-OR-INDEX in OBJECT.

OBJECT can be JSON array or object, which in Lisp translates to any
valid `sequence' ot `hash-table'.

For generic implementation, see `jget'."
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
