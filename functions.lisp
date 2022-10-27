;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3 Clause

(in-package #:njson)

(defgeneric jhas (key-or-index object)
  (:method ((keys sequence) (object t))
    (jhas (elt keys (1- (length keys)))
          (jget (subseq keys 0 (1- (length keys))) object)))
  (:method ((index integer) (object sequence))
    (<= 0 index (1- (length object))))
  (:method ((key string) (object hash-table))
    (nth-value 1 (gethash key object)))
  (:method (key (object string))
    (declare (ignore key))
    (error 'non-indexable :value object))
  (:method ((index string) (object sequence))
    (error 'invalid-key :key index :object object))
  (:method ((key integer) (object hash-table))
    (error 'invalid-key :key key :object object))
  (:documentation "Check the presence of the value under KEY-OR-INDEX in OBJECT.

The arguments are the same as in `jget'.

Throws `invalid-key' if using the wrong index type.
Throws `non-indexable' when trying to index something other than JSON
arrays or objects."))

(defun has_ (key-or-index object)
  "Check the presence of the value under KEY-OR-INDEX in OBJECT.

For generic implementation and getails, see `jhas'."
  (warn 'deprecated :deprecated 'has_ :replacement "NJSON/ALIASES:HAS")
  (jhas key-or-index object))

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
    (declare (ignore key))
    (error 'non-indexable :value object))
  (:method ((index string) (object sequence))
    (error 'invalid-key :key index :object object))
  (:method ((key integer) (object hash-table))
    (error 'invalid-key :key key :object object))
  (:documentation "Get the value at KEY-OR-INDEX in OBJECT.

KEY-OR-INDEX can be
- an integer (for array indexing),
- a string (for object keying),
- or a sequence of integers and strings (to index the nested
  structures).

Throws `invalid-key' if using the wrong index type.
Throws `non-indexable' when trying to index something other than JSON
arrays or objects.

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
    (declare (ignore value key))
    (error 'non-indexable :value object))
  (:method (value (index string) (object sequence))
    (declare (ignore value))
    (error 'invalid-key :key index :object object))
  (:method (value (key integer) (object hash-table))
    (declare (ignore value))
    (error 'invalid-key :key key :object object))
  (:documentation "Set the value at KEY-OR-INDEX in OBJECT.

KEY-OR-INDEX can be
- an integer (for array indexing),
- a string (for object keying),
- or a sequence of integers and strings (to modify the nested
  structures).

Throws `invalid-key' if using the wrong index type.
Throws `non-indexable' when trying to index something other than JSON
arrays or objects.

OBJECT can be JSON array or object, which in Lisp translates to any
valid `sequence' ot `hash-table'."))

(defun get_ (key-or-index object)
  "Get the value at KEY-OR-INDEX in OBJECT.

OBJECT can be JSON array or object, which in Lisp translates to any
valid `sequence' ot `hash-table'.

For generic implementation and getails, see `jget'."
  (warn 'deprecated :deprecated 'get_ :replacement "NJSON/ALIASES:GET")
  (jget key-or-index object))

(defun (setf get_) (value key-or-index object)
  "Set the value at KEY-OR-INDEX in OBJECT.

For generic implementation and getails, see `jget'."
  (warn 'deprecated :deprecated 'get_ :replacement "NJSON/ALIASES:get")
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
  (:method (key (object string))
    (declare (ignore key))
    (error 'non-indexable :value object))
  (:method ((index string) (object sequence))
    (error 'invalid-key :key index :object object))
  (:method ((key integer) (object hash-table))
    (error 'invalid-key :key key :object object))
  (:documentation "Remove the value at KEY-OR-INDEX of OBJECT.

The arguments are the same as in `jget'.

Throws `invalid-key' if using the wrong index type.
Throws `non-indexable' when trying to index something other than JSON
arrays or objects."))

(defun rem_ (key-or-index object)
  "Remove the value at KEY-OR-INDEX of OBJECT.

For generic implementation and getails, see `jrem'."
  (warn 'deprecated :deprecated 'rem_ :replacement "NJSON/ALIASES:REM")
  (jrem key-or-index object))

(defgeneric jcopy (object)
  (:method ((object real)) object)
  (:method ((object (eql :null))) object)
  (:method ((object (eql :undefined))) object)
  (:method ((object (eql t))) object)
  (:method ((object null)) object)
  (:method ((object string)) object)
  (:method ((object sequence))
    (map (type-of object) #'jcopy object))
  (:method ((object hash-table))
    (let ((new (make-hash-table :test 'equal)))
      (maphash (lambda (key val)
                 (setf (gethash key new) val))
               object)
      new))
  (:documentation "Copy the OBJECT, potentially creating an identical one."))

(defun copy_ (object)
  "Copy the OBJECT, potentially creating an identical one.

For generic implementation and getails, see `jcopy'."
  (warn 'deprecated :deprecated 'copy_ :replacement "NJSON/ALIASES:COPY")
  (jcopy object))

(defgeneric jtruep (object)
  (:method (object)
    (declare (ignore object))
    t)
  ;; TODO: A method on numbers, checking for zero?
  (:method ((object sequence))
    (not (uiop:emptyp object)))
  (:method ((object hash-table))
    (plusp (hash-table-count object)))
  (:method ((object symbol))
    (not (member object (list nil :null))))
  (:documentation "Test OBJECT for truthiness in JSON terms."))

(dolist (symbol '(jtrue-p jtrue?))
  (setf (symbol-function symbol) #'jtruep))

(macrolet ((defalias (name)
             `(defun ,name (object)
                "Test OBJECT for truthiness in JSON terms.
For generic implementation, see `jtruep'."
                (warn 'deprecated :deprecated (quote ,name) :replacement "NJSON/ALIASES:TRUE")
                (jtruep object))))
  (defalias truep)
  (defalias true-p)
  (defalias true?))

(defun jnot (arg)
  "JSON-aware version of `cl:not'."
  (not (jtruep arg)))

(defun not_ (arg)
  "JSON-aware version of `cl:not'.
Alias for `jnot'."
  (warn 'deprecated :deprecated 'not_ :replacement "NJSON/ALIASES:NOT")
  (jnot arg))
