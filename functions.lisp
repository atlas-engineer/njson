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
      (restart-case
          (with-input-from-string (s name)
            (loop for char = (read-char s nil nil)
                  while char
                  unless (eq #\/ char)
                    do (cerror "Use the pointer anyway"
                               'invalid-pointer :pointer pointer-pathname)
                  collect (parse-if-number (resolve-tildes (read-until #\/ s)))))
        (another-pointer (new-pointer)
          :report "Parse another pointer"
          :interactive read-new-pointer
          (parse-pointer-pathname new-pointer))))))

(defgeneric jget (key-or-index object &optional error-p)
  (:method ((keys sequence) (object t) &optional error-p)
    (declare (ignore error-p))
    (case (length keys)
      (0 object)
      (1 (jget (elt keys 0) object))
      (t (jget (subseq keys 1)
               (jget (elt keys 0) object)))))
  (:method ((index integer) (object array) &optional error-p)
    (cond
      ((<= 0 index (1- (length object)))
       (values (aref object index) t))
      (error-p
       (restart-case
           (cerror "Ignore key absence, return nothing"
                   'no-such-key :object object :key index)
         (another-key (new-key)
           :report "Use another key"
           :interactive read-new-key
           (check-type new-key integer)
           (jget new-key object error-p))))
      (t (values nil nil))))
  (:method ((key string) (object hash-table) &optional error-p)
    (cond
      ((nth-value 1 (gethash key object))
       (gethash key object))
      (error-p
       (restart-case
           (cerror "Ignore key absence, return nothing"
                   'no-such-key :object object :key key)
         (another-key (new-key)
           :report "Use another key"
           :interactive read-new-key
           (check-type new-key string)
           (jget new-key object error-p))))
      (t (values nil nil))))
  (:method ((pointer pathname) object &optional error-p)
    (declare (ignore error-p))
    (if (equal #p"" pointer)
        (values object t)
        (jget (parse-pointer-pathname pointer) object)))
  (:method ((index string) (object array) &optional error-p)
    (if error-p
        (restart-case
            (cerror "Return nothing"
                    'invalid-key :key index :object object)
          (use-integer (new-index)
            :report "Use an integer key"
            :interactive read-new-key
            (check-type new-index integer)
            (jget new-index object error-p)))
        (values nil nil)))
  (:method ((key integer) (object hash-table) &optional error-p)
    (if error-p
        (restart-case
            (cerror "Return nothing"
                    'invalid-key :key key :object object)
          (use-string (new-key)
            :report "Use a string key"
            :interactive read-new-key
            (check-type new-key string)
            (jget new-key object error-p)))
        (values nil nil)))
  (:method (key object &optional error-p)
    (declare (ignore key))
    (when error-p
      (cerror "Return nothing"
              'non-indexable :value object))
    (values nil nil))
  (:method :around (key (object string) &optional error-p)
    (declare (ignore key))
    (when error-p
      (cerror "Return nothing"
              'non-indexable :value object))
    (values nil nil))
  (:documentation "Get the value at KEY-OR-INDEX in OBJECT.

KEY-OR-INDEX can be
- an integer (for array indexing),
- a string (for object keying),
- a pathname (with JSON Pointer syntax),
- a sequence of integers and strings (to index the nested structures).
- an empty sequence/pathname (to match the whole object).

If ERROR-P:
- Throws `no-such-key' when the key is not present in object.
- Throws `invalid-key' if using the wrong index type.
- Throws `non-indexable' when trying to index something other than
  JSON arrays or objects.
- Throws `invalid-pointer' when using JSON Pointer with invalid syntax
  as key.

If not ERROR-P: does nothing and returns nothing in all the
exceptional cases.

For example, to get the data from a structure like
{\"data\": [1, 2, {\"three\": 3}]}
you can use
(jget #(\"data\" 2 \"three\") data)
;; => 3

OBJECT can be JSON array or object, which in Lisp translates to
`array' or `hash-table'."))

(defgeneric (setf jget) (value key-or-index object &optional error-p)
  (:method (value (keys sequence) (object t) &optional error-p)
    (case (length keys)
      (0 (when error-p
           (cerror "Don't set the value"
                   'invalid-key :key keys :object object)))
      (1 (setf (jget (elt keys 0) object) value))
      (t (setf (jget (elt keys (1- (length keys)))
                     (jget (subseq keys 0 (1- (length keys))) object))
               value))))
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
  (:method (value (index integer) (object array) &optional error-p)
    (cond
      ((<= 0 index (1- (length object)))
       (setf (aref object index) value))
      (error-p
       (restart-case
           (cerror "Don't set the value"
                   'no-such-key :object object :key index)
         (another-key (new-key)
           :report "Use another key"
           :interactive read-new-key
           (check-type new-key integer)
           (setf (jget new-key object error-p) value))))))
  (:method (value (key string) (object hash-table) &optional error-p)
    (cond
      ((nth-value 1 (gethash key object))
       (setf (gethash key object) value))
      (error-p
       (restart-case
           (progn
             (cerror "Set the value anyway"
                     'no-such-key :object object :key key)
             (setf (gethash key object) value))
         (another-key (new-key)
           :report "Use another key"
           :interactive read-new-key
           (check-type new-key string)
           (setf (jget new-key object error-p) value))))
      (t (setf (gethash key object) value))))
  (:method (value (pointer pathname) object &optional error-p)
    (if (equal #p"" pointer)
        (when error-p
          (restart-case
              (cerror "Don't set the value"
                      'invalid-key :key pointer :object object)
            (another-pointer (new-pointer)
              :report "Use another pointer"
              :interactive read-new-pointer
              (setf (jget new-pointer object error-p) value))))
        (setf (jget (parse-pointer-pathname pointer) object)
              value)))
  (:method (value (index string) (object array) &optional error-p)
    (when error-p
      (restart-case
          (cerror "Don't set the value"
                  'invalid-key :key index :object object)
        (use-integer (new-key)
          :report "Use an integer key"
          :interactive read-new-key
          (check-type new-key integer)
          (setf (jget new-key object error-p) value)))) )
  (:method (value (key integer) (object hash-table) &optional error-p)
    (when error-p
      (restart-case
          (cerror "Don't set the value"
                  'invalid-key :key key :object object)
        (use-string (new-key)
          :report "Use a string key"
          :interactive read-new-key
          (check-type new-key string)
          (setf (jget new-key object error-p) value)))))
  (:method (value key (object t) &optional error-p)
    (declare (ignore value key))
    (when error-p
      (cerror "Don't set the value"
              'non-indexable :value object)))
  (:method :around (value key (object string) &optional error-p)
    (declare (ignore value key))
    (when error-p
      (cerror "Do nothing"
              'non-indexable :value object)))
  (:documentation "Set the value at KEY-OR-INDEX in OBJECT.

The arguments are the same as in `jget', except KEY-OR-INDEX cannot be
an empty pathname/sequence (because setting the object itself to a new
value is not possible in CL, unless it's a place, which is not
guaranteed for `jget' arguments).

If ERROR-P:
- Throws `no-such-key' when the key is not present in object.
- Throws `invalid-key' if using the wrong index type.
- Throws `non-indexable' when trying to index something other than
  JSON arrays or objects.
- Throws `invalid-pointer' when using JSON Pointer with invalid syntax
  as key.

If not ERROR-P: does nothing and returns nothing in all the
exceptional cases.

OBJECT can be JSON array or object, which in Lisp translates to
`array' or `hash-table'."))

(defgeneric jcopy (object)
  (:method ((object real)) object)
  (:method ((object (eql :null))) object)
  (:method ((object (eql :undefined))) object)
  (:method ((object (eql t))) object)
  (:method ((object null)) object)
  (:method ((object string)) object)
  (:method ((object array))
    (make-array (length object)
                :adjustable t
                :fill-pointer t
                :initial-contents (map 'vector #'jcopy object)))
  (:method ((object hash-table))
    (let ((new (make-hash-table :test 'equal)))
      (maphash (lambda (key val)
                 (setf (gethash key new) val))
               object)
      new))
  (:documentation "Copy the OBJECT, potentially creating an identical one.
Coerce all JSON arrays to adjustable vectors."))

(defgeneric jkeys (object)
  (:method ((object vector))
    (loop for i from 0 below (length object)
          collect i))
  (:method ((object string))
    (cerror "Return nothing"
            'non-indexable :value object))
  (:method ((object hash-table))
    (loop for key being the hash-key of object
          collect key))
  (:method ((object t))
    (cerror "Return nothing"
            'non-indexable :value object))
  (:documentation "Get keys to index OBJECT with, as a list of integers/strings.
If the OBJECT is not a JSON array/object, throws `non-indexable'."))

(defgeneric jtruep (object)
  (:method (object)
    (declare (ignore object))
    t)
  (:method ((object symbol))
    (not (member object (list nil :null :undefined))))
  (:documentation "Test OBJECT for truthiness in JSON terms.

Recognize all the values true, except for null, undefined, and
false. This is to make the transition from JSON to Lisp (3+ false
values -> 1 false value) smoother.

Unlike JavaScript, empty strings and zero are not false (because this
behavior is confusing)."))

(dolist (symbol '(jtrue-p jtrue?))
  (setf (symbol-function symbol) #'jtruep))

(defun jnot (arg)
  "JSON-aware version of `cl:not'."
  (not (jtruep arg)))

(defgeneric ensure-array (object &key &allow-other-keys)
  (:method ((object hash-table) &key convert-objects &allow-other-keys)
    (if convert-objects
        (make-array (hash-table-count object)
                    :adjustable t
                    :fill-pointer t
                    :initial-contents (loop for key in (jkeys object)
                                            collect (jget key object)))
        (make-array 1 :initial-contents (list object))))
  (:method ((object vector) &key &allow-other-keys)
    object)
  (:method ((object string) &key &allow-other-keys)
    (make-array 1 :adjustable t :fill-pointer t :initial-contents (list object)))
  (:method ((object t) &key &allow-other-keys)
    (make-array 1 :adjustable t :fill-pointer t :initial-contents (list object)))
  (:documentation "Ensure that the return value is an array.
If OBJECT is an array already, return it.
If it's a literal value, wrap it into a one-element array.
If it's an object:
- When CONVERT-OBJECTS is T, put all the values into an array (order
  not guaranteed).
- Otherwise wrap the object into an array."))

(defgeneric ensure-object (key object &key &allow-other-keys)
  (:method ((key string) (object hash-table) &key &allow-other-keys)
    (jget key object t)
    object)
  (:method ((key string) (object t) &key &allow-other-keys)
    (let ((hash-table (make-hash-table :test 'equal)))
      (setf (jget key hash-table) object)
      hash-table))
  (:documentation "Ensure that the return value is a JSON object.
If OBJECT is an object already, return it, checking KEY presence.
If it's anything else, wrap it into an object with OBJECT under KEY.

Throws errors (`no-such-key', `invalid-key') from underlying `jget'."))
