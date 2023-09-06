;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3 Clause

(in-package #:njson)

(defmacro jif (test then &optional (else nil))
  "JSON-aware version of `cl:if'.
If TEST is `jtruep' evaluate THEN, otherwise evaluate ELSE."
  `(if (jtruep ,test)
       ,then
       ,else))

(defmacro jwhen (test &body body)
  "JSON-aware version of `cl:when'.
If TEST is `jtruep' evaluate BODY."
  `(jif ,test
       (progn ,@body)))

(defmacro jor (&rest args)
  "JSON-aware version of `cl:or'."
  `(or ,@(loop for arg in args
               collecting `(jwhen ,arg ,arg))))

(defmacro jand (&rest args)
  "JSON-aware version of `cl:and'."
  `(and ,@(loop for arg in args
                collecting `(jwhen ,arg ,arg))))

(defun check-value (expected indices object)
  "Check that JSON value in OBJECT at INDICES is `equal' to EXPECTED specification."
  (restart-case
      (let ((result (jget indices object)))
        (or (typecase expected
              ((eql t) (jget indices object))
              ((eql :true) (eq t result))
              ((eql :false) (eq nil result))
              ((and array (not string))
               (and (arrayp result)
                    (not (stringp result))))
              (list (hash-table-p result))
              ;; This is to allow double and single float comparisons.
              (number (when (numberp result)
                        (< (abs (- result expected))
                           single-float-epsilon)))
              (t (equal result expected)))
            (cerror
             "Ignore the mismatch"
             'value-mismatch
             :expected (typecase expected
                         ((eql :true) t)
                         ((eql :false) nil)
                         (t expected))
             :actual result
             :object (jget (subseq indices 0
                                   ;; This 1- and max is to get the
                                   ;; "parent" of RESULT.
                                   (max 0 (1- (length indices))))
                           object))
            t))
    (store-value (new-value)
      :report "Replace the offending value"
      :interactive read-new-value
      (setf (jget indices object) new-value)
      (check-value expected indices object))
    (use-value (new-value)
      :report "Return a replacement"
      :interactive read-new-value
      new-value)))

;; DESTRUCTURING-PATTERN is not a (&rest destructuring-pattern)
;; because it might be a vector too.
(defmacro jbind (destructuring-pattern form &body body)
  "Match the FORM against DESTRUCTURING-PATTERN.
The pattern might be:
- A symbol, in which case the current chosen form is bound to it. If
  the symbol is _, simply skip the form.
- A literal form:
  - String or number: compare with `equal'.
  - Keywords :TRUE, :FALSE, and :NULL, matching T, NIL, and :NULL
    respectively.
- If the pattern is a property list of string+pattern pairs, match the
  string+pattern pairs inside it to the provided JSON object and
  resolve them recursively.
- If the pattern is a list of symbols (VAR &optional VAR-P), these are
  bound to the respective values of `jget'. It is a good way to make
  `jbind' to be more lenient to missing keys, because the default
  behavior is to error on missing data.
- If the pattern is an inline vector, match it against a JSON array
  with at least as many elements as provided in the vector. Match
  every form in the vector against the element with the same index.

If the DESTRUCTURING-PATTERN doesn't match the object, throws `invalid-key'

Underlying `jget' can throw errors for the exceptionally malformed
inputs. See `jget' documentation for the types of errors it throws.

Example:
\(\"hello\" hello \"a\" _ \"b\" b
 \"array\" #(first second third _))

matches a JSON object
{\"hello\": 3, \"a\": 8, \"b\": 3, \"c\": null, \"array\": [1, 2, 3, 4]}

and binds
- HELLO to 3
- B to 3
- FIRST to 1
- SECOND to 2
- THIRD to 3

It also checks that \"a\" key is present in the object and there's a
fourth element in the nested array.

See more examples in njson tests."
  (let ((form-sym (gensym "BIND-FORM"))
        (bindings (list)))
    (labels ((parse-pattern (pattern &optional (current-path (list)))
               (etypecase pattern
                 ((or (member :true :false :null :undefined) string real)
                  (push (cons pattern (copy-list current-path))
                        bindings))
                 ((cons symbol *)
                  (push (cons pattern (copy-list current-path))
                        bindings))
                 (list
                  (loop for (key subpattern) on pattern by #'cddr
                        do (parse-pattern subpattern (append current-path (list key))))
                  (push (cons nil (copy-list current-path))
                        bindings))
                 ((and symbol (not keyword))
                  (push (cons (if (equal "_" (symbol-name pattern))
                                  (gensym "_PATTERN")
                                  pattern)
                              (copy-list current-path))
                        bindings))
                 (array (loop for elem across pattern
                              for index from 0
                              do (parse-pattern elem (append current-path (list index))))
                        (push (cons #() (copy-list current-path))
                              bindings)))))
      (check-type destructuring-pattern (or list (and array (not string))
                                            (and symbol (not keyword)))
                  "proper jbind destructuring pattern: list, array, or symbol")
      (parse-pattern destructuring-pattern)
      (let ((let-forms (loop for (binding . key) in bindings
                             do (check-type binding (or array real symbol
                                                        ;; For (VAR VAR-P) forms
                                                        (cons symbol (or (cons symbol null)
                                                                         null))))
                             if (typep binding '(or array real null
                                                 (member :true :false :null :undefined)))
                               collect `(,(gensym) (check-value ,binding (vector ,@key) ,form-sym))
                             else if (and (symbolp binding)
                                          (uiop:emptyp key))
                                    collect `(,binding ,form-sym)
                             else if (listp binding)
                                    append (destructuring-bind (var &optional (var-p nil var-p-provided))
                                               binding
                                             (append
                                              `((,var (ignore-errors (jget (vector ,@key) ,form-sym))))
                                              (when var-p-provided
                                                `((,var-p (handler-case
                                                              (prog1
                                                                  t
                                                                (jget (vector ,@key) ,form-sym))
                                                            (no-key ()
                                                              nil)))))))
                             else
                               collect `(,binding (check-value t (vector ,@key) ,form-sym)))))
        `(let* ((,form-sym ,form)
                ,@let-forms)
           (declare (ignorable ,form-sym ,@(mapcar #'first let-forms)))
           ,@body)))))

(defmacro jmatch (form &body clauses)
  "Similar to Trivia match macro, match the FORM (JSON value) against CLAUSES.
CLAUSES are (PATTERN . BODY) forms, where
- PATTERN is a `jbind' destructuring pattern.
- And BODY is an implicit progn.

If PATTERN matches successfully in `jbind', then BODY is executed with
the variables from the PATTERN bound to the respective values, as per
`jbind'.

The last clause could start with T, NIL, OTHERWISE, ELSE, or _, and it
will be invoked if other patterns don't match. If there's no such
clause, `jmatch' will simply return NIL on no matching patterns."
  (let ((form-sym (gensym "MATCH-FORM")))
    `(let ((,form-sym ,form))
       (cond
         ,@(loop for (pattern . body) in clauses
                 when (and (symbolp pattern)
                           (member (symbol-name pattern) '("T" "_" "NIL" "OTHERWISE" "ELSE")
                                   :test #'string=))
                   collect `(t ,@body)
                 else
                   collect `((ignore-errors (jbind ,pattern ,form-sym t))
                             (jbind ,pattern ,form-sym ,@body)))))))
