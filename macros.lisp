;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3 Clause

(defmacro jif (test then &optional (else nil))
  "JSON-aware version of `cl:if'.
If TEST is `jtruep' evaluate THEN, otherwise evaluate ELSE."
  `(if (jtruep ,test)
       ,then
       ,else))

(defmacro if* (test then &optional (else nil))
  "JSON-aware version of `cl:if'.
If TEST is `jtruep' evaluate THEN, otherwise evaluate ELSE.
Alias for `jif'."
  (jif ,test ,then ,else))

(defmacro jwhen (test &body body)
  "JSON-aware version of `cl:when'.
If TEST is `jtruep' evaluate BODY."
  `(jif ,test
       (progn ,@body)))

(defmacro when* (test &body body)
  "JSON-aware version of `cl:when'.
If TEST is `jtruep' evaluate BODY.
Alias for `jwhen'."
  `(jwhen ,test ,@body))

(defmacro jor (&rest args)
  "JSON-aware version of `cl:or'."
  `(or ,@(loop for arg in args
               collecting `(jwhen ,arg ,arg))))

(defmacro or* (&rest args)
  "JSON-aware version of `cl:or'.
Alias for `jor'."
  `(or ,@args))

(defmacro jand (&rest args)
  "JSON-aware version of `cl:and'."
  `(and ,@(loop for arg in args
                collecting `(jwhen ,arg ,arg))))

(defmacro and* (&rest args)
  "JSON-aware version of `cl:and'.
Alias for `jand'."
  `(and ,@args))

(defmacro jnot (arg)
  "JSON-aware version of `cl:not'."
  `(not (jtrue ,arg)))

(defmacro not* (arg)
  "JSON-aware version of `cl:not'.
Alias for `jnot'."
  (jnot ,arg))
