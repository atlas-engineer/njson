;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3 Clause

(in-package #:njson)

(defmacro jif (test then &optional (else nil))
  "JSON-aware version of `cl:if'.
If TEST is `jtruep' evaluate THEN, otherwise evaluate ELSE."
  `(if (jtruep ,test)
       ,then
       ,else))

(defmacro if_ (test then &optional (else nil))
  "JSON-aware version of `cl:if'.
If TEST is `jtruep' evaluate THEN, otherwise evaluate ELSE.
Alias for `jif'."
  (warn 'deprecated :deprecated 'if_ :replacement "NJSON/ALIASES:TRUE")
  `(jif ,test ,then ,else))

(defmacro jwhen (test &body body)
  "JSON-aware version of `cl:when'.
If TEST is `jtruep' evaluate BODY."
  `(jif ,test
       (progn ,@body)))

(defmacro when_ (test &body body)
  "JSON-aware version of `cl:when'.
If TEST is `jtruep' evaluate BODY.
Alias for `jwhen'."
  (warn 'deprecated :deprecated 'when_ :replacement "NJSON/ALIASES:when")
  `(jwhen ,test ,@body))

(defmacro jor (&rest args)
  "JSON-aware version of `cl:or'."
  `(or ,@(loop for arg in args
               collecting `(jwhen ,arg ,arg))))

(defmacro or_ (&rest args)
  "JSON-aware version of `cl:or'.
Alias for `jor'."
  (warn 'deprecated :deprecated 'or_ :replacement "NJSON/ALIASES:OR")
  `(or ,@args))

(defmacro jand (&rest args)
  "JSON-aware version of `cl:and'."
  `(and ,@(loop for arg in args
                collecting `(jwhen ,arg ,arg))))

(defmacro and_ (&rest args)
  "JSON-aware version of `cl:and'.
Alias for `jand'."
  (warn 'deprecated :deprecated 'and_ :replacement "NJSON/ALIASES:AND")
  `(and ,@args))
