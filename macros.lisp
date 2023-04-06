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
