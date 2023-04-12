;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3 Clause

(uiop:define-package #:njson/aliases
  (:use #:common-lisp)
  (:export
   #:decode #:encode
   #:get #:copy #:true #:truep #:true?
   #:keys #:ensure-array #:ensure-object
   #:if #:when #:or #:and #:not)
  (:shadow #:get #:rem #:if #:when #:or #:and #:not)
  (:documentation "Short aliases for the regular njson functions.
Perfect with j: package-local-nickname, disastrous when :use-d."))

(in-package #:njson/aliases)

(setf (symbol-function 'njson/aliases:decode) #'njson:decode
      (symbol-function 'njson/aliases:encode) #'njson:encode
      (symbol-function 'njson/aliases:get) #'njson:jget
      (fdefinition '(setf njson/aliases:get)) (fdefinition '(setf njson:jget))
      (symbol-function 'njson/aliases:copy) #'njson:jcopy
      (symbol-function 'njson/aliases:true) #'njson:jtruep
      (symbol-function 'njson/aliases:truep) #'njson:jtruep
      (symbol-function 'njson/aliases:true?) #'njson:jtruep
      (symbol-function 'njson/aliases:keys) #'njson:jkeys
      (symbol-function 'njson/aliases:ensure-array) #'njson:ensure-array
      (symbol-function 'njson/aliases:ensure-object) #'njson:ensure-object)

(defmacro njson/aliases:if (test then &optional (else nil))
  "JSON-aware version of `cl:if'.
If TEST is `njson:jtruep' evaluate THEN, otherwise evaluate ELSE.
Alias for `njson:jif'."
  `(njson:jif ,test ,then ,else))

(defmacro njson/aliases:when (test &body body)
  "JSON-aware version of `cl:when'.
If TEST is `njson:jtruep' evaluate BODY."
  `(njson:jwhen ,test ,@body))

(defmacro njson/aliases:or (&rest args)
  "JSON-aware version of `cl:or'."
  `(njson:jor ,@args))

(defmacro njson/aliases:and (&rest args)
  "JSON-aware version of `cl:and'."
  `(njson:jand ,@args))

(defun njson/aliases:not (arg)
  "JSON-aware version of `cl:not'."
  (njson:jnot arg))
