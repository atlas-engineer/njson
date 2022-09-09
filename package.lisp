;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3 Clause

(uiop:define-package #:njson
  (:use #:common-lisp)
  (:export
   ;; Conditions
   #:decode-json-from-stream-not-implemented
   #:encode-json-to-stream-not-implemented
   ;; Main generics
   #:decode #:encode
   ;; Generics to implement for backends.
   #:decode-json-from-stream #:decode-json-from-string #:decode-json-from-file
   #:encode-json-to-stream #:encode-json-to-string #:encode-json-to-file
   ;; Helpers
   #:jget #:get*
   #:jtruep #:jtrue-p #:jtrue? #:truep #:true-p #:true?
   ;; Macro helpers
   #:jif #:if* #:jwhen #:when*
   #:jor #:or* #:jand #:and* #:jnot #:not*)
  (:documentation ""))
