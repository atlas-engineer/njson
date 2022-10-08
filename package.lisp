;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3 Clause

(uiop:define-package #:njson
  (:use #:common-lisp)
  (:export
   ;; Conditions
   #:decode-json-from-stream-not-implemented
   #:encode-json-to-stream-not-implemented
   #:invalid-key #:non-indexable
   ;; Main generics
   #:decode #:encode
   ;; Generics to implement for backends.
   #:decode-from-stream #:decode-from-string #:decode-from-file
   #:encode-to-stream #:encode-to-string #:encode-to-file
   ;; Helpers
   #:jhas #:has_ #:jget #:get_ #:jrem #:rem_ #:jcopy #:copy_
   #:jtruep #:jtrue-p #:jtrue? #:truep #:true-p #:true?
   ;; Macro helpers
   #:jif #:if_ #:jwhen #:when_
   #:jor #:or_ #:jand #:and_ #:jnot #:not_)
  (:documentation "NJSON is a convenience library for JSON handling. Important functions/APIs:
- `njson:encode' and `njson:decode' as universal (en|de)coding functions working
  on strings, streams, and pathnames.
- `njson:jget' (and `njson:get_' alias) to get the value from decoded
  and arbitrarily nested JSON array/object.
- `njson:jtruep' (and aliases) to check the non-falsity of a decoded
  value.
- `njson:jif', `njson:jwhen', `njson:jor', `njson:jand', and
  `njson:jnot' (and aliases) as convenience macros for JSON
  non-falsity-based control flow.

Generics to implement:
- `njson:encode-json-to-stream' and `njson:decode-json-from-stream' as the basic
  methods to specialize for every backend.
- `njson:encode-json-to-string' and `njson:encode-json-to-file', as more specific
  methods to speed things up.
- `njson:decode-json-from-string' and `njson:decode-json-from-file', as more
  specific decoding methods."))
