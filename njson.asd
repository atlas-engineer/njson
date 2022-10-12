;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3 Clause

(defsystem "njson"
  :description "NJSON is a JSON handling framework with the focus on convenience and brevity."
  :author "Atlas Engineer LLC"
  :homepage "https://github.com/atlas-engineer/njson"
  :license  "BSD-3 Clause"
  :version "0.1.0"
  :serial t
  :components ((:file "package")
               (:file "conditions")
               (:file "njson")
               (:file "functions")
               (:file "macros")))

(defsystem "njson/cl-json"
  :depends-on (#:njson #:cl-json)
  :components ((:file "backend/cl-json"))
  :in-order-to ((test-op (test-op "njson/tests"))))

(defsystem "njson/tests"
  :depends-on (#:njson #:lisp-unit2)
  :serial t
  :pathname "tests/"
  :components ((:file "package")
               (:file "tests"))
  :perform (test-op (o c)
                    (symbol-call :lisp-unit2 :run-tests
                                 :package :njson/tests
                                 :run-contexts (find-symbol "WITH-SUMMARY-CONTEXT" :lisp-unit2))))
