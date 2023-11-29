;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(defsystem "njson"
  :description "NJSON is a JSON handling framework with the focus on convenience and brevity."
  :author "Atlas Engineer LLC"
  :homepage "https://github.com/atlas-engineer/njson"
  :bug-tracker "https://github.com/atlas-engineer/njson/issues"
  :source-control (:git "https://github.com/atlas-engineer/njson.git")
  :license  "BSD-3 Clause"
  :version "1.2.2"
  :serial t
  :components ((:file "package")
               (:file "conditions")
               (:file "njson")
               (:file "functions")
               (:file "macros")
               (:file "aliases"))
  :in-order-to ((test-op (test-op "njson/tests"))))

(defsystem "njson/cl-json"
  :depends-on ("njson" "cl-json")
  :components ((:file "backend/cl-json"))
  :in-order-to ((test-op (test-op "njson/tests"))))

(defsystem "njson/jzon"
  :depends-on ("njson" "com.inuoe.jzon")
  :components ((:file "backend/jzon"))
  :in-order-to ((test-op (test-op "njson/tests"))))

(defsystem "njson/tests"
  :depends-on ("njson" "lisp-unit2")
  :serial t
  :pathname "tests/"
  :components ((:file "package")
               (:file "tests"))
  :perform (test-op (op c)
                    (eval-input
                     "(lisp-unit2:run-tests
                       :package :njson/tests
                       :run-contexts #'lisp-unit2:with-summary-context)")))
