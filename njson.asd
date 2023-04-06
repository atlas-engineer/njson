;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3 Clause

(defsystem "njson"
  :description "NJSON is a JSON handling framework with the focus on convenience and brevity."
  :author "Atlas Engineer LLC"
  :homepage "https://github.com/atlas-engineer/njson"
  :license  "BSD-3 Clause"
  :version "0.2.4"
  :serial t
  :components ((:file "package")
               (:file "conditions")
               (:file "njson")
               (:file "functions")
               (:file "macros")
               (:file "aliases")))

(defsystem "njson/submodules"
  :defsystem-depends-on ("nasdf")
  :class :nasdf-submodule-system)

(defsystem "njson/cl-json"
  :depends-on ("njson" "cl-json")
  :components ((:file "backend/cl-json"))
  :in-order-to ((test-op (test-op "njson/tests")
                         (test-op "njson/tests/compilation"))))

(defsystem "njson/tests"
  :defsystem-depends-on ("nasdf")
  :class :nasdf-test-system
  :depends-on ("njson")
  :targets (:package :njson/tests)
  :serial t
  :pathname "tests/"
  :components ((:file "package")
               (:file "tests")))

(defsystem "njson/tests/compilation"
  :defsystem-depends-on ("nasdf")
  :class :nasdf-compilation-test-system
  :depends-on ("njson")
  :packages (:njson))
