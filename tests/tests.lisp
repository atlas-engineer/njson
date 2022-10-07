;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3 Clause

(in-package #:njson/tests)

(define-test json-literals ()
  (assert-eq :undefined (j:decode "undefined"))
  (assert-eq :null (j:decode "null"))
  (assert-false (j:decode "false"))
  (assert-eq t (j:decode "true")))

(define-test json-atoms ()
  (assert-equal 5 (j:decode "5"))
  (assert-equal 5.5 (j:decode "5.5"))
  (assert-equal -885.5 (j:decode "-885.5"))
  (assert-equal "foo32348hjvn" (j:decode "\"foo32348hjvn\""))
  (assert-equal "" (j:decode "\"\"")))

(define-test from-file ()
  (destructuring-bind (simple-1 float-3.8 true false undefined null
                       string-foo array-123 array-of-everything
                       object-quux-1883 object-of-everything)
      (j:decode (asdf:system-relative-pathname :njson "tests/test.json"))
    (assert-eql 1 simple-1)
    (assert-eql 3.8 float-3.8)
    (assert-eq t true)
    (assert-false false)
    (assert-eq :undefined undefined)
    (assert-eq :null null)
    (assert-equal "foo" string-foo)
    (assert-equalp '(1 2 3) array-123)
    (assert-equalp '("bar" 8.3 t :null 1000000) array-of-everything)
    (assert-typep 'hash-table object-quux-1883)
    (assert-eql 1883 (j:jget "quux" object-quux-1883))
    (assert-typep 'hash-table object-of-everything)
    (assert-eql 1 (j:jget "foo" object-of-everything))
    (assert-equalp '(1 2 "hey") (j:jget "bar" object-of-everything))
    (assert-typep 'hash-table (j:jget "quux" object-of-everything))
    (assert-eql 1 (j:jget "one" (j:jget "quux" object-of-everything)))))
