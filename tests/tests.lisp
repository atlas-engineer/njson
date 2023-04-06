;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3 Clause

(in-package #:njson/tests)

(define-test json-literals ()
  (assert-eq :undefined (decode "undefined"))
  (assert-eq :null (decode "null"))
  (assert-false (decode "false"))
  (assert-eq t (decode "true")))

(define-test json-atoms ()
  (assert-equal 5 (decode "5"))
  (assert-equal 5.5 (decode "5.5"))
  (assert-equal -885.5 (decode "-885.5"))
  (assert-equal "foo32348hjvn" (decode "\"foo32348hjvn\""))
  (assert-equal "" (decode "\"\"")))

(define-test tricky-values ()
  (assert-typep 'hash-table (decode "{}"))
  (assert-eql 0 (hash-table-count (decode "{}")))
  (assert-typep 'vector (decode "[]"))
  (assert-equalp #(nil) (decode "[false]")))

;;; TODO: jget, jrem tests.

(define-test from-file ()
  (destructuring-bind (simple-1 float-3.8 true false undefined null
                       string-foo array-123 array-of-everything
                       object-quux-1883 object-of-everything)
      (coerce (decode (asdf:system-relative-pathname :njson "tests/test.json")) 'list)
    (assert-eql 1 simple-1)
    (assert-eql 3.8 float-3.8)
    (assert-eq t true)
    (assert-false false)
    (assert-eq :undefined undefined)
    (assert-eq :null null)
    (assert-equal "foo" string-foo)
    (assert-equalp #(1 2 3) array-123)
    (assert-equalp #("bar" 8.3 t :null 1000000) array-of-everything)
    (assert-typep 'hash-table object-quux-1883)
    (assert-eql 1883 (jget "quux" object-quux-1883))
    (assert-typep 'hash-table object-of-everything)
    (assert-eql 1 (jget "foo" object-of-everything))
    (assert-equalp #(1 2 "hey") (jget "bar" object-of-everything))
    (assert-typep 'hash-table (jget "quux" object-of-everything))
    (assert-eql 1 (jget "one" (jget "quux" object-of-everything)))))

(define-test jcopy-test ()
  (let* ((array #(1 2 3 "hello")))
    (assert-eql 8 (jcopy 8))
    (assert-eql 1.3 (jcopy 1.3))
    (assert-eq :null (jcopy :null))
    (assert-eq :undefined (jcopy :undefined))
    (assert-error 'error (jcopy :whatever))
    (assert-eq t (jcopy t))
    (assert-false (jcopy nil))
    (assert-equal "hello there" (jcopy "hello there"))
    ;; TODO: hash-tables
    (assert-false (eq array (jcopy array)))
    (assert-equalp array (jcopy array))))

(define-test jget-json-pointer ()
  (let ((object (decode (asdf:system-relative-pathname
                         ;; Taken directly from RFC 6901.
                         :njson "tests/pointer-test.json"))))
    (assert-eq object (jget #p"" object))
    (assert-equalp #("bar" "baz") (jget #p"/foo" object))
    (assert-equal "bar" (jget #p"/foo/0" object))
    (assert-eql 0 (jget #p"/" object))
    (assert-eql 1 (jget #p"/a~1b" object))
    (assert-eql 2 (jget #p"/c%d" object))
    (assert-eql 3 (jget #p"/e^f" object))
    (assert-eql 4 (jget #p"/g|h" object))
    ;; FIXME: broken due to pathname processing.
    ;; (assert-eql 5 (jget #p"/i\\j" object))
    (assert-eql 6 (jget #p"/k\"l" object))
    (assert-eql 7 (jget #p"/ " object))
    (assert-eql 8 (jget #p"/m~0n" object))))
