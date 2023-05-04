;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3 Clause

(in-package #:njson/tests)

(define-test json-literals ()
  (assert-eq :null (decode "null"))
  (assert-false (decode "false"))
  (assert-eq t (decode "true")))

(define-test json-atoms ()
  (assert-equal 5 (decode "5"))
  (assert-float-equal 5.5 (decode "5.5"))
  (assert-float-equal -885.5 (decode "-885.5"))
  (assert-equal "foo32348hjvn" (decode "\"foo32348hjvn\""))
  (assert-equal "" (decode "\"\"")))

(define-test tricky-values ()
  (assert-typep 'hash-table (decode "{}"))
  (assert-eql 0 (hash-table-count (decode "{}")))
  (assert-typep 'vector (decode "[]"))
  (assert-equalp #(nil) (decode "[false]")))

(define-test from-file ()
  (destructuring-bind (simple-1 float-3.8 true false null
                       string-foo array-123 array-of-everything
                       object-quux-1883 object-of-everything)
      (coerce (decode (asdf:system-relative-pathname :njson "tests/test.json")) 'list)
    (assert-eql 1 simple-1)
    (assert-float-equal 3.8 float-3.8)
    (assert-eq t true)
    (assert-false false)
    (assert-eq :null null)
    (assert-equal "foo" string-foo)
    (assert-equalp #(1 2 3) array-123)
    (assert-equalp #("bar" t :null 1000000) array-of-everything)
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
    (assert-float-equal 1.3 (jcopy 1.3))
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

(define-test jget-errors ()
  (let ((object (decode (asdf:system-relative-pathname :njson "tests/test.json"))))
    (assert-error 'non-indexable (jget 20 nil))
    (assert-error 'non-indexable (jget 20 t))
    (assert-error 'non-indexable (jget 20 :null))
    (assert-error 'non-indexable (jget 20 :undefined))
    (assert-error 'non-indexable (jget 20 200))
    (assert-error 'non-indexable (jget 20 200.3))
    (assert-error 'non-indexable (jget 20 "foo"))
    (assert-error 'invalid-key (jget 20 (jget 8 object)))
    (assert-error 'invalid-key (jget "bar" (jget 7 object)))
    (assert-error 'invalid-pointer (jget #p"hello" object))))

(define-test setf-jget-errors ()
  (let ((object (decode (asdf:system-relative-pathname :njson "tests/test.json"))))
    (assert-error 'invalid-key (setf (jget 20 (jget 8 object)) nil))
    (assert-error 'invalid-key (setf (jget "bar" (jget 7 object)) nil))
    (assert-error 'invalid-key (setf (jget #p"" (jget 7 object)) nil))
    (assert-error 'invalid-key (setf (jget #() (jget 7 object)) nil))
    (assert-error 'non-indexable (setf (jget 20 200.3) 10))
    (assert-error 'non-indexable (setf (jget 20 "foo") nil))))

(define-test keys ()
  (let ((object (decode (asdf:system-relative-pathname :njson "tests/test.json"))))
    (assert-error 'non-indexable (jkeys (jget 0 object)))
    (assert-error 'non-indexable (jkeys (jget 5 object)))
    (assert-equal '(0 1 2 3) (jkeys (jget 7 object)))
    (assert-equal '("quux") (jkeys (jget 8 object)))
    (assert-equal '() (jkeys (decode "{}")))
    (assert-equal '() (jkeys (decode "[]")))))

(define-test ensure ()
  (assert-equalp #("hello") (ensure-array "hello"))
  (assert-equalp #(t) (ensure-array t))
  (assert-equalp #(nil) (ensure-array nil))
  (assert-equalp #(:null) (ensure-array :null))
  (assert-equalp #(:undefined) (ensure-array :undefined))
  (assert-equalp #(8) (ensure-array 8))
  (assert-equalp #(1.3) (ensure-array 1.3))
  (assert-equalp #(1.3 "foo") (ensure-array #(1.3 "foo")))
  (assert-equalp #() (ensure-array #()))
  (assert-equalp (vector (decode "{}"))
                 (ensure-array (decode "{}")))
  (assert-equalp (vector (decode "{\"a\": 1, \"b\": 2}"))
                 (ensure-array (decode "{\"a\": 1, \"b\": 2}")))
  ;; Order is not guaranteed.
  (assert-true (member (ensure-array (decode "{\"a\": 1, \"b\": 2}") :convert-objects t)
                       (list #(1 2) #(2 1))
                       :test #'equalp))

  (let ((literal-object (ensure-object "number" 3)))
    (assert-typep 'hash-table literal-object)
    (assert-eql 3 (jget "number" literal-object)))
  (let ((object-already (ensure-object "number" (njson:decode "{\"number\": 3}"))))
    (assert-typep 'hash-table object-already)
    (assert-eql 3 (jget "number" object-already)))
  (let ((array-object (ensure-object "numbers" #(1 2 3 4 82))))
    (assert-typep 'hash-table array-object)
    (assert-equalp #(1 2 3 4 82) (jget "numbers" array-object))))

(define-test jbind-patterns ()
  (let ((baker (decode (asdf:system-relative-pathname :njson "tests/baker.json"))))
    (macrolet ((assert-bind (path)
                 `(assert-true
                   (njson:jbind ,path
                       baker
                     t)))
               (assert-mismatch (path)
                 `(assert-error
                   'value-mismatch
                   (njson:jbind ,path
                       baker))))
      (assert-bind #())
      (assert-bind #(()))
      (assert-mismatch ())
      (assert-typep
       'hash-table
       (njson:jbind #(("data" data))
           baker
         data))
      (assert-bind #(("data" ("children" #()))))
      (assert-bind #(("data" ("children" #(())))))
      (assert-typep
       'hash-table
       (njson:jbind #(("data" ("children" #(("data" data)))))
           baker
         data))
      (assert-bind #(("data" ("children" #(("data" ()))))))
      (assert-equal
       "Henry Baker: Meta-circular semantics for Common Lisp special forms"
       (njson:jbind #(("data" ("children" #(("data" ("title" title))))))
           baker
         title))
      (assert-bind #(("data" ("children" #(("kind" "t3"))))))
      (assert-mismatch #(("data" ("children" ()))))
      (assert-mismatch #(("data" ("children" #(("kind" 5))))))
      (assert-bind #(("data" ("after" :null))))
      (assert-mismatch #(("data" ("after" "not null"))))
      (assert-bind #(("data" ("dist" 1))))
      (assert-mismatch #(("data" ("dist" 1.000001))))
      (assert-bind #(("data" ("children" #(("data" ("upvote_ratio" 0.59)))))))
      (assert-mismatch #(("data" ("children" #(("data" ("upvote_ratio" 0.6)))))))
      (assert-bind #(("data" ("children" #(("data" ("hide_score" :false)))))))
      (assert-mismatch #(("data" ("children" #(("data" ("hide_score" :true)))))))
      (assert-bind #(("data" ("children" #(("data" ("is_robot_indexable" :true)))))))
      (assert-mismatch #(("data" ("children" #(("data" ("is_robot_indexable" :false)))))))
      (assert-bind #(("data" ("modhash" ""))))
      (assert-mismatch #(("data" ("modhash" "non-empty-string"))))
      (assert-mismatch #(("data" ("modhash" #()))))
      (assert-bind
       #(("data"
          ("children"
           #(("data" ("title"
                      "Henry Baker: Meta-circular semantics for Common Lisp special forms")))))))
      (assert-error
       'type-error
       (macroexpand
        '(jbind #(("data" ("modhash" :invalid-keword)))
          baker)))
      (assert-true
       (jbind true
           t
         true))
      (assert-false
       (jbind false
           nil
         nil))
      (assert-equal
       "hello" (jbind string
                   "hello"
                 string))
      ;; Test lenient var-p bindings
      (assert-bind #(("data" ("modhash" (modhash modhash-p)))))
      (assert-bind #(("data" ("modfoo" (modfoo modfoo-p)))))
      (assert-bind #(("data" ("modfoo" (modfoo))))))))

