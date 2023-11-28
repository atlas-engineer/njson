;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(uiop:define-package #:njson/aliases
  (:use #:common-lisp)
  (:export
   #:decode #:encode
   #:get #:get* #:copy #:true #:truep #:true?
   #:keys #:ensure-array #:ensure-object
   #:if #:when #:or #:and #:not
   #:bind #:match
   #:@)
  (:shadow #:get #:rem #:if #:when #:or #:and #:not)
  (:documentation "Short aliases for the regular njson functions.
Perfect with j: package-local-nickname, disastrous when :use-d."))

(in-package #:njson/aliases)

(loop for (alias original) in '((njson/aliases:decode njson:decode)
                                (njson/aliases:encode njson:encode)
                                (njson/aliases:get njson:jget)
                                (njson/aliases:get* njson:jget*)
                                ((setf njson/aliases:get) (setf njson:jget))
                                (njson/aliases:copy njson:jcopy)
                                (njson/aliases:true njson:jtruep)
                                (njson/aliases:truep njson:jtruep)
                                (njson/aliases:true? njson:jtruep)
                                (njson/aliases:keys njson:jkeys)
                                (njson/aliases:not njson:jnot)
                                (njson/aliases:ensure-array njson:ensure-array)
                                (njson/aliases:ensure-object njson:ensure-object))
      do (setf (fdefinition alias) (fdefinition original))
      unless (listp alias)
        do (setf (documentation alias 'function) (documentation original 'function)))

(loop for (alias original) in '((njson/aliases:if njson:jif)
                                (njson/aliases:when njson:jwhen)
                                (njson/aliases:or njson:jor)
                                (njson/aliases:and njson:jand)
                                (njson/aliases:bind njson:jbind)
                                (njson/aliases:match njson:jmatch))
      do (setf (macro-function alias) (macro-function original))
      unless (listp alias)
        do (setf (documentation alias 'function) (documentation original 'function)))

(defun @ (object &rest keys)
  "Alias for `jget' that indexes OBJECT with KEYS.
Setf-able."
  (njson:jget keys object))

(defun (setf @) (value object &rest keys)
  (setf (njson:jget keys object) value))
