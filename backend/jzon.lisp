;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package #:njson)

;; FIXME: 'null shouldn't be taken as valid.

(defmethod com.inuoe.jzon:write-value ((writer com.inuoe.jzon:writer) (value (eql :null)))
  "This allows us to serialize :null as actual null JSON."
  (com.inuoe.jzon:write-value writer 'null))

(deftype com.inuoe.jzon:json-atom ()
  "Redefined type so that :null counts too."
  `(or (eql t)
       (eql nil)
       (eql null)
       (eql :null)
       real
       string))

(defun default-decode (in)
  (labels ((convert-nulls (object)
             (typecase object
               (string object)
               (array
                (loop for elem across object
                      for idx from 0
                      if (or (hash-table-p elem)
                             (and (arrayp elem)
                                  (not (stringp elem))))
                        do (convert-nulls elem)
                      else if (and (symbolp elem)
                                   (eq elem 'null))
                             do (setf (elt object idx) :null))
                object)
               (hash-table
                (maphash (lambda (key value)
                           (cond
                             ((eq 'null value)
                              (setf (gethash key object) :null))
                             ((or (hash-table-p value)
                                  (and (arrayp value)
                                       (not (stringp value))))
                              (convert-nulls value))))
                         object)
                object)
               (t (if (eq object 'null)
                      :null
                      object)))))
    (convert-nulls (com.inuoe.jzon:parse in :allow-comments t :allow-trailing-comma t))))

(defmethod decode-from-stream ((stream stream))
  (default-decode stream))

(defmethod decode-from-string ((string string))
  (default-decode string))

(defmethod decode-from-file ((file pathname))
  (default-decode file))

(defun default-encode (object)
  (com.inuoe.jzon:stringify object ))

(defmethod encode-to-stream ((object t) (stream stream))
  (com.inuoe.jzon:stringify object :stream stream ))

(defmethod encode-to-string ((object t))
  (com.inuoe.jzon:stringify object))

;; NOTE: not specializing `encode-to-file', because the default plays
;; well enough with stream specialization.
