;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3 Clause

(in-package #:njson)

(define-condition decode-json-from-stream-not-implemented (error) ()
  (:report "DECODE-JSON-FROM-STREAM is not specialized.
You need to specialize it to use NJSON. Example:

(defmethod njson:decode-json-from-stream ((stream stream))
  (some-json-parsing-library:decode-json-from-stream stream))

Alternatively, load a system with this method already defined, like :njson/cl-json."))

(define-condition encode-json-to-stream-not-implemented (error) ()
  (:report "ENCODE-JSON-TO-STREAM is not specialized.
You need to specialize it to use NJSON. Example:

(defmethod njson:encode-json-to-stream ((object t) (stream stream))
  (some-json-parsing-library:encode-json-to-stream object stream))

Alternatively, load a system with this method already defined, like :njson/cl-json."))
