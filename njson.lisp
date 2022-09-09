;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3 Clause

(in-package #:njson)

(defgeneric decode-json-from-stream (stream)
  (:method (stream)
    (signal 'decode-json-from-stream-not-implemented))
  (:documentation "Decode JSON from STREAM.
Specialize on `stream' to make NJSON decode JSON."))

(defgeneric decode-json-from-string (string)
  (:method (string)
    (with-input-from-string (stream string)
      (decode-json-from-stream stream)))
  (:documentation "Decode JSON from STRING.
Specialize on `string' to make NJSON better decode JSON strings.
Uses `decode-json-from-stream' by default."))

(defgeneric decode-json-from-file (file)
  (:method (file)
    (with-open-file (stream file :direction :input)
      (decode-json-from-stream stream)))
  (:documentation "Decode JSON from FILE.
Specialize on `pathname' to make NJSON better decode JSON files.
Uses `decode-json-from-stream' by default."))

(defgeneric decode (from)
  (:method :around (from))
  (:documentation "Decode OBJECT from JSON source FROM.
FROM can be a string, stream, pathname, or byte array.

Distinguishes between null/false and arrays/objects.
Decodes:
- null as :NULL,
- undefined as :UNDEFINED,
- false as nil,
- true as t,
- objects as hash-tables."))

(defgeneric encode-json-to-stream (object stream)
  (:method (object stream)
    (signal 'encode-json-to-stream-not-implemented))
  (:documentation "Encode OBJECT to STREAM as JSON.
Specialize on `stream' (and, optionally, OBJECT types) to make NJSON encode JSON."))

(defgeneric encode-json-to-string (object)
  (:method (object)
    (with-output-to-string (stream)
      (encode-json-to-stream object stream)
      (get-output-stream-string stream)))
  (:documentation "Encode OBJECT to JSON string.
Specialize on `string' (and, optionally, OBJECT types) to make NJSON better encode JSON to strings.
Uses `encode-json-to-stream' by default."))

(defgeneric encode-json-to-file (object file)
  (:method (object file)
    (with-open-file (stream file)
      (encode-json-to-stream object stream)))
  (:documentation "Encode OBJECT to FILE.
Specialize on `pathname' (and, optionally, OBJECT types) to make NJSON better encode JSON to files.
Uses `encode-json-to-stream' by default."))

(defgeneric encode (object &optional to)
  (:method :around (object &optional to)
    (typecase to
      (null (encode-json-to-string object))
      (pathname (encode-json-to-file object to))
      (stream (call-next-method object to))
      ((eql t) (call-next-method object *standard-output*))))
  (:method (object &optional to)
    (encode-json-to-stream object to))
  (:documentation "Encode OBJECT to JSON output spec TO.
TO can be:
- T, in which case `*standard-output*' is used as encoding stream.
- NIL, in which case OBJECT is encoded to a string.
- STREAM, in which case OBJECT is encoded to it.
- PATHNAME, in which case OBJECT is encoded to the file designated by the pathname.

Distinguishes between null and false.
Encodes:
- :NULL as null,
- :UNDEFINED as undefined,
- nil as false."))
