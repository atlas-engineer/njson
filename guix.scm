;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3 Clause

;;; Commentary:
;;
;; GNU Guix development package.  To start the REPL:
;;
;;   guix shell -f path/to/guix.scm sbcl -- sbcl
;;
;;; Code:

(use-modules (guix packages)
             (guix gexp)
             (gnu packages lisp-xyz))

(package
  (inherit cl-njson)
  (version "dev")
  (source (local-file (dirname (current-filename)) #:recursive? #t)))
