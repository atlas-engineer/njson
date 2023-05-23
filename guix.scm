;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3 Clause

;;; Commentary:
;;
;; GNU Guix development package.  To build and install, clone this repository,
;; switch directory to here and run:
;;
;;   guix package --install-from-file=guix.scm
;;
;; To use as the basis for a development environment, run:
;;
;;   guix shell --container -D -f build-scripts/guix.scm
;;
;; Replace --container by --pure if you still want ASDF to see external
;; libraries in ~/common-lisp, etc.
;; To build a local executable and then run it:
;;; Code:

(use-modules (guix packages)
             ((guix licenses) #:prefix license:)
             (guix gexp)
             (guix git-download)
             (guix build-system asdf)
             (gnu packages)
             (gnu packages lisp)
             (gnu packages lisp-check)
             (gnu packages lisp-xyz))

(define-public sbcl-njson
  (package
   (name "sbcl-njson")
   (version "1.1.0")
   (source
    (local-file (dirname (current-filename)) #:recursive? #t)
    ;;;; Or this, in case of contributing to Guix.
    ;; (origin
    ;;   (method git-fetch)
    ;;   (uri (git-reference
    ;;         (url "https://github.com/atlas-engineer/njson")
    ;;         (commit version)))
    ;;   (file-name (git-file-name "cl-njson" version))
    ;;   (sha256
    ;;    (base32
    ;;     "SPECIFY-HASH")))
    )
   (build-system asdf-build-system/sbcl)
   ;; We use `cl-*' inputs and not `sbcl-*' ones so that CCL users can also use
   ;; this Guix manifests.
   ;;
   ;; Another reason is to not fail when an input dependency is found in
   ;; ~/common-lisp, which would trigger a rebuild of the SBCL input in the
   ;; store, which is read-only and would thus fail.
   ;;
   ;; The official Guix package should use `sbcl-*' inputs though.
   (native-inputs (list cl-lisp-unit2 sbcl))
   (inputs (list cl-json cl-jzon))
   (arguments
    '(#:asd-systems '("njson" "njson/jzon")))
   (synopsis "JSON handling framework for Common Lisp.")
   (home-page "https://github.com/atlas-engineer/njson")
   (description "NJSON aims to make it convenient for one to decode, encode,
and process JSON data, in the minimum keystrokes/minutes possible.

NJSON is parser-independent, with existing Common Lisp JSON parsers being
loadable as additional system.  @code{jzon} is included by default, though.
Conveniences that NJSON provides are:

@itemize

@item @code{encode} and @code{decode} as single entry points for JSON reading
and writing, be it from streams/string/files, or from those.

@item @code{jget}, @code{jrem}, @code{jtruep}, and their aliases to
access/delete the decoded objects' properties and check their truth value
without the need to worry about the low-level details of how these values are
decoded.

@item @code{jif}, @code{jwhen}, @code{jor}, @code{jand}, and other macros
mimicking Lisp ones, while using truth values of JSON-decoded data.

@item @code{njson/aliases} package to nickname to @code{j} for all the
forms conveniently accessible as @code{j:rem}, @code{j:get},
@code{j:if} etc.

@end itemize\n")
   (license license:bsd-3)))

(define-public cl-njson
  (sbcl-package->cl-source-package sbcl-njson))

(define-public ecl-njson
  (sbcl-package->ecl-package sbcl-njson))

cl-njson
