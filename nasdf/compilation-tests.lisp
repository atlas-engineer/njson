;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nasdf)

(export-always 'nasdf-compilation-test-system)
(defclass nasdf-compilation-test-system (nasdf-test-system)
  ((packages
    :initform '()  ;; (error "Packages required")
    :initarg :packages
    :reader packages
    :documentation "Packages to check for unbound exports.
Sub-packages are included in the check."))
  (:documentation "Specialized systems for compilation tests."))
(import 'nasdf-compilation-test-system :asdf-user)

(defun valid-type-p (type-specifier)
  (handler-case
      (progn
        (typep t type-specifier)
        t)
    (error () nil)))

(defun list-unbound-exports (package)
  (let ((result '()))
    (do-external-symbols (s (find-package package) result)
      (unless (or (fboundp s)
                  (boundp s)
                  (find-class s nil)
                  (valid-type-p s)
                  (and (find-package :parenscript)
                       (gethash s (symbol-value (find-symbol "*MACRO-TOPLEVEL*" :parenscript)))))
        (push s result)))))

(defun subpackage-p (subpackage package)
  "Return non-nil if SUBPACKAGE is a sub-package of PACKAGE.
A sub-package has a name that starts with that of PACKAGE followed by a '/' separator."
  (not (null
        (uiop:string-prefix-p (uiop:strcat (package-name package) "/")
                              (package-name subpackage)))))

(defun list-subpackages (package)
  (remove-if (lambda (pkg) (not (subpackage-p pkg package))) (list-all-packages)))

(defun list-undocumented-exports (package)
  (let ((result '()))
    (do-external-symbols (s (find-package package) result)
      (unless (or (some (lambda (doctype) (documentation s doctype))
                        '(variable function compiler-macro setf method-combination type structure))
                  ;; Parenscript macros don't have documentation.
                  (and (find-package :parenscript)
                       (gethash s (symbol-value (find-symbol "*MACRO-TOPLEVEL*" :parenscript)))))
        (push s result)))))

(flet ((list-offending-packages (package export-lister testing-for)
         (let* ((package (find-package package)))
           (delete nil
                   (mapcar (lambda (package)
                             (logger ";;; Testing ~a for ~a" package testing-for)
                             (let ((exports (funcall export-lister package)))
                               (when exports
                                 (list package exports))))
                           (cons (find-package package) (list-subpackages package)))))))
  (defun unbound-exports (package)
    "Report unbound exported symbols for PACKAGE and all its subpackages."
    ;; NOTE: these implementations throw errors on atypical type specifier, enabling `valid-type-p'
    #+(or sbcl ccl ecl clisp)
    (let ((report (list-offending-packages package #'list-unbound-exports "unbound exports")))
      (when report
        (error "~a~&Found unbound exported symbols in ~a package~:p."
               report (length report))))
    #-(or sbcl ccl ecl clisp) nil)

  (defun undocumented-exports (package)
    "Report undocumented exported symbols for PACKAGE and all its subpackages."
    (let ((report (list-offending-packages package #'list-undocumented-exports "undocumented exports")))
      (when report
        (error "~a~&Found undocumented exported symbols in ~a package~:p."
               report (length report))))))

(defmethod asdf:perform ((op asdf:test-op) (c nasdf-compilation-test-system))
  (logger "------- STARTING Compilation Testing: ~a" (packages c))
  (mapc #'unbound-exports (packages c))
  (mapc #'undocumented-exports (packages c))
  (logger "------- ENDING Compilation Testing: ~a" (packages c)))
