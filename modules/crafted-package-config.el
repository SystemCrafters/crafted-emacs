;;; crafted-package-config.el --- Macro helpers for crafted modules  -*- lexical-binding: t; -*-

;; Copyright (C) 2022
;; SPDX-License-Identifier: MIT

;; Author: System Crafters Community

;;; Commentary:

;; Helpers for installing packages.  Crafted Emacs assumes the use of
;; `package.el' for package management, thus these are built to
;; support that library.

;; `crafted-package-installer' defaults to `package-install', but can
;; be set by the user to some other installer function.

;; `crafted-package-installed-predicate' is used to check if a package
;; is already installed; it defautls to `package-installed-p', but can
;; be set by the user to some other predicate function.

;; `crafted-package-install-package' is used to install individual
;; packages, the installer and predicate functions can be passed in if
;; needed.

;; `crafted-package-install-package-list' installs packages from the
;; `package-selected-packages' list.  All `crafted-<module>-package'
;; files add packages to this list, they are not installed
;; automatically.  The user is responsible for iterating over that
;; list to install the packages, if desired.  Users are not obligated
;; to use the `crafted-<module>-package' files and may prefer to
;; manage installing packages without using any of the facilities
;; here.  Additionally, if they choose to use the
;; `crafted-<module>-package' files, they have the opportunity modify
;; the list before calling any processing occurs to install any packages

;;; Code:
(require 'package)

(defvar crafted-package-installer #'package-install
  "Function to use when installing packages")

(defvar crafted-package-installed-predicate #'package-installed-p
  "Function to use when checking if a package is installed")

(defun crafted-package-install-package (package &optional installer-fn predicate-fn)
  "Install PACKAGE optionally using the INSTALLER-FN.

Uses the PREDICATE-FN to check if a package is already installed
before installing it.  Default values for both the PREDICATE-FN an
INSTALLER-FN are held in the
`crafted-package-installed-predicate' and
`crafted-package-installer' variables."
  (let ((checker (or predicate-fn crafted-package-installed-predicate))
        (installer (or installer-fn crafted-package-installer)))
    (unless (funcall checker package)
      (funcall installer package))))

(defun crafted-package-install-selected-packages ()
  "Installs all packages listed in the `package-selected-packages' list.

If `crafted-package-installer' has been customized, use it to
install packages one at at time from the list
`package-selected-packages', otherwise use the built-in
`package-install-selected-packages', which is purpose built for
this."
  (if (eq #'package-install crafted-package-installer)
      (package-install-selected-packages t)
    (mapc #'crafted-package-install-package package-selected-packages)))

(provide 'crafted-package-config)
;;; crafted-package-config.el ends here
