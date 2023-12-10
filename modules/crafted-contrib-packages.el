;;; crafted-contrib-packages.el --- Emacs Lisp contribution packages  -*- lexical-binding: t; -*-

;; Copyright (C) 2023
;; SPDX-License-Identifier: MIT

;; Author: System Crafters Community

;;; Commentary:

;; Packages that assist core/package Emacs Lisp code contribution.

;;; Code:

;; Access the GNU bug tracker within Emacs.
(add-to-list 'package-selected-packages 'debbugs)

;; Linting library for package metadata, available both as a command
;; and as a Flymake backend.
(add-to-list 'package-selected-packages 'package-lint)
(add-to-list 'package-selected-packages 'package-lint-flymake)

;; Indentation helper.
(add-to-list 'package-selected-packages 'aggressive-indent-mode)

(provide 'crafted-contrib-packages)
;;; crafted-contrib-packages.el ends here
