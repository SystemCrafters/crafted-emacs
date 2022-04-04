;;; rational-use-package.el -*- lexical-binding: t; -*-

;; Copyright (C) 2022
;; SPDX-License-Identifier: MIT

;; Author: System Crafters Community

;; Commentary

;; Built-in `package.el' with `use-package' configuration, including
;; elpas to search.

;;; Code:

(rational-install-package 'use-package)

;; we are possibly here because `straight.el' is preferred, so only
;; configure package in that case. This work has already been done if
;; `package.el' is enabled at startup.
(unless package-enable-at-startup
 (require 'package)

 ;; Emacs 27.x has the gnu elpa as the default for `package-archives'
 ;; Emacs 28.x has the gnu and nongnu elpa as the default.
 ;; Only add nongnu elpa for Emacs version < 28
 (when (version< emacs-version "28.0")
   (add-to-list 'package-archives (cons "nongnu" "https://elpa.nongnu.org/nongnu/")))

 ;; Always add melpa to `package-archives'
 (add-to-list 'package-archives (cons "melpa" "https://melpa.org/packages/"))

 (package-initialize)
 (unless package-archive-contents
   (package-refresh-contents)))

(provide 'rational-use-package)
;;; rational-use-package.el ends here
