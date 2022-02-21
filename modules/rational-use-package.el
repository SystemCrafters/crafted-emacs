;;; rational-use-package.el -*- lexical-binding: t; -*-

;; Copyright (C) 2022
;; SPDX-License-Identifier: MIT

;; Author: System Crafters Community

;;; Commentary:

;; Built-in `package.el' with `use-package' configuration, including
;; elpas to search.

;;; Code:

(require 'package)

;; Emacs 27.x has the gnu elpa as the default for `package-archives'
;; Emacs 28.x has the gnu and nongnu elpa as the default.
;; Only add nongnu elpa for Emacs version < 28
(when (version< emacs-version "28.0")
  (add-to-list 'package-archives (cons "nongnu" "https://elpa.nongnu.org/nongnu/")))

;; Always add melpa to `package-archives'
(add-to-list 'package-archives (cons "melpa" "https://melpa.org/packages/"))

;; prefer gnu and nongnu elpas over melpa
(customize-set-variable 'package-archive-priorities
                        '(("gnu"    . 99)
                          ("nongnu" . 80)
                          ("melpa"  . 0)))

;; don't automatically activate all the installed packages, allow
;; use-package, require or autoloads to do so when the user provides
;; appropriate configuration.
(package-initialize t)
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(provide 'rational-use-package)
;;; rational-use-package.el ends here
