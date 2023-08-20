;;;; crafted-writing-packages.el --- Packages used for writing  -*- lexical-binding: t; -*-

;; Copyright (C) 2023
;; SPDX-License-Identifier: MIT

;; Author: System Crafters Community

;;; Commentary:

;; Packages used for writing different kinds of documents.

;;; Code:

;; Markdown support
(add-to-list 'package-selected-packages 'markdown-mode)
(add-to-list 'package-selected-packages 'pandoc-mode)

;; LaTeX support - uses Auctex
;; only install and load auctex when the latex executable is found,
;; otherwise it crashes when loading
(when (executable-find "latex")
  (add-to-list 'package-selected-packages 'auctex))

;; Install the auctex-latexmk package when the latex and latexmk
;; executable are found.
;;
;; This package contains a bug which might make it crash during loading
;; (with a bug related to tex-buf) on newer systems.
;;
;; If you encounter the bug, you should uninstall this package, then
;; you can install a fix (not on melpa) with the following recipe,
;; and the configuration in this file will still work
;;
;; (N.B. the recipe is for straight.el, but can be modified for use with Emacs
;;       29 package-vc, quelpa.el, or other \"from source\" package
;;       managers.)
;;
;; '(auctex-latexmk :fetcher git :host github :repo \"wang1zhen/auctex-latexmk\")
(when (and (executable-find "latex")
           (executable-find "latexmk"))
  (add-to-list 'package-selected-packages 'auctex-latexmk))

(provide 'crafted-writing-packages)
;;; crafted-writing-packages.el ends here
