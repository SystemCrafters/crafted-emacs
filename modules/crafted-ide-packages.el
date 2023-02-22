;; crafted-ide-packages.el -*- lexical-binding: t; -*-

;; Copyright (C) 2022
;; SPDX-License-Identifier: MIT

;; Author: System Crafters Community

;; Commentary

;; Eglot package installation for Emacs prior to version 29.

;;; Code:


;; Add Eglot only for Emacs prior to version 29.  It is built-in since
;; Emacs 29.
(when (version< emacs-version "29")
  ;; LSP capabilities
  (add-to-list 'package-selected-packages 'eglot)

  ;; tree-sitter syntax aware packages
  (add-to-list 'package-selected-packages 'tree-sitter                )
  (add-to-list 'package-selected-packages 'tree-sitter-indent)
  (add-to-list 'package-selected-packages 'tree-sitter-ispell)
  (add-to-list 'package-selected-packages 'tree-sitter-langs))

;; Emacs 29 packages
(when (> (string-to-number emacs-version) 29)
  ;; automatically handles switching to tree-sitter versions of major
  ;; modes, can install grammars, etc.
  (add-to-list 'package-selected-packages 'treesit-auto)

  ;; code navigation using tree-sitter grammars. 
  (add-to-list 'package-selected-packages 'combobulate))

(provide 'crafted-ide-packages)
;;; crafted-ide-packages.el ends here


