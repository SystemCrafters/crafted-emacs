;; crafted-ide-packages.el -*- lexical-binding: t; -*-

;; Copyright (C) 2022
;; SPDX-License-Identifier: MIT

;; Author: System Crafters Community

;; Commentary

;; Eglot package installation for Emacs prior to version 29.

;;; Code:


;; Add Eglot only for Emacs prior to version 29.  It is built-in since
;; Emacs 29.
(when (< emacs-major-version 29)
  ;; LSP capabilities
  (add-to-list 'package-selected-packages 'eglot)

  ;; tree-sitter syntax aware packages
  (add-to-list 'package-selected-packages 'tree-sitter)
  (add-to-list 'package-selected-packages 'tree-sitter-indent)
  (add-to-list 'package-selected-packages 'tree-sitter-ispell)
  (add-to-list 'package-selected-packages 'tree-sitter-langs))

;; Emacs 29 packages
(when (> emacs-major-version 29)
  ;; automatically handles switching to tree-sitter versions of major
  ;; modes, can install grammars, etc.
  (add-to-list 'package-selected-packages 'treesit-auto)

  ;; code navigation using tree-sitter grammars.
  (add-to-list 'package-selected-packages 'combobulate))

;; editorconfig is a cross-editor/ide configuration tool to control
;; indentation, spaces vs tabs, etc.
(add-to-list 'package-selected-packages 'editorconfig)

;; a minor mode to always keep your code indented while editing blocks
;; of code.
(add-to-list 'package-selected-packages 'aggressive-indent)

;; as most development is done on projects, add ibuffer-project to
;; group project buffers together when listing buffers with ibuffer
(add-to-list 'package-selected-packages 'ibuffer-project)

(provide 'crafted-ide-packages)
;;; crafted-ide-packages.el ends here
