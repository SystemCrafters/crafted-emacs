;;; crafted-pdf-reader.el -*- lexical-binding: t; -*-

;; Copyright (C) 2022
;; SPDX-License-Identifier: MIT

;; Author: System Crafters Community

;; Commentary

;; Configuration for the pdf-tools package to read pdf files

;;; Code:

(crafted-package-install-package 'pdf-tools)

(add-hook 'doc-view-mode-hook (lambda () (require 'pdf-tools)))

(with-eval-after-load 'pdf-tools
  (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-width))

(when (boundp 'crafted-latex-use-pdf-tools)
  (customize-set-variable 'crafted-latex-use-pdf-tools t))

(provide 'crafted-pdf-reader)
