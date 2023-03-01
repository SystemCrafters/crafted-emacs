;;; crafted-pdf-reader-config.el -*- lexical-binding: t; -*-

;; Copyright (C) 2022
;; SPDX-License-Identifier: MIT

;; Author: System Crafters Community

;;; Commentary:

;; Configuration for the pdf-tools package to read pdf files

;;; Code:

(when (featurep 'pdf-tools)
  (add-hook 'doc-view-mode-hook (lambda () (require 'pdf-tools)))

  (with-eval-after-load 'pdf-tools
    (pdf-tools-install)
    (setq-default pdf-view-display-size 'fit-width)))

(provide 'crafted-pdf-reader-config)
;;; crafted-pdf-reader-config.el ends here
