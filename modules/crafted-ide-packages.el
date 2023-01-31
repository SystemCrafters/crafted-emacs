;; crafted-ide-packages.el -*- lexical-binding: t; -*-

;; Copyright (C) 2022
;; SPDX-License-Identifier: MIT

;; Author: System Crafters Community

;; Commentary

;; Eglot package installation for Emacs prior to version 29.

;;; Code:


(when (version< emacs-version "29")
  (add-to-list 'package-selected-packages 'eglot))

(provide 'crafted-ide-packages)
;;; crafted-ide-packages.el ends here


