;;; crafted-contrib-config.el --- Emacs Lisp contribution configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2023
;; SPDX-License-Identifier: MIT

;; Author: System Crafters Community

;;; Commentary:

;; Configuration that assists core/package Emacs Lisp contribution.

;;; Code:

(when (locate-library "aggressive-indent")
  (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode))

(provide 'crafted-contrib-config)
;;; crafted-contrib-config.el ends here
