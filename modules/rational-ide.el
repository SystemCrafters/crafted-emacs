;; rational-ide.el -*- lexical-binding: t; -*-

;; Copyright (C) 2022
;; SPDX-License-Identifier: MIT

;; Author: System Crafters Community

;; Commentary

;; Eglot configuration

;;; Code:


;; Install dependencies
(rational-package-install-package 'xref)
(rational-package-install-package 'project)
(rational-package-install-package 'eldoc)
(rational-package-install-package 'yasnippet)
(rational-package-install-package 'yasnippet-snippets)



(rational-package-install-package 'eglot)


;; hooks
(add-hook 'prog-mode-hook 'eglot-ensure)
(add-hook 'prog-mode-hook 'yas-minor-mode)

(add-hook 'LaTeX-mode-hook 'eglot-ensure)
(add-hook 'LaTeX-mode-hook 'yas-minor-mode)
