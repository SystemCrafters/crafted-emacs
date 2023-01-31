;;; crafted-completion-packages.el --- Completion packages  -*- lexical-binding: t; -*-

;; Copyright (C) 2022
;; SPDX-License-Identifier: MIT

;; Author: System Crafters Community

;;; Commentary:

;; Add completion packages to the list of packages to install.

;;; Code:

(add-to-list 'package-selected-packages 'cape)
(add-to-list 'package-selected-packages 'consult)
(add-to-list 'package-selected-packages 'corfu)
(add-to-list 'package-selected-packages 'corfu-terminal)
(add-to-list 'package-selected-packages 'embark)
(add-to-list 'package-selected-packages 'embark-consult)
(add-to-list 'package-selected-packages 'marginalia)
(add-to-list 'package-selected-packages 'orderless)
(add-to-list 'package-selected-packages 'vertico)


(provide 'crafted-completion-packages)
;;; crafted-completion-packages.el ends here
