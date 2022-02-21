;;; rational-editing.el -*- lexical-binding: t; -*-

;; Copyright (C) 2022
;; SPDX-License-Identifier: MIT

;; Author: System Crafters Community

;; Commentary

;; Editing text configuration.

;;; Code:

(straight-use-package 'ws-butler)
(straight-use-package 'evil-nerd-commenter)

;; Set up ws-butler for trimming whitespace and line endings
(add-hook 'text-mode-hook 'ws-butler-mode)
(add-hook 'prog-mode-hook 'ws-butler-mode)

;; Set a global binding for better line commenting/uncommenting
(global-set-key (kbd "M-/") 'evilnc-comment-or-uncomment-lines)

;; parentheses
(electric-pair-mode 1) ; auto-insert matching bracket
(show-paren-mode 1)    ; turn on paren match highlighting

;; When text is selected, overwrite it by typing
(delete-selection-mode 1)

;; easilyNavigateCamelCase
(global-subword-mode 1)

(provide 'rational-editing)
;;; rational-editing.el ends here
