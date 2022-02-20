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

;; Parentheses
(electric-pair-mode 1) ; auto-insert matching bracket
(show-paren-mode 1)    ; turn on paren match highlighting

;; Highlight current line
(add-hook 'prog-mode-hook 'hl-line-mode)

(provide 'rational-editing)
;;; rational-editing.el ends here
