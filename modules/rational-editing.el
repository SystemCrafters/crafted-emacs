;;; rational-editing.el -*- lexical-binding: t; -*-

;; Copyright (C) 2022
;; SPDX-License-Identifier: MIT

;; Author: System Crafters Community

;; Commentary

;; Editing text configuration.

;;; Code:

(rational-install-package 'evil-nerd-commenter)

;; Set a global binding for better line commenting/uncommenting
(define-key global-map [remap comment-dwim] #'evilnc-comment-or-uncomment-lines)

;; whitespace
(customize-set-variable 'whitespace-style
                        '(face tabs empty trailing tab-mark indentation::space))
(customize-set-variable 'whitespace-action '(cleanup auto-cleanup))
(add-hook 'prog-mode-hook #'whitespace-mode)
(add-hook 'text-mode-hook #'whitespace-mode)

;; parentheses
(electric-pair-mode 1) ; auto-insert matching bracket
(show-paren-mode 1)    ; turn on paren match highlighting

(provide 'rational-editing)
;;; rational-editing.el ends here
