;;; rational-evil.el -*- lexical-binding: t; -*-

;; Copyright (C) 2022
;; SPDX-License-Identifier: MIT

;; Author: System Crafters Community

;; Commentary

;; Evil mode configuration, for those who prefer `vi' keybindings.

;;; Code:

;; Define configuration variables
(defcustom rational-evil-discourage-arrow-keys nil
  "When non-nil, prevent navigation with the arrow keys in Normal state."
  :group 'rational
  :type 'boolean)

;; Install dependencies
(rational-package-install-package 'evil)
(rational-package-install-package 'undo-tree)
(rational-package-install-package 'evil-collection)
(rational-package-install-package 'evil-nerd-commenter)

;; Turn on undo-tree globally
(global-undo-tree-mode)

;; Set some variables that must be configured before loading the package
(customize-set-variable 'evil-want-integration t)
(customize-set-variable 'evil-want-keybinding nil)
(customize-set-variable 'evil-want-C-i-jump nil)
(customize-set-variable 'evil-respect-visual-line-mode t)
(customize-set-variable 'evil-undo-system 'undo-tree)

;; Load Evil and enable it globally
(require 'evil)
(evil-mode 1)

;; Turn on Evil Nerd Commenter
(evilnc-default-hotkeys)

;; Make C-g revert to normal state
(define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)

;; Rebind `universal-argument' to 'C-M-u' since 'C-u' now scrolls the buffer
(global-set-key (kbd "C-M-u") 'universal-argument)

;; C-h is backspace in insert state
(define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

;; Use visual line motions even outside of visual-line-mode buffers
(evil-global-set-key 'motion "j" 'evil-next-visual-line)
(evil-global-set-key 'motion "k" 'evil-previous-visual-line)

(defun rational-evil/discourage-arrow-keys ()
  (interactive)
  (message "Use HJKL keys instead!"))

(when rational-evil-discourage-arrow-keys
  ;; Disable arrow keys in normal and visual modes
  (define-key evil-normal-state-map (kbd "<left>") 'rational-evil/discourage-arrow-keys)
  (define-key evil-normal-state-map (kbd "<right>") 'rational-evil/discourage-arrow-keys)
  (define-key evil-normal-state-map (kbd "<down>") 'rational-evil/discourage-arrow-keys)
  (define-key evil-normal-state-map (kbd "<up>") 'rational-evil/discourage-arrow-keys)
  (evil-global-set-key 'motion (kbd "<left>") 'rational-evil/discourage-arrow-keys)
  (evil-global-set-key 'motion (kbd "<right>") 'rational-evil/discourage-arrow-keys)
  (evil-global-set-key 'motion (kbd "<down>") 'rational-evil/discourage-arrow-keys)
  (evil-global-set-key 'motion (kbd "<up>") 'rational-evil/discourage-arrow-keys))

;; Make sure some modes start in Emacs state
;; TODO: Split this out to other configuration modules?
(dolist (mode '(custom-mode
                eshell-mode
                term-mode))
  (add-to-list 'evil-emacs-state-modes mode))

(evil-collection-init)

(provide 'rational-evil)
;;; rational-evil.el ends here
