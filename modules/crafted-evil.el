;;; crafted-evil.el --- Evil mode configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2022
;; SPDX-License-Identifier: MIT

;; Author: System Crafters Community

;;; Commentary:

;; Evil mode configuration, for those who prefer `Vim' keybindings.

;;; Code:

(defgroup crafted-evil '()
  "Vim-like related configuration for Crafted Emacs."
  :tag "Crafted Evil"
  :group 'crafted)

;; Define configuration variables
(define-obsolete-variable-alias
  'rational-evil-discourage-arrow-keys
  'crafted-evil-discourage-arrow-keys
  "1")
(defcustom crafted-evil-discourage-arrow-keys nil
  "When non-nil, prevent navigation with the arrow keys in Normal state."
  :group 'crafted-evil
  :type 'boolean)

(define-obsolete-variable-alias
  'rational-evil-vim-muscle-memory
  'crafted-evil-vim-muscle-memory
  "1")
(defcustom crafted-evil-vim-muscle-memory nil
  "When non-nil, let evil mode take some of the default keybindings, in order to make a more familiar Vim experience."
  :group 'crafted-evil
  :type 'boolean)

;; Install dependencies
(crafted-package-install-package 'evil)
(crafted-package-install-package 'evil-collection)
(crafted-package-install-package 'evil-nerd-commenter)

;; Turn on undo-tree globally
(when (< emacs-major-version 28)
  (crafted-package-install-package 'undo-tree)
  (global-undo-tree-mode))

;; Set some variables that must be configured before loading the package
(customize-set-variable 'evil-want-integration t)
(customize-set-variable 'evil-want-keybinding nil)
(customize-set-variable 'evil-want-C-i-jump nil)
(customize-set-variable 'evil-respect-visual-line-mode t)
;; C-h is backspace in insert state
(customize-set-variable 'evil-want-C-h-delete t)
(if (< emacs-major-version 28)
  (customize-set-variable 'evil-undo-system 'undo-tree)
  (customize-set-variable 'evil-undo-system 'undo-redo))

(when crafted-evil-vim-muscle-memory
    (customize-set-variable 'evil-want-C-i-jump t)
    (customize-set-variable 'evil-want-Y-yank-to-eol t)
    (customize-set-variable 'evil-want-fine-undo t))

;; Load Evil and enable it globally
(require 'evil)
(evil-mode 1)

;; Make evil search more like vim
(evil-select-search-module 'evil-search-module 'evil-search)

;; Turn on Evil Nerd Commenter
(evilnc-default-hotkeys)

;; Make C-g revert to normal state
(define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)

;; Rebind `universal-argument' to 'C-M-u' since 'C-u' now scrolls the buffer
(global-set-key (kbd "C-M-u") 'universal-argument)

;; Use visual line motions even outside of visual-line-mode buffers
(evil-global-set-key 'motion "j" 'evil-next-visual-line)
(evil-global-set-key 'motion "k" 'evil-previous-visual-line)

(defun crafted-evil/discourage-arrow-keys ()
  (interactive)
  (message "Use HJKL keys instead!"))

(when crafted-evil-discourage-arrow-keys
  ;; Disable arrow keys in normal and visual modes
  (define-key evil-normal-state-map (kbd "<left>") 'crafted-evil/discourage-arrow-keys)
  (define-key evil-normal-state-map (kbd "<right>") 'crafted-evil/discourage-arrow-keys)
  (define-key evil-normal-state-map (kbd "<down>") 'crafted-evil/discourage-arrow-keys)
  (define-key evil-normal-state-map (kbd "<up>") 'crafted-evil/discourage-arrow-keys)
  (evil-global-set-key 'motion (kbd "<left>") 'crafted-evil/discourage-arrow-keys)
  (evil-global-set-key 'motion (kbd "<right>") 'crafted-evil/discourage-arrow-keys)
  (evil-global-set-key 'motion (kbd "<down>") 'crafted-evil/discourage-arrow-keys)
  (evil-global-set-key 'motion (kbd "<up>") 'crafted-evil/discourage-arrow-keys))

;; Make sure some modes start in Emacs state
;; TODO: Split this out to other configuration modules?
(dolist (mode '(custom-mode
                eshell-mode
                term-mode))
  (add-to-list 'evil-emacs-state-modes mode))

(evil-collection-init)

(provide 'crafted-evil)
;;; crafted-evil.el ends here
