;;; crafted-evil-config.el --- Evil mode configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2023
;; SPDX-License-Identifier: MIT

;; Author: System Crafters Community

;;; Commentary:

;; Evil mode configuration, for those who prefer `Vim' keybindings.

;;; Code:

;; Turn on undo-tree globally for version older than 28.  Use
;; undo-redo for Emacs 28+
(when (and (version< emacs-version "28")
           (locate-library "undo-tree"))
  (global-undo-tree-mode))

;; Set some variables that must be configured before loading the package
(customize-set-variable 'evil-want-integration t)
(customize-set-variable 'evil-want-keybinding nil)
(customize-set-variable 'evil-want-C-i-jump nil)
(customize-set-variable 'evil-respect-visual-line-mode t)
;; C-h is backspace in insert state
(customize-set-variable 'evil-want-C-h-delete t)
(if (version< emacs-version "28")
  (customize-set-variable 'evil-undo-system 'undo-tree)
  (customize-set-variable 'evil-undo-system 'undo-redo))

(defun crafted-evil-vim-muscle-memory ()
  "Make a more familiar Vim experience.

Take some of the default keybindings for evil mode."
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
(keymap-set evil-insert-state-map "C-g" 'evil-normal-state)

;; Rebind `universal-argument' to 'C-M-u' since 'C-u' now scrolls the buffer
(keymap-global-set "C-M-u" 'universal-argument)

;; Use visual line motions even outside of visual-line-mode buffers
(evil-global-set-key 'motion "j" 'evil-next-visual-line)
(evil-global-set-key 'motion "k" 'evil-previous-visual-line)

(defun crafted-evil-discourage-arrow-keys ()
  "Turn on a message to discourage use of arrow keys.

Rebinds the arrow keys to display a message instead."

  (defun crafted-evil/discourage-arrow-keys ()
    (message "Use HJKL keys instead!"))

  ;; Disable arrow keys in normal and visual modes
  (keymap-set evil-normal-state-map "<left>"  'crafted-evil/discourage-arrow-keys)
  (keymap-set evil-normal-state-map "<right>" 'crafted-evil/discourage-arrow-keys)
  (keymap-set evil-normal-state-map "<down>"  'crafted-evil/discourage-arrow-keys)
  (keymap-set evil-normal-state-map "<up>"    'crafted-evil/discourage-arrow-keys)
  (evil-global-set-key 'motion      (kbd "<left>")  'crafted-evil/discourage-arrow-keys)
  (evil-global-set-key 'motion      (kbd "<right>") 'crafted-evil/discourage-arrow-keys)
  (evil-global-set-key 'motion      (kbd "<down>")  'crafted-evil/discourage-arrow-keys)
  (evil-global-set-key 'motion      (kbd "<up>")    'crafted-evil/discourage-arrow-keys))

;; Make sure some modes start in Emacs state
;; TODO: Split this out to other configuration modules?
(dolist (mode '(custom-mode
                eshell-mode
                term-mode))
  (add-to-list 'evil-emacs-state-modes mode))

;;; evil-collection or some crafted defaults
(if (locate-library "evil-collection")
    ;; if the user has `evil-collection' installed, let it take care of
    ;; keybindings
    (evil-collection-init)
  ;; Otherwise load up a few keybindings for other crafted modules
  ;; as needed
  ((with-eval-after-load 'crafted-completion-config
     (when (featurep 'vertico) ;; only if the user actually uses vertico
       (keymap-set vertico-map "C-j" 'vertico-next)
       (keymap-set vertico-map "C-k" 'vertico-previous)
       (keymap-set vertico-map "M-h" 'vertico-directory-up)))

   (with-eval-after-load 'crafted-speedbar-config
     ;;edit/open file under point
     (keymap-set speedbar-mode-map "<return>" 'speedbar-edit-line)
     ;;toggle thing at point
     (keymap-set speedbar-mode-map "<tab>" 'speedbar-toggle-line-expansion)
     ;;evaluate as elisp if file at point is elisp, I dont use this too much but might be useful
     (keymap-set speedbar-mode-map "L" 'speedbar-item-load)
     ;;temporarly change mode in speedbar to "buffer-switching mode".
     ;;useful for quickly switching to an open buffer
     (keymap-set speedbar-mode-map "b" 'speedbar-switch-to-quick-buffers))))

(provide 'crafted-evil-config)
;;; crafted-evil-config.el ends here
