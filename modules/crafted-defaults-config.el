;;; crafted-defaults.el -*- lexical-binding: t; -*-

;; Copyright (C) 2022
;; SPDX-License-Identifier: MIT

;; Author: System Crafters Community

;; Commentary

;; General Crafted Emacs endorsed defaults

;;; Code:

;; Revert Dired and other buffers
(customize-set-variable 'global-auto-revert-non-file-buffers t)

;; Revert buffers when the underlying file has changed
(global-auto-revert-mode 1)

;; Typed text replaces the selection if the selection is active,
;; pressing delete or backspace deletes the selection.
(delete-selection-mode)

;; Use spaces instead of tabs
(setq-default indent-tabs-mode nil)

;; Turn on recentf mode
(add-hook 'after-init-hook #'recentf-mode)

;; Do not save duplicates in kill-ring
(customize-set-variable 'kill-do-not-save-duplicates t)

;; Make scrolling less stuttered
(setq auto-window-vscroll nil)
(customize-set-variable 'fast-but-imprecise-scrolling t)
(customize-set-variable 'scroll-conservatively 101)
(customize-set-variable 'scroll-margin 0)
(customize-set-variable 'scroll-preserve-screen-position t)

;; Better support for files with long lines
(setq-default bidi-paragraph-direction 'left-to-right)
(setq-default bidi-inhibit-bpa t)
(global-so-long-mode 1)

;; Make shebang (#!) file executable when saved
(add-hook 'after-save-hook
          #'executable-make-buffer-file-executable-if-script-p)

;; Enable savehist-mode for command history
(savehist-mode 1)


;;; Window management
(defgroup crafted-windows '()
  "Window related configuration for Crafted Emacs."
  :tag "Crafted Windows"
  :group 'crafted)

(defcustom crafted-windows-prefix-key "C-c w"
  "Configure the prefix key for window movement bindings.

Movment commands provided by `windmove' package, `winner-mode'
also enables undo functionality if the window layout changes."
  :group 'crafted-windows
  :type 'string)

;; Turning on `winner-mode' provides an "undo" function for resetting
;; your window layout.  We bind this to `C-c w u' for winner-undo and
;; `C-c w r' for winner-redo (see below). 
(winner-mode 1)

(define-prefix-command 'crafted-windows-key-map)

(define-key 'crafted-windows-key-map (kbd "u") 'winner-undo)
(define-key 'crafted-windows-key-map (kbd "r") 'winner-redo)
(define-key 'crafted-windows-key-map (kbd "n") 'windmove-down)
(define-key 'crafted-windows-key-map (kbd "p") 'windmove-up)
(define-key 'crafted-windows-key-map (kbd "b") 'windmove-left)
(define-key 'crafted-windows-key-map (kbd "f") 'windmove-right)

(global-set-key (kbd crafted-windows-prefix-key) 'crafted-windows-key-map)


;;; Mastering Emacs inspired configuration defaults
;; Completions
(customize-set-variable 'completion-cycle-threshold 3)
(customize-set-variable 'tab-always-indent 'complete)
(customize-set-variable 'completion-category-overrides
                        '((file (styles . (partial-completion)))))
(customize-set-variable 'completions-detailed t)
(if (version< emacs-version "28")
    (icomplete-mode 1)
  (fido-vertical-mode 1))               ; fido-vertical-mode is
                                        ; available beginning in Emacs
                                        ; 28

(when (version< emacs-version "28")
  (defun crafted-mastering-emacs-use-icomplete-vertical ()
    "Install and enable icomplete-vertical-mode for Emacs versions
less than 28."
    (interactive)
    (crafted-package-install-package 'icomplete-vertical)
    (icomplete-mode 1)
    (icomplete-vertical-mode 1)))

;; Window configuration for special windows.
;; This section inspired by the article "Demystifying Emacs’s Window
;; Manager" found here:
;; https://www.masteringemacs.org/article/demystifying-emacs-window-manager
(add-to-list 'display-buffer-alist
             '("\\*Help\\*"
               (display-buffer-reuse-window display-buffer-pop-up-window)
               (inhibit-same-window . t)))

(add-to-list 'display-buffer-alist
             '("\\*Completions\\*"
               (display-buffer-reuse-window display-buffer-pop-up-window)
               (inhibit-same-window . t)
               (window-height . 10)))

;; Show dictionary definition on the left
(add-to-list 'display-buffer-alist
             '("^\\*Dictionary\\*"
               (display-buffer-in-side-window)
               (side . left)
               (window-width . 70)))

;; define a key to define the word at point.
(define-key global-map (kbd "M-#") #'dictionary-lookup-definition)

;; pop up dedicated buffers in a different window.
(customize-set-variable 'switch-to-buffer-in-dedicated-window 'pop)
;; treat manual buffer switching (C-x b for example) the same as
;; programmatic buffer switching.
(customize-set-variable 'switch-to-buffer-obey-display-actions t)

;; Turn on repeat mode to allow certain keys to repeat on the last
;; keystroke. For example, C-x [ to page backward, after pressing this
;; keystroke once, pressing repeated [ keys will continue paging
;; backward. `repeat-mode' is exited with the normal C-g, by movement
;; keys, typing, or pressing ESC three times.
(unless (version< emacs-version "28")
  (repeat-mode 1))

;; turn off forward and backward movement cycling
(customize-set-variable 'ibuffer-movement-cycle nil)
;; the number of hours before a buffer is considered "old" by
;; ibuffer.
(customize-set-variable 'ibuffer-old-time 24)
;; prefer the more full-featured built-in ibuffer for managing
;; buffers.
(global-set-key [remap list-buffers] #'ibuffer-list-buffers)


;; turn on spell checking, if available.
(with-eval-after-load 'ispell
  (when (executable-find ispell-program-name)
    (add-hook 'text-mode-hook #'flyspell-mode)
    (add-hook 'prog-mode-hook #'flyspell-prog-mode)))

(require 'hydra "hydra" :no-error)
(require 'dumb-jump "dumb-jump" :no-error)
;; add hydra to facilitate remembering the keys and actions for dumb-jump
(when (and (featurep 'hydra)
	   (featurep 'dumb-jump))
  (defhydra dumb-jump-hydra (:color blue :columns 3)
    "Dumb Jump"
    ("j" dumb-jump-go "Go")
    ("o" dumb-jump-go-other-window "Other window")
    ("e" dumb-jump-go-prefer-external "Go external")
    ("x" dumb-jump-go-prefer-external-other-window "Go external other window")
    ("i" dumb-jump-go-prompt "Prompt")
    ("l" dumb-jump-quick-look "Quick look")
    ("b" dumb-jump-back "Back"))
  ;; not a great key as a mnemonic, but easy to press quickly
  (define-key dumb-jump-mode-map (kbd "C-M-y") #'dumb-jump-hydra/body))

;; use xref
(with-eval-after-load 'dumb-jump
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

;; use completion system instead of popup window
(customize-set-variable 'xref-show-definitions-function
                        #'xref-show-definitions-completing-read)

(provide 'crafted-defaults-config)
;;; crafted-defaults-config.el ends here
