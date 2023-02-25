;;;; crafted-mastering-emacs-config.el --- Configuration based on the book Mastering Emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2022
;; SPDX-License-Identifier: MIT

;; Author: System Crafters Community

;;; Commentary:

;; Configuration based on the book "Mastering Emacs" by Mickey Peterson.
;; See: https://masteringemacs.org/r/systemcrafters if you are
;; interested in buying the book as it helps the SystemCrafters
;; YouTube channel.
;;
;; This module configures a nice base Emacs without necessarily
;; installing additional packages.  Two packages which could be used
;; are `dumb-jump' and `hydra'.  If installed, they are configured
;; here.
;;
;; For users with Emacs prior to version 28, `icomplete-mode' is
;; enabled with an method to install a vertical mode to make it work
;; more like `fido-vertical-mode' from Emacs 28.  The defun is only
;; provided for Emacsen prior to 28.

;;; Code:

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
(keymap-set global-map "M-#" #'dictionary-lookup-definition)

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
(keymap-global-set "<remap> <list-buffers>" #'ibuffer-list-buffers)

;; Turning on `winner-mode' provides an "undo" function for resetting
;; your window layout.  This functionality is bound to control left
;; (`winner-undo') and control right (`winner-redo')
(winner-mode 1)

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
  (keymap-set dumb-jump-mode-map "C-M-y" #'dumb-jump-hydra/body))

;; use xref
(with-eval-after-load 'dumb-jump
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

;; use completion system instead of popup window
(customize-set-variable 'xref-show-definitions-function
                        #'xref-show-definitions-completing-read)

(provide 'crafted-mastering-emacs-config)
;;; crafted-mastering-emacs-config.el ends here
