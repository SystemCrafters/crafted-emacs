;;;; crafted-mastering-emacs.el --- Configuration based on the book Mastering Emacs  -*- lexical-binding: t; -*-

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
;; installing additional packages.  Enabling `dumb-jump' will install
;; the package and configure it.  Enabling `hydra' will install the
;; package and use it, if it is available, when configuring
;; `dumb-jump'.  See the appropriate defuns below.

;;; Code:

;; Completions
(customize-set-variable 'completion-cycle-threshold 3)
(customize-set-variable 'tab-always-indent 'complete)
(customize-set-variable 'completion-category-overrides
                        '((file (styles . (partial-completion)))))
(customize-set-variable 'completions-detailed t)
(fido-vertical-mode 1)

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

;; Turning on `winner-mode' provides an "undo" function for resetting
;; your window layout.  This functionality is bound to control left
;; (`winner-undo') and control right (`winner-redo')
(winner-mode 1)

;; turn on spell checking, if available.
(when (executable-find ispell-program-name)
  (add-hook 'text-mode-hook #'flyspell-mode)
  (add-hook 'prog-mode-hook #'flyspell-prog-mode))

(defun enable-hydra-package ()
  "Install the hydra package."
  (interactive)
  (crafted-package-install-package 'hydra))

(defun enable-dumb-jump-package ()
  "Install and configure dumb-jump package.

Uses ripgrep, TheSilverSearcher(ag), or grep, however, the
slowest is grep.  Prefer to install ripgrep or ag to improve
performance."
  (interactive)
  (crafted-package-install-package 'dumb-jump)

  ;; add hydra to facilitate remembering the keys and actions.
  (when (crafted-package-installed-p 'hydra)
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
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)

  ;; use completion system instead of popup window
  (customize-set-variable 'xref-show-definitions-function
                          #'xref-show-definitions-completing-read))

(provide 'crafted-mastering-emacs)
;;; crafted-mastering-emacs.el ends here
