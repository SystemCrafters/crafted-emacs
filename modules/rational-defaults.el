;;; rational-defaults.el -*- lexical-binding: t; -*-

;; Copyright (C) 2022
;; SPDX-License-Identifier: MIT

;; Author: System Crafters Community

;; Commentary

;; General sane defaults

;;; Code:

(straight-use-package 'ibuffer-project)

;; Revert Dired and other buffers
(customize-set-variable 'global-auto-revert-non-file-buffers t)

;; Revert buffers when the underlying file has changed
(global-auto-revert-mode 1)

;; Use spaces instead of tabs
(setq-default indent-tabs-mode nil)

;; Use "y" and "n" to confirm/negate prompt instead of "yes" and "no"
(fset 'yes-or-no-p 'y-or-n-p)

;; Turn on recentf mode
(add-hook 'after-init-hook #'recentf-mode)
(setq recentf-save-file (expand-file-name "recentf" rational-config-var-directory))

;; Do not saves duplicates in kill-ring
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
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; Enable savehist-mode for an command history
(savehist-mode 1)

;; whitespace
(customize-set-variable 'whitespace-style
                        '(face tabs empty trailing tab-mark))
(customize-set-variable 'whitespace-action '(cleanup auto-cleanup))
(global-whitespace-mode)

;; use ibuffer as a buffer switcher
(define-key global-map [remap list-buffers] #'ibuffer)

;; ibuffer configuration
(add-hook 'ibuffer-hook
          (lambda ()
            (setq ibuffer-filter-groups (ibuffer-project-generate-filter-groups))
            (unless (eq ibuffer-sorting-mode 'project-file-relative)
              (ibuffer-do-sort-by-project-file-relative))))


(provide 'rational-defaults)
;;; rational-defaults.el ends here
