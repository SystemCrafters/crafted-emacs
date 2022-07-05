;;; rational-defaults.el -*- lexical-binding: t; -*-

;; Copyright (C) 2022
;; SPDX-License-Identifier: MIT

;; Author: System Crafters Community

;; Commentary

;; General sane defaults

;;; Code:

;; Defining variable for XDG directories specifications
(defvar xdg-data-home-directory
  (if (getenv "XDG_DATA_HOME")
    (getenv "XDG_DATA_HOME")
    (expand-file-name ".local/share/")))

(defvar xdg-cache-home-directory
  (if (getenv "XDG_DATA_HOME")
    (getenv "XDG_DATA_HOME")
    (expand-file-name ".cache/")))

;; Keep emacs folder clean

;; Backup file ( init.el~ )
(customize-set-variable backup-directory-alist `(("." . ,(expand-file-name "emacs/backups/" xdg-data-home-directory))))
;; Emacs autosave files ( #init.el# )
;; The autosave feature doesn't create the directory
(make-directory (expand-file-name "emacs/auto-save/" xdg-cache-home-directory) t)
(customize-set-variable auto-save-file-name-transforms
    `((".*" ,(expand-file-name "emacs/auto-save/" xdg-cache-home-directory) t)))

;; eshell history, lastdir and aliases
(customize-set-variable eshell-directory-name 
  (expand-file-name "emacs/eshell" xdg-data-home-directory))
(customize-set-variable eshell-aliases-file 
  (expand-file-name "emacs/eshell/aliases" xdg-data-home-directory))
;; bookmark default file
(customize-set-variable bookmark-default-file 
  (expand-file-name "emacs/bookmark-default.el" xdg-data-home-directory))

;; Revert Dired and other buffers
(customize-set-variable 'global-auto-revert-non-file-buffers t)

;; Revert buffers when the underlying file has changed
(global-auto-revert-mode 1)

;; Use spaces instead of tabs
(customize-set-variable-default indent-tabs-mode nil)

;; Use "y" and "n" to confirm/negate prompt instead of "yes" and "no"
;; Using `advice' here to make it easy to reverse in custom
;; configurations with `(advice-remove 'yes-or-no-p #'y-or-n-p)'
;;
;; N.B. Emacs 28 has a variable for using short answers, which should
;; be preferred if using that version or higher.
(if (boundp 'use-short-answers)
    (customize-set-variable use-short-answers t)
  (advice-add 'yes-or-no-p :override #'y-or-n-p))

;; Turn on recentf mode
(add-hook 'after-init-hook #'recentf-mode)
(customize-set-variable 'recentf-save-file
                        (expand-file-name "recentf" rational-config-var-directory))

;; Do not saves duplicates in kill-ring
(customize-set-variable 'kill-do-not-save-duplicates t)

;; Make scrolling less stuttered
(customize-set-variable auto-window-vscroll nil)
(customize-set-variable 'fast-but-imprecise-scrolling t)
(customize-set-variable 'scroll-conservatively 101)
(customize-set-variable 'scroll-margin 0)
(customize-set-variable 'scroll-preserve-screen-position t)

;; Better support for files with long lines
(customize-set-variable-default bidi-paragraph-direction 'left-to-right)
(customize-set-variable-default bidi-inhibit-bpa t)
(global-so-long-mode 1)

;; Make shebang (#!) file executable when saved
(add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)

;; Enable savehist-mode for an command history
(savehist-mode 1)
(customize-set-variable 'savehist-file
                        (expand-file-name "history" rational-config-var-directory))

(provide 'rational-defaults)
;;; rational-defaults.el ends here
