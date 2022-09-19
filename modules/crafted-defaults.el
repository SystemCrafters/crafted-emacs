;;; crafted-defaults.el -*- lexical-binding: t; -*-

;; Copyright (C) 2022
;; SPDX-License-Identifier: MIT

;; Author: System Crafters Community

;; Commentary

;; General sane defaults

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

;; Use "y" and "n" to confirm/negate prompt instead of "yes" and "no"
;; Using `advice' here to make it easy to reverse in custom
;; configurations with `(advice-remove 'yes-or-no-p #'y-or-n-p)'
;;
;; N.B. Emacs 28 has a variable for using short answers, which should
;; be preferred if using that version or higher.
(if (boundp 'use-short-answers)
    (setq use-short-answers t)
  (advice-add 'yes-or-no-p :override #'y-or-n-p))

;; Turn on recentf mode
(add-hook 'after-init-hook #'recentf-mode)
(customize-set-variable 'recentf-save-file
                        (expand-file-name "recentf" crafted-config-var-directory))

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
(add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)

;; Enable savehist-mode for command history
(savehist-mode 1)

;; Keep state files in `crafted-config-var-directory' by default
;; we use `with-eval-after-load' to only affect what is being used.
;;
;; Note that this can introduce issues depending on how each module
;; works. Like, for example, if the module reads those files during
;; load it may happen that it reads the file on its default location
;; before the path is changed (because this code runs after-load,
;; and user customization is run after all of crafted-emacs is loaded)
;;
;; So, each variable needs some thought on how/when to set it,
;; while also trying to not set variables for modules the user
;; is not loading / using.

;; Enable the sensible path defaults
(defcustom crafted-folders t
  "Non-nil enabled 'sensible folder layout' behaviour."
  :type 'boolean
  :group 'crafted)

(defun crafted-defaults--sensible-path
    (root varname name)
  "Sets the VARNAME to a path named NAME inside ROOT.
   But only if `crafted-folders' is enabled (`t').

  For example (crafted-config-var-directory 'savehist-file \"history\")
  Will set `savehist-file' to, ie, ~/.config/crafted-emacs/var/history"
  (if-let ((path (expand-file-name name root))
           (crafted-folders))
      (customize-set-variable varname path)
    ))

(crafted-defaults--sensible-path crafted-config-var-directory
                                 'savehist-file "history")

(with-eval-after-load 'saveplace
  (crafted-defaults--sensible-path crafted-config-var-directory
                                    'save-place-file "places"))

(with-eval-after-load 'bookmark
  (crafted-defaults--sensible-path crafted-config-var-directory
                                    'bookmark-default-file "bookmarks"))

(with-eval-after-load 'tramp
  (crafted-defaults--sensible-path crafted-config-var-directory
                                    'tramp-persistency-file-name
                                    "tramp"))

(with-eval-after-load 'org-id
  (crafted-defaults--sensible-path crafted-config-var-directory
                                    'org-id-locations-file
                                    "org-id-locations"))

(with-eval-after-load 'nsm
  (crafted-defaults--sensible-path crafted-config-var-directory
                                    'nsm-settings-file
                                    "network-security.data"))

(with-eval-after-load 'project
  (crafted-defaults--sensible-path crafted-config-var-directory
                                    'project-list-file
                                    "projects"))

(provide 'crafted-defaults)
;;; crafted-defaults.el ends here
