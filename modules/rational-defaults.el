;;; rational-defaults.el -*- lexical-binding: t; -*-

;; Revert Dired and other buffers
(setq global-auto-revert-non-file-buffers t)

;; Revert buffers when the underlying file has changed
(global-auto-revert-mode 1)

;; Use spaces instead of tabs
(setq-default indent-tabs-mode nil)

;; Use "y" and "n" to confirm/negate prompt instead of "yes" and "no"
(fset 'yes-or-no-p 'y-or-n-p)

;; Turn on recentf mode
(recentf-mode 1)

;; Do not saves duplicates in kill-ring
(setq kill-do-not-save-duplicates t)

;; Make scrolling less stuttered
(setq auto-window-vscroll nil)
(setq fast-but-imprecise-scrolling t)
(setq scroll-conservatively 101)
(setq scroll-margin 0)
(setq scroll-preserve-screen-position t)

;; Make shebang (#!) file executable when saved
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

(provide 'rational-defaults)
