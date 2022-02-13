;;; rational-files-management.el -*- lexical-binding: t; -*-

(require 'dired)

;; Always delete and copy recursively
(setq dired-recursive-deletes 'always)
(setq dired-recursive-copies 'always)

;; Create destinations directories if needed
(setq dired-create-destination-dirs 'always)

;; Refresh buffers automatically
(add-hook 'dired-mode-hook 'auto-revert-mode)

;; Display directories first
(setq dired-listing-switches "-vAlahF --group-directories-first")

(provide 'rational-files-management)
;;; rational-files-management ends here
