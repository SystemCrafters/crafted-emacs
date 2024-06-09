;;; early-init.el --- straight.el Example -*- lexical-binding: t; -*-

;; Copyright (C) 2023
;; SPDX-License-Identifier: MIT

;; Author: System Crafters Community

;;; Commentary:

;; Example early-init.el for using the straight.el package manager.
;; This does *not* load crafted-early-init-config
;; (which would normally bootstrap package.el).

;;; Code:

;;; Bootstrap straight.el
(setq package-enable-at-startup nil)

;; See https://github.com/radian-software/straight.el#getting-started
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;;; Set up crafted-package
;; Configure crafted-emacs to use straight as package manager.
;; See `(info "(crafted-emacs)Using alternate package managers")'
(load (expand-file-name "../../modules/crafted-package-config"
                        user-emacs-directory))

(setq crafted-package-system 'straight)
(setq crafted-package-installer #'straight-use-package)
(setq crafted-package-installed-predicate #'straight--installed-p)

;;; _
(provide 'early-init)
;;; early-init.el ends here
