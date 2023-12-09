;;; early-init.el --- Elpaca Example Config -*- lexical-binding: t; -*-

;; Copyright (C) 2023
;; SPDX-License-Identifier: MIT

;; Author: System Crafters Community

;;; Commentary:

;; Example early-init.el for using the Elpaca package manager.
;; This does *not* load crafted-early-init-config
;; (which would normally bootstrap package.el).

;;; Code:

;;; Bootstrap elpaca
(setq package-enable-at-startup nil)

(defvar elpaca-installer-version 0.5)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil
                              :files (:defaults (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))

(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (call-process "git" nil buffer t "clone"
                                       (plist-get order :repo) repo)))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;;; Set up crafted-package
;; Configure crafted-emacs to use straight as package manager.
;; See `(info "(crafted-emacs)Using alternate package managers")'
(load (expand-file-name "../../modules/crafted-package-config"
                        user-emacs-directory))

(setq crafted-package-system 'elpaca)
(setq crafted-package-installer #'elpaca-try)
(setq crafted-package-installed-predicate #'elpaca-installed-p)

;; Enable :elpaca use-package keyword.
(elpaca elpaca-use-package
        (elpaca-use-package-mode))

;; Wait for elpaca
(elpaca-wait)

;; If on Windows, disable symlinks
;; (Symlinks require admin permissions before Windows 11)
(when (member system-type '(windows-nt ms-dos))
  (elpaca-no-symlink-mode))

;;; _
(provide 'early-init)
;;; early-init.el ends here
