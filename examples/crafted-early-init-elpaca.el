;;; crafted-early-init-elpaca.el --- Bootstrap elpaca  -*- lexical-binding: t; -*-

;; Copyright (C) 2023
;; SPDX-License-Identifier: MIT

;; Author: System Crafters Community

;;; Commentary:

;; Code to bootstrap elpaca package manager
;;
;; Load it in early-init.el like this:

;; (setq crafted-emacs-home "~/crafted-emacs")
;; (load (expand-file-name "custom-modules/crafted-early-init-elpaca"
;;                         user-emacs-directory))

;; on init.el you need to use crafted-package-install-selected-packages
;; instead of package-install-selected-packages
;;
;; Packages phase
;; ....
;; (crafted-package-install-selected-packages)
;; (elpaca-wait)
;;
;; Configuration phase

;;; Code:

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

(load (expand-file-name "modules/crafted-package-config" crafted-emacs-home))

(setq crafted-package-system 'elpaca)
(setq crafted-package-installer #'elpaca-try)
(setq crafted-package-installed-predicate #'elpaca-installed-p)
(elpaca elpaca-use-package
  ;; Enable :elpaca use-package keyword.
  (elpaca-use-package-mode))
(elpaca-wait)

;; If on Windows, disable symlinks (Symlinks require admin permissions before Windows 11)
(when (member system-type '(windows-nt ms-dos))
  (elpaca-no-symlink-mode))

;;; crafted-early-init-elpaca.el ends here
