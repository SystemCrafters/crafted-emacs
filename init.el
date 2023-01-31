;;; init.el -*- lexical-binding: t; -*-

;; Profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Crafted Emacs loaded in %s."
                     (emacs-init-time))))

;; Add the modules folder to the load path
;; (when-let ((modules (expand-file-name "modules/"))
;;            (modules-p (file-directory-p modules)))
;;   (message (concat "adding modules to load-path " modules))
;;   (add-to-list 'load-path modules))

(let ((modules (expand-file-name "./modules/" user-emacs-directory)))
  (when (file-directory-p modules)
    (message (concat "adding modules to load-path: " modules))
    (add-to-list 'load-path modules)))

;; Add the user's custom-modules to the top of the load-path
;; so any user custom-modules take precedence.
(when-let ((custom-modules (expand-file-name "custom-modules/"))
           (custom-modules-p (file-directory-p custom-modules)))
  (setq load-path
        (append (let ((load-path (list))
                      (default-directory custom-modules))
                  (add-to-list 'load-path custom-modules)
                  (normal-top-level-add-subdirs-to-load-path)
                  load-path)
                load-path)))

;; Set default coding system (especially for Windows)
(set-default-coding-systems 'utf-8)

(defun crafted-ensure-package (package &optional args)
  "Ensure that PACKAGE is installed on the system, either via
package.el or Guix depending on the value of
`crafted-prefer-guix-packages'."
  (if crafted-prefer-guix-packages
      (unless (featurep package)
        (message "Package '%s' does not appear to be installed by Guix: " package))
    (package-installed-p package)))

;; Check the system used
(defconst ON-LINUX   (eq system-type 'gnu/linux))
(defconst ON-MAC     (eq system-type 'darwin))
(defconst ON-BSD     (or ON-MAC (eq system-type 'berkeley-unix)))
(defconst ON-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))

;; Load the custom file if it exists
(when (and custom-file
           (file-exists-p custom-file))
  (load custom-file nil 'nomessage))

;; When writing crafted-modules, insert header from skeleton
(auto-insert-mode)
(with-eval-after-load "autoinsert"
  (define-auto-insert
    (cons (concat (expand-file-name crafted-emacs-home) "modules/crafted-.*\\.el")
          "Crafted Emacs Lisp Skeleton")
    '("Crafted Emacs Module Description: "
      ";;;; " (file-name-nondirectory (buffer-file-name)) " --- " str
      (make-string (max 2 (- 80 (current-column) 27)) ?\s)
      "-*- lexical-binding: t; -*-" '(setq lexical-binding t)
      "

;; Copyright (C) " (format-time-string "%Y") "
;; SPDX-License-Identifier: MIT

;; Author: System Crafters Community

;;; Commentary:

;; " _ "

;;; Code:

(provide '"
      (file-name-base (buffer-file-name))
      ")
;;; " (file-name-nondirectory (buffer-file-name)) " ends here\n")))

;;   The file used by the Customization UI to store value-setting
;; forms in a customization file, rather than at the end of the
;; `init.el' file, is called `custom.el' in Crafted Emacs. The file
;; is loaded after this `init.el' file, and after the user `config.el'
;; file has been loaded. Any variable values set in the user
;; `config.el' will be overridden with the values set with the
;; Customization UI and saved in the custom file.

(require 'crafted-startup)
(unless crafted-startup-inhibit-splash
  (setq initial-buffer-choice #'crafted-startup-screen))

(let ((crafted-info-dir (expand-file-name "docs/dir")))
  (when (file-exists-p crafted-info-dir)
    (require 'info)
    (info-initialize)
    (push (file-name-directory crafted-info-dir) Info-directory-list)))
