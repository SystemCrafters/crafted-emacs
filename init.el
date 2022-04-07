;;; init.el -*- lexical-binding: t; -*-

;; Profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Rational Emacs loaded in %s."
                     (emacs-init-time))))

;; Define customization group for Rational Emacs.
(defgroup rational '()
  "A sensible starting point for hacking your own Emacs configuration."
  :tag "Rational Emacs"
  :link '(url-link "https://github.com/SystemCrafters/rational-emacs")
  :group 'emacs)

;;; package configuration
(defun rational-package-archives-stale-p ()
  "Check on-disk archives last modified date to see if it is stale.

Return non-nil if the on-disk cache is older than one day or
`nil' otherwise."
  (let ((today (time-to-days (current-time)))
        result)
   (dolist (archive package-archives result)
     (let* ((archive-name (expand-file-name (format "archives/%s/archive-contents" archive)
                                            package-user-dir))
            (archive-modified-date (time-to-days
                                    (file-attribute-modification-time
                                     (file-attributes archive-name)))))
       (when (time-less-p archive-modified-date today)
         (setq result t))))))

(defmacro rational-package-install-package (package)
  "Only install the package if it is not already installed."
  `(unless (package-installed-p ,package) (package-install ,package)))

;; Only use package.el if it is enabled. The user may have turned it
;; off in their `early-config.el' file, so respect their wishes if so.
(when package-enable-at-startup
 ;; initialize package, but don't activate any packages, use `require'
 ;; to load them manually to avoid activating unused but installed
 ;; packages.
  (cond
   ((eq system-type 'darwin)
    ;; For some reason, on a Mac, the load-path is not initialized
    ;; with installed packages unless packages are also activated in
    ;; Emacs 28.
    (package-initialize))
   (t
    (package-initialize t)))

 (require 'seq)
 ;; Only refresh package contents once per day on startup, or if the
 ;; `package-archive-contents' has not been initialized. If Emacs has
 ;; been running for a while, user will need to manually run
 ;; `package-refresh-contents' before calling `package-install'.
 (when (or (seq-empty-p package-archive-contents)
           (rational-package-archives-stale-p))
   (package-refresh-contents)))

;; Add the modules folder to the load path
(add-to-list 'load-path (expand-file-name "modules/" user-emacs-directory))

;; Set default coding system (especially for Windows)
(set-default-coding-systems 'utf-8)
(customize-set-variable 'visible-bell 1)  ; turn off beeps, make them flash!
(customize-set-variable 'large-file-warning-threshold 100000000) ;; change to ~100 MB


(defun rational-ensure-package (package &optional args)
  "Ensure that PACKAGE is installed on the system, either via
package.el or Guix depending on the value of
`rational-prefer-guix-packages'."
  (if rational-prefer-guix-packages
      (unless (featurep package)
        (message "Package '%s' does not appear to be installed by Guix!"))
    (rational-install-package package)))

;; Check the system used
(defconst ON-LINUX   (eq system-type 'gnu/linux))
(defconst ON-MAC     (eq system-type 'darwin))
(defconst ON-BSD     (or ON-MAC (eq system-type 'berkeley-unix)))
(defconst ON-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))

;; Find the user configuration file
(defvar rational-config-file (expand-file-name "config.el" rational-config-path)
  "The user's configuration file.")

;; Defines the user configuration var and etc folders
;; and ensure they exist.
(defvar rational-config-etc-directory (expand-file-name "etc/" rational-config-path)
  "The user's configuration etc/ folder.")
(defvar rational-config-var-directory (expand-file-name "var/" rational-config-path)
  "The user's configuration var/ folder.")

(mkdir rational-config-etc-directory t)
(mkdir rational-config-var-directory t)

;; Load the user configuration file if it exists
(when (file-exists-p rational-config-file)
  (load rational-config-file nil 'nomessage))

;; When writing rational-modules, insert header from skeleton
(auto-insert-mode)
(with-eval-after-load "autoinsert"
  (define-auto-insert
    (cons (concat (expand-file-name user-emacs-directory) "modules/rational-.*\\.el")
          "Rational Emacs Lisp Skeleton")
    '("Rational Emacs Module Description: "
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
;; `init.el' file, is called `custom.el' in Rational Emacs. The file
;; is loaded after this `init.el' file, and after the user `config.el'
;; file has been loaded. Any variable values set in the user
;; `config.el' will be overridden with the values set with the
;; Customization UI and saved in the custom file.
(customize-set-variable 'custom-file
  (expand-file-name "custom.el" rational-config-path))

;; The custom file will only be loaded if `rational-load-custom-file'
;; is set to a non-nil value in the user's `config.el'.
(when rational-load-custom-file
  (load custom-file t))

;; Make GC pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))
