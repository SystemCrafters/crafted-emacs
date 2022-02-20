;;;; rational-emacs.el --- Rational Emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2022
;; SPDX-License-Identifier: MIT

;; Author: System Crafters Community

;; Commentary:

;; A sensible starting point for hacking your own Emacs configuration
;;
;; Proives early-init and init functions to be added to your own
;; `early-init.el' and `init.el' files.

;;; Code:

(defgroup rational '()
  "A sensible starting point for hacking your own Emacs configuration."
  :group 'emacs)

;; Find the user configuration path
;; In order do these checks:
;; * using chemacs?
;; ** yes, and have specified a location with the RATIONAL_EMACS_HOME
;;    environment variable
;; ** yes, but no environment variable, assume the rational-emacs
;;    folder in the profile
;; * use RATIONAL_EMACS_HOME environment variable
;; * XDG_CONFIG_HOME or the path .config/rational-emacs
;;   exists. XDG_CONFIG_HOME usually defaults to $HOME/.config/, so
;;   these are the same thing
;; * use HOME environment variable
(defvar rational-emacs-config-path
  (cond
   ((featurep 'chemacs)
    (if (getenv  "RATIONAL_EMACS_HOME")
        (expand-file-name (getenv "RATIONAL_EMACS_HOME"))
      (expand-file-name "rational-emacs" user-emacs-directory)))
   ((getenv "RATIONAL_EMACS_HOME") (expand-file-name (getenv "RATIONAL_EMACS_HOME")))
   ((or (getenv "XDG_CONFIG_HOME") (file-exists-p (expand-file-name ".config/rational-emacs" (getenv "HOME"))))
    (if (getenv "XDG_CONFIG_HOME")
    (expand-file-name "rational-emacs" (getenv "XDG_CONFIG_HOME"))
      (expand-file-name ".config/rational-emacs" (getenv "HOME"))))
   ((getenv "HOME") (expand-file-name ".rational-emacs" (getenv "HOME"))))
  "The user's rational-emacs configuration path.")

;; Find the user configuration file
(defvar rational-emacs-config-file (expand-file-name "config.el" rational-emacs-config-path)
  "The user's configuration file.")

(defun rational-emacs-using-guix-emacs-p ()
  "Verifies if the running Emacs executable is under the `/gnu/store/' path."
  (unless (or (equal system-type 'ms-dos)
              (equal system-type 'windows-nt))
    ;; Since there is no windows implementation of guix
    (string-prefix-p "/gnu/store/"
                     (file-truename
                      (executable-find
                       (car command-line-args))))))

(defvar rational-emacs-prefer-guix-packages (rational-emacs-using-guix-emacs-p)
  "If t, expect packages to be installed via Guix by default.")

;; Defines the user configuration var and etc folders
;; and ensure they exists.
(defvar rational-emacs-config-etc-directory (expand-file-name "etc/" rational-emacs-config-path)
  "The user's configuration etc/ folder.")
(defvar rational-emacs-config-var-directory (expand-file-name "var/" rational-emacs-config-path)
  "The user's configuration var/ folder.")

(defun rational-emacs-ensure-package (package &optional args)
  "Ensure that PACKAGE is installed on the system, either via
straight.el or Guix depending on the value of
`rational-emacs-prefer-guix-packages'."
  (if rational-emacs-prefer-guix-packages
      (unless (featurep package)
        (message "Package '%s' does not appear to be installed by Guix!" package))
    (straight-use-package package)))

(defun rational-emacs---enable-auto-skeleton ()
  "Turn on auto-insertion of Rational Emacs Lisp skeleton.

When writing rational-modules, insert header from skeleton."
  (auto-insert-mode)
  (with-eval-after-load "autoinsert"
    (define-auto-insert
      (cons "rational-.*\\.el" "Rational Emacs Lisp Skeleton")
      '("Rational Emacs Module Description: "
      ";;;; " (file-name-nondirectory (buffer-file-name)) " --- " str
      (make-string (max 2 (- 80 (current-column) 27)) ?\s)
      "-*- lexical-binding: t; -*-" '(setq lexical-binding t)
      "

;; Copyright (C) " (format-time-string "%Y") "
;; SPDX-License-Identifier: MIT

;; Author: System Crafters Community

;; Commentary:

;; " _ "

;;; Code:

(provide '"
       (file-name-base (buffer-file-name))
      ")
;;; " (file-name-nondirectory (buffer-file-name)) " ends here\n"))))

(defun rational-emacs--init-config ()
  "Configures rational defaults for initialization."
  ;; Set default coding system (especially for Windows)
  (set-default-coding-systems 'utf-8)
  ;; turn off beeps, make them flash!
  (customize-set-variable 'visible-bell 1)
  ;; change to ~100 MB
  (customize-set-variable 'large-file-warning-threshold 100000000))

(defun rational-emacs--load-user-config ()
  "Load the user configuration file, if it exists."
  (when (file-exists-p rational-emacs-config-file)
    (load rational-emacs-config-file nil 'nomessage)))

(defun rational-emacs--bootstrap-straight ()
  "Bootstraps `straight'."
  ;; Initialize straight.el
  (defvar bootstrap-version)
  (let ((bootstrap-file
         (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
        (bootstrap-version 5))
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer
          (url-retrieve-synchronously
           "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
           'silent 'inhibit-cookies)
        (goto-char (point-max))
        (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage)))

(defun rational-emacs--startup-function ()
  "Provides feeback to user regarding Rational Emacs startup.

This function is intended to be run from `emacs-startup-hook'."
  ;; Profile Emacs startup
  (message "Rational Emacs loaded in %s."
           (format "%.2f seconds"
                   (float-time
                    (time-subtract after-init-time before-init-time)))))

(defun rational-emacs-early-init ()
  "Perform early setup and initialization for Rational Emacs.

This function is intended to be called from your own
Emacs early-init.el file."
  ;; Increase the GC threshold for faster startup
  ;; The default is 800 kilobytes.  Measured in bytes.
  (setq gc-cons-threshold (* 50 1000 1000))

  ;; Prefer loading newest compiled .el file
  (setq load-prefer-newer noninteractive)

  ;; Native compilation settings
  (when (featurep 'native-compile)
    ;; Silence compiler warnings as they can be pretty disruptive
    (setq native-comp-async-report-warnings-errors nil)

    ;; Make native compilation happens asynchronously
    (setq native-comp-deferred-compilation t)

    ;; Set the right directory to store the native compilation cache
    (add-to-list 'native-comp-eln-load-path (expand-file-name "eln-cache/" user-emacs-directory)))

  ;; Don't use package.el, we'll use straight.el instead
  (setq package-enable-at-startup nil)

  ;; Remove some unneeded UI elements (the user can turn back on anything they wish)
  (setq inhibit-startup-message t)
  (push '(tool-bar-lines . 0) default-frame-alist)
  (push '(menu-bar-lines . 0) default-frame-alist)
  (push '(vertical-scroll-bars) default-frame-alist)
  (push '(background-color . "#232635") default-frame-alist)
  (push '(foreground-color . "#FFFFFF") default-frame-alist)
  (push '(mouse-color . "white") default-frame-alist)

  ;; Make the initial buffer load faster by setting its mode to fundamental-mode
  (setq initial-major-mode 'fundamental-mode)

  (unless (file-exists-p rational-emacs-config-path)
    (mkdir rational-emacs-config-path t))

  ;; Load the early config file if it exists
  (let ((early-config-path (expand-file-name "early-config.el" rational-emacs-config-path)))
    (when (file-exists-p early-config-path)
      (load early-config-path nil 'nomessage))))

(defun rational-emacs-init ()
  "Perform setup and initialization for Rational Emacs.

This function is intended to be called from your own
Eamcs init.el file."
  (add-hook 'emacs-startup-hook #'rational-emacs--startup-function)

  ;; Add the modules folder to the load path
  (add-to-list 'load-path (expand-file-name "modules/" user-emacs-directory))

  (rational-emacs--init-config)
  (rational-emacs--bootstrap-straight)

  (mkdir rational-emacs-config-etc-directory t)
  (mkdir rational-emacs-config-var-directory t)

  (rational-emacs---enable-auto-skeleton)

  (rational-emacs--load-user-config)

  ;; Make GC pauses faster by decreasing the threshold.
  (setq gc-cons-threshold (* 2 1000 1000)))

(provide 'rational-emacs)
;;; rational-emacs.el ends here
