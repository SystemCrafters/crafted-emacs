;;; init.el -*- lexical-binding: t; -*-

;; Profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Rational Emacs loaded in %s." (emacs-init-time))))

;; Save customizing settings in a separate file
(setq custom-file (locate-user-emacs-file "emacs-custom.el"))
(load custom-file 'noerror)

;; Add the modules folder to the load path
(add-to-list 'load-path (expand-file-name "modules/" user-emacs-directory))

;; Set default coding system (especially for Windows)
(set-default-coding-systems 'utf-8)

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
  (load bootstrap-file nil 'nomessage))

(defun rational-ensure-package (package &optional args)
  "Ensure that PACKAGE is installed on the system, either via
straight.el or Guix depending on the value of
`rational-prefer-guix-packages'."
  (if rational-prefer-guix-packages
      (unless (featurep package)
        (message "Package '%s' does not appear to be installed by Guix!"))
    (straight-use-package package)))

;; Find the user configuration file
(defvar rational-config-file (expand-file-name "config.el" rational-config-path)
  "The user's configuration file.")

;; Load the user configuration file if it exists
(load rational-config-file 'noerror 'nomessage)

;; Make GC pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))
