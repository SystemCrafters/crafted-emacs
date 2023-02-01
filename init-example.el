;;; init.el -*- lexical-binding: t; -*-

;;; Initial phase.

;; Load the custom file if it exists.  Among other settings, this will
;; have the list `package-selected-packages', so we need to load that
;; before adding more packages.  The value of the `custom-file'
;; variable must be set appropriately, by default the value is nil.
;; This can be done here, or in the early-init.el file.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (and custom-file
           (file-exists-p custom-file))
  (load custom-file nil 'nomessage))

;; Must be loaded *BEFORE* loading the crafted-init-config file.
;; Creates a variable to hold the path to the project root for
;; crafted-emacs (ie where it was cloned to) so the modules folder can
;; be added to the load path.
(load "~/crafted-emacs/modules/crafted-emacs-home-config")

;; Adds crafted-emacs modules to the `load-path', sets up a module
;; writing template, sets the `crafted-emacs-home' variable.
(load "~/crafted-emacs/modules/crafted-init-config")

;;; Packages phase

;; Collect list of packages to install.  Do not just blindly copy this
;; list, instead think about what you need and see if there is a
;; module which provides the list of packages needed.  This phase is
;; not needed if manage the installed packages with Guix or Nix.  It
;; is also not needed if you do not need Crafted Emacs to install
;; packages for a module, for example,
;; `crafted-mastering-emacs-config' does not require any packages to
;; be installed.
(require 'crafted-completion-packages)	; add completion packages to
					; the
					; `package-selected-packages'
					; list

;; Install the packages listed in the `package-selected-packages' list. 
(package-install-selected-packages :noconfirm)

;;; Configuration phase

;; Some example modules to configure Emacs. Don't blindly copy these,
;; they are here for example purposes.  Find the modules which work
;; for you and add them here.
(require 'crafted-defaults-config)
(require 'crafted-mastering-emacs-config)
(require 'crafted-startup-config)
(unless crafted-startup-inhibit-splash
  (setq initial-buffer-choice #'crafted-startup-screen))

;;; Optional configuration

;;; Emacs lisp source/compiled preference
;; Prefer loading newest compiled .el file
(customize-set-variable 'load-prefer-newer t)

;; Profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Crafted Emacs loaded in %s."
                     (emacs-init-time))))

;; Set default coding system (especially for Windows)
(set-default-coding-systems 'utf-8)

