;;;; crafted-init-config.el --- Crafted Emacs initial configuration  -*- lexical-binding: t; -*-

;; Copyright (C) 2023
;; SPDX-License-Identifier: MIT

;; Author: System Crafters Community

;;; Commentary:

;; Perform some initialization for use by Crafted Emacs modules among
;; other things.

;;; Code:

;; Use this file to find the project root where you cloned
;; `crafted-emacs' and use that as the value for the
;; `crafted-emacs-home' value which is needed by a few modules,
;; including the template below used for writing Crafted Emacs
;; modules.

(defgroup crafted-init '()
  "Initialization configuration for Crafted Emacs"
  :tag "Crafted Init"
  :group 'crafted)

;; By default, crafted emacs calls `customize-save-variable' in the
;; `after-init-hook'. The user can opt out of this by setting
;; `crafted-init-auto-save-customized' to nil.
(defcustom crafted-init-auto-save-customized t
  "Save customized variables automatically every session."
  :type 'boolean
  :group 'crafted-init)

;; By default, crafted emacs calls `package--save-selected-packages' in the
;; `after-init-hook'. The user can opt out of this by setting
;; `crafted-init-auto-save-selected-packages' to nil.
(defcustom crafted-init-auto-save-selected-packages t
  "Save the list of selected packages automatically every session."
  :type 'boolean
  :group 'crafted-init)

(when (version< emacs-version "29")
  ;; Get some Emacs 29 compatibility functions. Notably missing is
  ;; `setopt' which the `compat' library deliberately does not
  ;; provide, so we continue to use the `customize-set-variable'
  ;; function for setting user options, unless we have a version guard
  ;; around a block, in which case we use `setopt' instead.
  (unless (require 'compat nil :noerror)
    (package-install 'compat)))

(require 'project)

;; If the source file is newer than the compiled file, load it instead
;; of the compiled version.
(customize-set-variable 'load-prefer-newer t)

;; Create the variable if needed
(if (boundp 'crafted-emacs-home)
    (message "crafted-emacs-home value set by user: %s" crafted-emacs-home)
  (defvar crafted-emacs-home nil
    "Defines where the Crafted Emacs project was cloned to.

This is set when loading the crafted-init-config.el module during
initialization.  Alternatively, it can be set by the user
explicitly."))

;; Only set the `crafted-emacs-home' variable if it does not already
;; have a value set by the user.
(when (null crafted-emacs-home)
  (setq crafted-emacs-home
        (expand-file-name
         (vc-find-root load-file-name "modules"))))

;; we still don't have a `crafted-emacs-home' value, so we can't
;; proceed, without it the `load-path' will not be set correctly and
;; crafted-emacs modules will not be found.
(unless crafted-emacs-home
  (error "%s\n%s"
         "The value for crafted-emacs-home is not set"
         "Please set this value to the location where crafted-emacs is installed"))

;; update the `load-path' to include the Crafted Emacs modules path

(let ((modules (expand-file-name "./modules/" crafted-emacs-home)))
  (when (file-directory-p modules)
    (message "adding modules to load-path: %s" modules)
    (add-to-list 'load-path modules)))

;; If a `custom-modules' directory exists in the
;; `user-emacs-directory', include it on the load-path.
(let ((custom-modules (expand-file-name "custom-modules" user-emacs-directory)))
  (when (file-directory-p custom-modules)
    (message "adding custom-modules to load-path: %s" custom-modules)
    (add-to-list 'load-path custom-modules)))

;; When writing crafted-modules, insert header from skeleton
(auto-insert-mode)
(with-eval-after-load "autoinsert"
  ;; Handle a missing `custom-file' by not running `auto-insert' when
  ;; it gets created.  The value of the `custom-file' for Crafted
  ;; Emacs is `custom.el', however, the user could change that to
  ;; something else.  On startup, asking the user to automatically
  ;; insert the standard headers may cause confusion if they choose to
  ;; answer 'y'.  Here we advise the `auto-insert' function to not run
  ;; when the file is the `custom-file' and it is being created.
  (defun ignore-auto-insert-for-custom (orig-auto-insert &rest args)
    "Apply ORIG-AUTO-INSERT only when the file is not the
         `custom-file' to avoid confusion when that file doesn't exist on
         startup."
    (if (and custom-file buffer-file-name
             (string-match (file-name-nondirectory custom-file) buffer-file-name))
        (message "Skipping auto-insert for %s" custom-file)
      (apply orig-auto-insert args)))
  (advice-add 'auto-insert :around #'ignore-auto-insert-for-custom)
  (define-auto-insert
    (cons (expand-file-name "modules/crafted-.*\\.el" crafted-emacs-home)
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

;; Add the Crafted Emacs documentation to the info nodes
(let ((crafted-info-dir (expand-file-name "docs/dir" crafted-emacs-home)))
  (when (file-exists-p crafted-info-dir)
    (require 'info)
    (info-initialize)
    (push (file-name-directory crafted-info-dir) Info-directory-list)))

(defun crafted-save-customized ()
  "Save and reload the customizations made during Emacs initialization.

Due to the way Emacs Customization works - or seems to - and this
bug: https://debbugs.gnu.org/cgi/bugreport.cgi?bug=21355, we need
to save all customizations made during Emacs startup and then
reload the custom-file.  This sets (or should set) all customized
values to the \"SET and saved.\" state and (hopefully) avoid the
bug above.  If the user never set a value for `custom-file' then
we can't reload the file."
  (customize-save-customized)
  ;; only load the `custom-file' if it is not `nil'. 
  (unless custom-file
    (load custom-file :noerror)))

;; Save all customizations to `custom-file', unless the user opted out.
(when crafted-init-auto-save-customized
  (add-hook 'after-init-hook #'crafted-save-customized))
(when crafted-init-auto-save-selected-packages
  (add-hook 'after-init-hook #'package--save-selected-packages))

(provide 'crafted-init-config)
;;; crafted-init-config.el ends here
