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
         (project-root
          (project-current nil (file-name-directory load-file-name))))))

;; update the `load-path' to include the Crafted Emacs modules path

(let ((modules (expand-file-name "./modules/" crafted-emacs-home)))
  (when (file-directory-p modules)
    (message (concat "adding modules to load-path: " modules))
    (add-to-list 'load-path modules)))

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

;; Add the Crafted Emacs documentation to the info nodes
(let ((crafted-info-dir (expand-file-name "docs/dir" crafted-emacs-home)))
  (when (file-exists-p crafted-info-dir)
    (require 'info)
    (info-initialize)
    (push (file-name-directory crafted-info-dir) Info-directory-list)))

;; Save all customizations to `custom-file'
(add-hook 'after-init-hook #'customize-save-customized)

(provide 'crafted-init-config)
;;; crafted-init-config.el ends here
