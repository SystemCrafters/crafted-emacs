;;;; crafted-package/straight.el --- Configuration to use `straigt.el`.  -*- lexical-binding: t; -*-

;; Copyright (C) 2022
;; SPDX-License-Identifier: MIT

;; Author: System Crafters Community

;;; Commentary:

;; This code loads the bootstrap code for `straight.el', according as
;; described in <https://github.com/radian-software/straight.el>.

;; Bootstrap `straight.el' configuration, as described in
;; <https://github.com/radian-software/straight.el>.  Code provided
;; herein is intended for internal use, the user is not expected to
;; use the interface provided here to manage their packages.  In fact,
;; should the user prefer to use `use-package' in their configuration,
;; that should work seamlessly with this configuration. The user will
;; need to install `use-package', of course.  That being said, the
;; user is welcome to use the macros presented here.  They provide
;; `crafted-emacs' a standard way to install packages in the modules
;; provided as we can't predict if the user will choose to use
;; `straight.el' or some other tool.

;;; Code:

(defvar bootstrap-version)

(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" crafted-config-path))
      (bootstrap-version 5))
  ;; moves the straight install directory to the users crafted
  ;; configuration folder rather than the `user-emacs-directory'
  (setq straight-base-dir crafted-config-path)
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(defmacro crafted-package-install-package (package)
  "Crafted Emacs interface to install packages.

Only install the package if it is not already installed using
straight.el."
  `(straight-use-package ,package))

(defmacro crafted-package-installed-p (package)
  "Crafted Emacs interface to check if a package is installed."
  `(straight--installed-p ,package))

(provide 'crafted-package/straight)
;;; crafted-package/straight.el ends here
