;;;; crafted-package/package.el --- Configuration to use `package.el'.  -*- lexical-binding: t; -*-

;; Copyright (C) 2022
;; SPDX-License-Identifier: MIT

;; Author: System Crafters Community

;;; Commentary:

;; Bootstrap `package.el' configuration.  This is the default package
;; manager for Crafted Emacs.  Code provided herein is intended for
;; internal use, the user is not expected to use the interface
;; provided here to manage their packages.  In fact, should the user
;; prefer to use `use-package' in their configuration, that should
;; work seamlessly with this configuration. The user will need to
;; install `use-package', of course.  That being said, the user is
;; welcome to use the macros presented here.  They provide
;; `crafted-emacs' a standard way to install packages in the modules
;; provided as we can't predict if the user will choose to use
;; `package.el' or some other tool.

;;; Code:

;; package configuration
(require 'package)

(add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/"))
(add-to-list 'package-archives '("stable" . "https://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(customize-set-variable 'package-archive-priorities
                        '(("gnu"    . 99)   ; prefer GNU packages
                          ("nongnu" . 80)   ; use non-gnu packages if
                                            ; not found in GNU elpa
                          ("stable" . 70)   ; prefer "released" versions
                                            ; from melpa
                          ("melpa"  . 0)))  ; if all else fails, get it
                                            ; from melpa
(customize-set-variable 'package-user-dir
                        (expand-file-name "elpa/" crafted-config-path))

;;; package configuration
(defun crafted-package-archive-stale-p (archive)
  "Return `t' if ARCHIVE is stale.

ARCHIVE is stale if the on-disk cache is older than 1 day"
  (let ((today (time-to-days (current-time)))
        (archive-name (expand-file-name
                       (format "archives/%s/archive-contents" archive)
                       package-user-dir)))
    (time-less-p (time-to-days (file-attribute-modification-time
                                (file-attributes archive-name)))
                 today)))

(defun crafted-package-archives-stale-p ()
  "Return `t' if any PACKAGE-ARHIVES cache is out of date.

Check each archive listed in PACKAGE-ARCHIVES, if the on-disk
cache is older than 1 day, return a non-nil value. Fails fast,
will return `t' for the first stale archive found or `nil' if
they are all up-to-date."
  (interactive)
  (cl-some #'crafted-package-archive-stale-p (mapcar #'car package-archives)))

(defmacro crafted-package-install-package (package)
  "Only install the package if it is not already installed."
  `(unless (package-installed-p ,package) (package-install ,package)))

(defmacro crafted-package-installed-p (package)
  `(package-installed-p ,package))

(defun crafted-package-initialize ()
  "Initialize the package system."

  ;; Only use package.el if it is enabled. The user may have turned it
  ;; off in their `early-config.el' file, so respect their wishes if so.
  (when package-enable-at-startup
    (package-initialize)

    (require 'seq)
    ;; Only refresh package contents once per day on startup, or if the
    ;; `package-archive-contents' has not been initialized. If Emacs has
    ;; been running for a while, user will need to manually run
    ;; `package-refresh-contents' before calling `package-install'.
    (cond ((seq-empty-p package-archive-contents)
           (progn
             (message "crafted-init: package archives empty, initializing")
             (package-refresh-contents)))
          ((crafted-package-archives-stale-p)
           (progn
             (message "crafted-init: package archives stale, refreshing in the background")
             (package-refresh-contents t))))
    ))

(provide 'crafted-package/package)
;;; crafted-package/package.el ends here
