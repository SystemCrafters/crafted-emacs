;;;; crafted-package/package.el --- Configuration to use `package.el'.  -*- lexical-binding: t; -*-

;; Copyright (C) 2022
;; SPDX-License-Identifier: MIT

;; Author: System Crafters Community

;;; Commentary:

;; Most of this code was already in `early-init.el' and `init.el'. I
;; Just moved here.

;; I added the macro `crafted-package-installed-p', to abstract the
;; verification of installed packages.

;; It still need to implement the checking of packages installed via
;; `guix'.

;;; Code:

;; package configuration
(require 'package)

;; Emacs 27.x has gnu elpa as the default
;; Emacs 28.x adds the nongnu elpa to the list by default, so only
;; need to add nongnu when this isn't Emacs 28+
(when (version< emacs-version "28")
  (add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/")))
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
 )

(provide 'crafted-package/package)
;;; crafted-package/package.el ends here
