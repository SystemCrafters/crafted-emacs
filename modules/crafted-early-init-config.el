;;;; crafted-early-init-config.el --- Crafted early initialization  -*- lexical-binding: t; -*-

;; Copyright (C) 2023
;; SPDX-License-Identifier: MIT

;; Author: System Crafters Community

;;; Commentary:

;; Code to setup `package.el' during `early-init.el'

;;; Code:

(require 'package)

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

;;; refresh package archive contents
(require 'time-date)

(defvar crafted-package-perform-stale-archive-check t
  "Check if any package archives are stale.

Set this value in your `early-init.el' file.")

(defvar crafted-package-update-days 1
  "Number of days before an archive will be considered stale.

Set this value in your `early-init.el' file")

(defun crafted-package-archive-stale-p (archive)
  "Return t if ARCHIVE is stale.

ARCHIVE is stale if the on-disk cache is older than
`crafted-package-update-days' old.  If
`crafted-package-perform-stale-archive-check' is nil, the check
is skipped"
  (let* ((today (decode-time nil nil t))
         (archive-name (expand-file-name
                        (format "archives/%s/archive-contents" archive)
                        package-user-dir))
         (last-update-time (decode-time (file-attribute-modification-time
                                         (file-attributes archive-name))))
         (delta (make-decoded-time :day crafted-package-update-days)))
    (when crafted-package-perform-stale-archive-check
      (time-less-p (encode-time (decoded-time-add last-update-time delta))
                   (encode-time today)))))

(defun crafted-package-archives-stale-p ()
  "Return t if any package archives' cache is out of date.

Check each archive listed in `package-archives', if the on-disk
cache is older than `crafted-package-update-days', return a
non-nil value.  Fails fast, will return t for the first stale
archive found or nil if they are all up-to-date"
  (interactive)
  (cl-some #'crafted-package-archive-stale-p (mapcar #'car package-archives)))

(defun crafted-package-initialize ()
  "Initialize the package system.

Run this in the `before-init-hook'"

  (when package-enable-at-startup
    (package-initialize)

    (require 'seq)
    (message "crafted-package-config: checking package archives")
    (cond ((seq-empty-p package-archive-contents)
           (progn
             (message "crafted-package-config: package archives empty, initalizing")
             (package-refresh-contents)))
          ((crafted-package-archives-stale-p)
           (progn
             (message "crafted-package-config: package archives stale, refreshing")
             (package-refresh-contents t))))
    (message "crafted-package-config: package system initialized!")))

;; Initialize package system, refresh archives if necessary before
;; init file runs.
(add-hook 'before-init-hook #'crafted-package-initialize)

(provide 'crafted-early-init-config)
;;; crafted-early-init-config.el ends here
