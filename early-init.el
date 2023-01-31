;;; early-init.el --- Emacs early initialization for Crafted Emacs (optional) -*- lexical-binding: t; -*-
;;; Commentary:
;;
;;; Code:

;;; Garbage collection
;; Increase the GC threshold for faster startup
;; The default is 800 kilobytes.  Measured in bytes.

(setq gc-cons-threshold (* 50 1000 1000))

;;; Emacs lisp source/compiled preference
;; Prefer loading newest compiled .el file
(customize-set-variable 'load-prefer-newer t)

;;; Native compilation settings
(when (featurep 'native-compile)
  ;; Silence compiler warnings as they can be pretty disruptive
  (setq native-comp-async-report-warnings-errors nil)

  ;; Make native compilation happens asynchronously
  (setq native-comp-deferred-compilation t)

  ;; Set the right directory to store the native compilation cache
  ;; NOTE the method for setting the eln-cache directory depends on the emacs version
  (when (fboundp 'startup-redirect-eln-cache)
    (if (version< emacs-version "29")
        (add-to-list 'native-comp-eln-load-path (convert-standard-filename (expand-file-name "var/eln-cache/" user-emacs-directory)))
      (startup-redirect-eln-cache (convert-standard-filename (expand-file-name "var/eln-cache/" user-emacs-directory)))))

  (add-to-list 'native-comp-eln-load-path (expand-file-name "eln-cache/" user-emacs-directory)))

(defun crafted-using-guix-emacs-p ()
  "Verifies if the running Emacs executable is under the `/gnu/store/' path."
  (unless (or (equal system-type 'ms-dos)
              (equal system-type 'windows-nt))
    ;; Since there is no windows implementation of guix
    (string-prefix-p "/gnu/store/"
                     (file-truename
                      (executable-find
                       (car command-line-args))))))

(defvar crafted-prefer-guix-packages (crafted-using-guix-emacs-p)
  "If t, expect packages to be installed via Guix by default.")

(require 'package)
(require 'time-date)

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
(defvar crafted-bootstrap-package-perform-stale-archive-check t
  "Check if an archive is stale on startup when t.")

(defvar crafted-bootstrap-package-update-days 1
  "The number of days old when a package archive is considered stale.")

;;; package configuration
(defun crafted-package-archive-stale-p (archive)
  "Return t if ARCHIVE is stale.

ARCHIVE is stale if the on-disk cache is older than
`crafted-bootstrap-package-update-days' old.  If
`crafted-bootstrap-package-perform-stale-archive-check' is nil,
the check is skipped."
  (let* ((today (decode-time nil nil t))
         (archive-name (expand-file-name
                        (format "archives/%s/archive-contents" archive)
                        package-user-dir))
         (last-update-time (decode-time (file-attribute-modification-time
                                         (file-attributes archive-name))))
         (delta (make-decoded-time :day crafted-bootstrap-package-update-days)))
    (if crafted-bootstrap-package-perform-stale-archive-check
        (time-less-p (encode-time (decoded-time-add last-update-time delta))
                     (encode-time today))
      nil)))

(defun crafted-package-archives-stale-p ()
  "Return t if any PACKAGE-ARHIVES cache is out of date.

Check each archive listed in PACKAGE-ARCHIVES, if the on-disk
cache is older than 1 day, return a non-nil value.  Fails fast,
will return t for the first stale archive found or nil if
they are all up-to-date."
  (interactive)
  (cl-some #'crafted-package-archive-stale-p (mapcar #'car package-archives)))

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
             (package-refresh-contents t))))))

(crafted-package-initialize)

(provide 'early-init)

;;; early-init.el ends here
