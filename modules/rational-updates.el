;;;; rational-updates.el --- Provides automatic update behavior for the configuration.  -*- lexical-binding: t; -*-

;; Copyright (C) 2022
;; SPDX-License-Identifier: MIT

;; Author: System Crafters Community

;; Commentary

;;; Code:

(defun rational-updates--call-git (&rest args)
  (let ((default-directory user-emacs-directory))
    (with-temp-buffer
      (if (apply #'vc-git--out-ok args)
          (buffer-string)
        nil))))

(defun rational-updates--get-new-commit-count ()
  (string-to-number (rational-updates--call-git "rev-list" "--count" "master..origin/master")))

(defun rational-updates--notify-of-updates ()
  (if (> (rational-updates--get-new-commit-count) 0)
      (message "Rational Emacs updates are available!")
    (message "Rational Emacs is up to date!")))

(defun rational-updates--poll-git-fetch-status (process)
  (if (eql (process-status process) 'exit)
      (when (eql (process-exit-status process) 0)
          )
    (run-at-time 1 nil #'rational-updates--poll-git-fetch-status process)))

(defun rational-updates--find-init-el ()
  (find-file-noselect (expand-file-name "init.el" user-emacs-directory)))

(defun rational-updates-check-for-latest ()
  "Fetches the latest Rational Emacs commits from GitHub and
notifies you if there are any updates."
  (interactive)
  (message "Checking for Rational Emacs updates...")
  (when (rational-updates--call-git #' "fetch" "origin")
    (rational-updates--notify-of-updates)))

(defun rational-updates-show-latest ()
  "Shows a buffer containing a log of the latest commits to
Rational Emacs."
  (interactive)
  (message "Fetching latest commit log for Rational Emacs...")
  (with-current-buffer (find-file-noselect (expand-file-name "init.el" user-emacs-directory))
    (vc-log-incoming)))

(defun rational-updates--pull-commits ()
  (message "Pulling latest commits to Rational Emacs...")
  (with-current-buffer (find-file-noselect (expand-file-name "init.el" user-emacs-directory))
    (vc-pull)))

(defun rational-updates-pull-latest (do-pull)
  "Pull the latest Rational Emacs version into the local repository.

If DO-PULL is nil then only the latest updates will be shown,
otherwise the local repository will get updated to the GitHub
version.

Interactively, the default if you just type RET is to show recent
changes as if you called `rational-updates-show-latest'.

With a `\\[universal-argument]' prefix immediately pull changes
and don't prompt for confirmation."
  (interactive
   (list
    (or current-prefix-arg
        (pcase (completing-read "Rational Update Action: " '("Show Log" "Update") nil t nil nil "Show Log")
          ("Show Log" nil)
          ("Update" t)))))
  (if do-pull
      (rational-updates--pull-commits)
    (rational-updates-show-latest)))

(defgroup rational-updates '()
  "Configuration for keeping Rational Emacs up-to-date."
  :tag "Rational Updates"
  :group 'rational)

;; TODO: use a derived type to check that the value is something `run-at-time'
;; will accept
(defcustom rational-updates-fetch-interval "24 hours"
  "The interval at which `rational-updates-mode' will check for updates.

The interval is scheduled with `run-at-time', so the value of
this variable must conform to a format accepted by
`run-at-time'."
  :group 'rational-updates)

(defun rational-updates--do-automatic-fetch ()
  (when rational-updates-mode
    (rational-updates-check-for-latest)
    (rational-updates--schedule-fetch)))

(defun rational-updates--schedule-fetch ()
  (run-at-time rational-updates-fetch-interval nil #'rational-updates--do-automatic-fetch))

(define-minor-mode rational-updates-mode
  "Provides an automatic update checking feature for Rational
Emacs.  When enabled, it will automatically check for updates at
the specified `rational-updates-fetch-interval'."
  :global t
  :group 'rational-updates
  (when rational-updates-mode
    (rational-updates--schedule-fetch)))

(provide 'rational-updates)
;;; rational-updates.el ends here
