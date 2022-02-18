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

(defun rational-updates-pull-latest ()
  "Pulls the latest updates to Rational Emacs into the local
configuration repository."
  (interactive)
  (let ((prompt-answer (read-string "This will update your Rational Emacs configuration, are you sure?  [yes/no/Log]: ")))
    (pcase (downcase prompt-answer)
      ("yes" (rational-updates--pull-commits))
      ("no" (message "You can always check the latest commits by running M-x rational-updates-show-latest."))
      ("log" (rational-updates-show-latest)))))

(defvar rational-updates-fetch-interval "24 hours"
  "The interval at which `rational-updates-mode' will check for updates.")

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
  (when rational-updates-mode
    (rational-updates--schedule-fetch)))

(provide 'rational-updates)
;;; rational-updates.el ends here
