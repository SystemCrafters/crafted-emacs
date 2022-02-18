;;;; rational-updates.el --- Provides automatic update behavior for the configuration.  -*- lexical-binding: t; -*-

;; Copyright (C) 2022
;; SPDX-License-Identifier: MIT

;; Author: System Crafters Community

;; Commentary

;;; Code:

(defun rational-updates--get-new-commit-count ()
  (string-to-number (shell-command-to-string "git rev-list --count master..origin/master")))

(defun rational-updates--poll-git-fetch-status (process)
  (if (eql (process-status process) 'exit)
      (when (eql (process-exit-status process) 0)
          (if (> (rational-updates--get-new-commit-count) 0)
              (message "Rational Emacs updates are available!")
            (message "Rational Emacs is up to date!")))
    (run-at-time 1 nil #'rational-updates--poll-git-fetch-status process)))

(defun rational-updates-check-for-latest ()
  (interactive)
  (let ((git-process (start-process-shell-command "rational-git-fetch" nil "git fetch origin")))
    (run-at-time 1 nil #'rational-updates--poll-git-fetch-status git-process)))

(defun rational-updates-show-latest ()
  (interactive)
  (message "Fetching latest commit log for Rational Emacs...")
  (with-current-buffer (find-file-noselect (expand-file-name "init.el" user-emacs-directory))
    (vc-log-incoming)))

(defun rational-updates--pull-commits ()
  (interactive)
  (message "Pulling latest commits to Rational Emacs...")
  (with-current-buffer (find-file-noselect (expand-file-name "init.el" user-emacs-directory))
    (vc-pull)))

(defun rational-updates-pull-latest ()
  (interactive)
  (let ((prompt-answer (read-string "This will update your Rational Emacs configuration, are you sure?  [yes/no/Log]: ")))
    (pcase (downcase prompt-answer)
      ("yes" (rational-updates--pull-commits))
      ("no" (message "You can always check the latest commits by running M-x rational-updates-show-latest."))
      ("log" (rational-updates-show-latest)))))

(provide 'rational-updates)
;;; rational-updates.el ends here
