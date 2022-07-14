;;; rational2crafted.el --- Convert rational-emacs to crafted-emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2022
;; SPDX-License-Identifier: MIT

;; Author: System Crafters Community

;;; Commentary:

;; Convert a Rational Emacs configuration to a Crafted Emacs
;; configuration. This involves the following steps:
;;
;; 1. Find the `rational-config-path' and copy it to the new crafted
;; path.
;;
;; 2. Set the `crafted-config-path' from the `rational-config-path'
;;
;; 3. Replace `rational' with `crafted' in the users `config.el' and
;; `early-config.el', assuming the latter exists.
;;
;; 4. Check if there are any modules in the users `custom-modules'
;; folder, and replace any `rational' with `crafted' including
;; renaming the modules if necessary.
;;
;; Usage:
;;
;; emacs -Q --batch -l rational2crafted.el

;;; Code:

(require 'xdg)
(require 'files)
(require 'seq)

;; Find the rational-config-path.  This will be used as the basis for
;; the crafted-config-path.  We find the the `rational-config-path'
;; the same way the `early-init.el' file does.
(defvar rational-config-path
  (cond
   ((featurep 'chemacs)
    (if (getenv  "RATIONAL_EMACS_HOME")
        (expand-file-name (getenv "RATIONAL_EMACS_HOME"))
      (expand-file-name "rational-emacs" user-emacs-directory)))
   ((getenv "RATIONAL_EMACS_HOME") (expand-file-name (getenv "RATIONAL_EMACS_HOME")))
   ((or (file-exists-p (xdg-config-home))
        (file-exists-p (expand-file-name "rational-emacs" (xdg-config-home))))
    (expand-file-name "rational-emacs" (xdg-config-home)))
   ((getenv "HOME") (expand-file-name ".rational-emacs" (getenv "HOME"))))
  "The user's rational-emacs configuration path.")

(print (concat "rational config path: " rational-config-path))

;; Set the `crafted-config-path' from the `rational-config-path'
(defvar crafted-config-path
  (string-replace "rational" "crafted" rational-config-path)
  "The user's crafted-emacs config path.")

(print (concat "crafted config path: " crafted-config-path))

;; Copy the rational-emacs folder to crafted-emacs.  This,
;; effectively, makes a backup of the rational-config-path. From here,
;; we will only work with in the crafted-config-path folder.
(copy-directory rational-config-path crafted-config-path)

(defvar crafted-config-file
  (expand-file-name "config.el" crafted-config-path)
  "The user's crafted-emacs config file, config.el")

(defvar crafted-early-config-file
  (expand-file-name "early-config.el" crafted-config-path)
  "The user's crafted-emacs early init file, early-config.el")

(print (concat "crafted-config-file: " crafted-config-file))
(print (concat "crafted-early-config-file: " crafted-early-config-file))

(defun perform-replacement ()
  (goto-char (point-min))
  (while (re-search-forward "rational" nil t)
    (replace-match "crafted")))

;; Modify the config.el file to replace the occurrences of `rational'
;; with `crafted'.
(with-temp-file crafted-config-file
  (insert-file-contents crafted-config-file)
  (perform-replacement))

;; Same with the early-config.el file, if it exists
(when (file-exists-p crafted-early-config-file)
  (with-temp-file crafted-early-config-file
    (insert-file-contents crafted-early-config-file)
    (perform-replacement)))

(print (concat "modified: " crafted-config-file))

(when (file-exists-p crafted-early-config-file)
  (print (concat "modified: " crafted-early-config-file)))

;; Update any custom modules that may exist. Replace occurrences of
;; `rational' with `crafted' within each file. Take into account the
;; word `rational' may appear in the file name and this script would
;; have changed a `require' in the `config.el' to use the word
;; `crafted' instead, so rename any files with the word `rational' to
;; have the word `crafted' instead.
(let ((files (mapcar (lambda (f)        ; get a list of custom module file names, fully path qualified
                       (let ((p (expand-file-name "custom-modules" crafted-config-path)))
                         (expand-file-name f p)))
                     (seq-drop (directory-files (expand-file-name "custom-modules" crafted-config-path)) 2))))
  (let ((crafted-file-names (mapcar (lambda (f) (string-replace "rational" "crafted" f)) ; crafted file names
                                    files)))
    (seq-do-indexed (lambda (file-name i)
                      (let ((crafted-name (seq-elt crafted-file-names i)))
                        (with-temp-file crafted-name ; create the crafted version of the file
                          (insert-file-contents file-name) ; insert the contents of the rational
                          (perform-replacement))
                        (print (concat "modified: " crafted-name))
                        (delete-file file-name))) ; delete the rational file
                    files)))

(provide 'rational2crafted)
;;; rational2crafted.el ends here
