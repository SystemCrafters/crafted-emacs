;;;; rational-autocompile.el --- Autocompile emacs-lisp code  -*- lexical-binding: t; -*-

;; Copyright (C) 2022
;; SPDX-License-Identifier: MIT

;; Author: System Crafters Community


;;; Commentary:

;; This module defines variables, modules and hooks to enable
;; automatic compilation of emacs-lisp source files.

;; This could be activated for the `modules' subdirectory under your
;; `user-emacs-directory', your files under `rational-config-path', or
;; all files with mode `emacs-lisp-mode' after saving the file.

;; It also chooses to compile to byte-code or native code depending if
;; the feature is available on the current build.


;;; Code:

;;;; Variables:
;; We define a variable to enable/disable modules autocompilation.
(defvar rational-autocompile-modules t
  "When non-nil, autocompile emacs lisp sources for the modules on startup.")

(defvar rational-autocompile-user-configuration t
  "When non-nil, autocompile emacs lisp sources for the modules on startup.")

(defvar rational-autocompile-init-files nil
  "When non-nil, autocompile init files on startup.")

(defvar rational-autocompile-on-save nil
  "When non-nil, autocompile the file after saving it.")

(defvar rational-autocompile-extra-directories-list nil
  "List of extra directories to autocompile.")

(defvar rational-autocompile-init-files-list
  '("early-init.el" "init.el")
  "List of initialization file names.")

;;;; Functions:
;; A function to compile a specific file
(defun rational-autocompile-file (f)
  "Compiles (native or byte-code) the file F.

F could be a single file or a list of files.

If F is a directory or contains a directory, the content of that
directory will be compiled, but not it's subdirectories."
  (setq f (flatten-list (list f)))
  (message "Compiling file(s): %s" f)
  (if (featurep 'native-compile)
      (native-compile-async f)
    (dolist (source f)
      (when (file-exists-p source)
        (if (file-directory-p source)
            (byte-recompile-directory source 0)
          (byte-recompile-file source nil 0))))))

;; A function to compile the buffer's file
(defun rational-autocompile-buffer (&optional b)
  "Compiles (native or byte-code) the file of buffer B."
  (when b
    (unless (bufferp b)
      (cl-return nil))
  (let ((file (buffer-file-name b)))
    (when file
      (rational-autocompile-file file)))))

;; A function to compile a specific directory
(defun rational-autocompile-directory (d)
  "Compiles (native or byte-ocde) the files within directory D.

D could be a single directory or a list of directories."
  (setq d (flatten-list (list d)))
  (message "Compiling directory/ies: %s" d)
  (if (featurep 'native-compile)
      (native-compile-async d t)
    (dolist (source d)
      (byte-recompile-directory source 0))))

;; A function to get the list of modules directories with full path:
(defun rational-autocompile--config-dirs-list ()
  "Returns a list of configured directories to autocompile."
  (flatten-list `(,(and rational-autocompile-modules
                        (expand-file-name "modules/" user-emacs-directory))
                  ,(and rational-autocompile-user-configuration
                        (expand-file-name "./" rational-config-path))
                  ,rational-autocompile-extra-directories-list)))

;; A function to get the list of init files with full path:
(defun rational-autocompile--init-files-list ()
  "Returns a list of the init files."
  (mapcar (lambda (f)
            (expand-file-name f user-emacs-directory))
          rational-autocompile-init-files-list))

;; A function to execute the compilation of the modules directory:
(defun rational-autocompile-modules ()
  "Compile (native or byte-code) the modules' source code for faster startups."
  (interactive)
  (rational-autocompile-directory
   (rational-autocompile--config-dirs-list)))

;; A funciton to execute the compilation of the init files:
(defun rational-autocompile-init ()
  "Compile (native or byte-code) the initialization files.

The files to be compiled is defined in
`rational-autocompile-init-files-list'."
  (interactive)
  (rational-autocompile-file (rational-autocompile--init-files-list)))

;;;; Hooks:

;;;;; Modules
;; To autocompile modules.  This could be toggled by setting
;; `rational-autocompile-modules' or
;; `rational-autocompile-user-configuration' either to `nil' or
;; `t'.
(add-hook 'emacs-startup-hook ;; or kill-emacs-hook?
          (lambda ()
            (when (or rational-autocompile-modules
                      rational-autocompile-user-configuration)
              (rational-autocompile-modules))))

;;;;; Init files
;; To autocompile init files.  This could be toggled by setting
;; `rational-autocompile-init-files' or
;; `rational-autocompile-user-configuration' either to `nil' or
;; `t'.
(add-hook 'emacs-startup-hook ;; or kill-emacs-hook?
          (lambda ()
            (when (or rational-autocompile-init-files
                      rational-autocompile-user-configuration)
              (rational-autocompile-init))))

;;;;; On save files
;; To auto compile after saving the file.  This could be toggled by
;; seting `rational-autocompile-on-save' to `nil' or `t'
(add-hook 'after-save-hook
          (lambda ()
            (when (and rational-autocompile-on-save
                       (string-equal major-mode "emacs-lisp-mode"))
              (rational-autocompile-buffer))))


;;; Package:
(provide 'rational-autocompile)
;;; rational-autocompile.el ends here
