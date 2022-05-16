;;;; rational-compile.el --- Autocompile emacs-lisp code  -*- lexical-binding: t; -*-

;; Copyright (C) 2022
;; SPDX-License-Identifier: MIT

;; Author: System Crafters Community


;;; Commentary:

;; This module defines variables, functions and hooks to enable
;; automatic compilation of some emacs-lisp source files.

;; This could be activated for the `modules' subdirectory under your
;; `user-emacs-directory', your files under `rational-config-path', or
;; all files with mode `emacs-lisp-mode' after saving the file.

;; It also chooses to compile to byte-code or native code depending if
;; the feature is available on the current build.


;;; Code:


;;;; Variables:
;; We define a variable to enable/disable modules compilation.
(defvar rational-compile-modules t
  "When non-nil, compile emacs lisp sources for the modules on startup.")

;; We define a variable to enable/disable user's modules compilation.
(defvar rational-compile-user-modules t
  "When non-nil, compile user's emacs lisp sources for the modules on startup.")

(defvar rational-compile-module-list '()
  "A list of modules to compile.")

(defvar rational-compile-modules-path
  `(,(expand-file-name "custom-modules" rational-config-path)
    ,(expand-file-name "modules" user-emacs-directory))
  "Path where to locate modules.")

(defvar rational-compile-user-configuration t
  "When non-nil, compile emacs lisp sources for the modules on startup.")

(defvar rational-compile-init-files nil
  "When non-nil, compile init files on startup.")

(defvar rational-compile-on-save nil
  "When non-nil, compile the file after saving it.")

(defvar rational-compile-extra-directories-list nil
  "List of extra directories to search for source-files.

This could be another directory added to `load-path', that is not
standard, nor is it part of rational-emacs.")

(defvar rational-compile-init-files-list
  '("early-init.el" "init.el")
  "List of initialization file names.")

(defvar rational-compile-config-files-list
  '("early-config.el" "config.el")
  "List of configuration file names.")


;;;; Functions:
;; A function to locate a module's source file:
(defun rational-locate-module-file (module &optional path)
  "Locates the source-file for the MODULE.

MODULE must be a symbol.

If the optional argument PATH is non-nil, it asumes it is a list
containing the name of the directories to search for the
source-file.  If it is ommited or nil, it will search in the
directories defined in `rational-compile-modules-path' and
`rational-compile-extra-directories-list'.

It returns the first source-file that matches for the MODULE in
the order specified search path.  If it finds none, it retorns
nil."
  (let ((dir (seq-find (lambda (dir)
               (file-exists-p (expand-file-name (format "%s.el" (symbol-name module)) dir)))
             (or path
                 (flatten-list `(,rational-compile-modules-path
                                 ,rational-compile-extra-directories-list))))))
    (when dir
      (expand-file-name (format "%s.el" (symbol-name module)) dir))))

;; A function to compile a specific file
(defun rational-compile-file (f)
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
(defun rational-compile-buffer (&optional b)
  "Compiles (native or byte-code) the file of buffer B."
  (when (and b ;; Let's be sure it is not nil
             (not (bufferp b)))
    (cl-return nil))
  (let ((file (buffer-file-name b)))
    (when file
      (rational-compile-file file))))

;; A function to compile a specific directory
(defun rational-compile-directory (d)
  "Compiles (native or byte-ocde) the files within directory D.

D could be a single directory or a list of directories."
  (setq d (flatten-list (list d)))
  (message "Compiling directory/ies: %s" d)
  (if (featurep 'native-compile)
      (native-compile-async d t)
    (dolist (source d)
      (byte-recompile-directory source 0))))

;; A function to get the list of modules directories with full path:
(defun rational-compile--config-dirs-list ()
  "Returns a list of configured directories to autocompile."
  (flatten-list `(,(and rational-compile-modules
                        (expand-file-name "modules/" user-emacs-directory))
                  ,(and rational-compile-user-modules
                        (expand-file-name "custom-modules/" rational-config-path))
                  ,rational-compile-extra-directories-list)))

;; A function to get the list of init files with full path:
(defun rational-compile--init-files-list ()
  "Returns a list of the init files."
  (mapcar (lambda (f)
            (expand-file-name f user-emacs-directory))
          rational-compile-init-files-list))

(defun rational-compile--config-files-list ()
  "Returns a list of the init files."
  (mapcar (lambda (f)
            (expand-file-name f rational-config-path))
          rational-compile-config-files-list))
;;(rational-compile--config-files-list)

;; A function to execute the compilation of the modules within the list:
(defun rational-compile-modules ()
  "Compile specific files defined in `rational-compile-module-list'.

If `rational-compile-init-files' is non-nil, then it compiles
the init-files (`rational-compile-init-files-list') within
`user-emacs-directory'.

If `rational-compile-user-configuration' is non-nil, then it
compiles the config-files (`rational-compile-config-files-list')
within `rational-config-path'.

If `rational-compile-modules' is non-nil, then it compiles all
the source-files for the modules defined in
`rational-compile-module-list' within the subdirectory 'modules'
in `user-emacs-directory', or in any directory specified in
`rational-compile-extra-directories-list'.

If `rational-compile-user-modules' is non-nil, then it compiles
all the source-files for the modules defined in
`rational-compile-module-list' within the subdirectory 'modules'
in `rational-config-path'.

If any source-file for any module specified in
`rational-compile-module-list' doesn't exist within the paths
specified, then it is ignored without a warning."
  (interactive)
  (when rational-compile-init-files
    (rational-compile-init))
  (when rational-compile-user-configuration
    (rational-compile-config))
  (when rational-compile-modules
    (dolist (module rational-compile-module-list)
      (let ((module-src (rational-locate-module-file module
                                                     (flatten-list
                                                      `(,(expand-file-name "modules/" user-emacs-directory)
                                                        ,rational-compile-extra-directories-list)))))
        (when module-src
          (rational-compile-file module-src)))))
  (when rational-compile-user-modules
    (dolist (module rational-compile-module-list)
      (let ((module-src (rational-locate-module-file module
                                                     `(,(expand-file-name "custom-modules/" rational-config-path)))))
        (when module-src
          (rational-compile-file module-src))))))

;; A funciton to execute the compilation of the init files:
(defun rational-compile-init ()
  "Compile (native or byte-code) the initialization files.

The files to be compiled is defined in
`rational-compile-init-files-list'."
  (interactive)
  (rational-compile-file (rational-compile--init-files-list)))

;; A funciton to execute the compilation of the config files:
(defun rational-compile-config ()
  "Compile (native or byte-code) the configuration files.

The files to be compiled is defined in
`rational-compile-config-files-list'."
  (interactive)
  (rational-compile-file (rational-compile--config-files-list)))


;;;; Hooks:

;;;;; Modules
;; To autocompile modules.  This could be toggled by setting
;; `rational-compile-modules' or
;; `rational-compile-user-configuration' either to `nil' or
;; `t'.
(add-hook 'emacs-startup-hook ;; or kill-emacs-hook?
          (lambda ()
            (when (or rational-compile-modules
                      rational-compile-user-modules
                      rational-compile-init-files
                      rational-compile-user-configuration)
              (rational-compile-modules))))

;;;;; On save files
;; To auto compile after saving the file.  This could be toggled by
;; seting `rational-compile-on-save' to `nil' or `t'
(add-hook 'after-save-hook
          (lambda ()
            (when (and rational-compile-on-save
                       (string-equal major-mode "emacs-lisp-mode"))
              (rational-compile-buffer))))


;;; Package:
(provide 'rational-compile)
;;; rational-compile.el ends here
