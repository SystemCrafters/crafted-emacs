;;;; crafted-compile.el --- Autocompile emacs-lisp code  -*- lexical-binding: t; -*-

;; Copyright (C) 2022
;; SPDX-License-Identifier: MIT

;; Author: System Crafters Community


;;; Commentary:

;; This module defines variables, functions and hooks to enable
;; automatic compilation of some emacs-lisp source files.

;; This could be activated for the `modules' subdirectory under your
;; `user-emacs-directory', your files under `crafted-config-path', or
;; all files with mode `emacs-lisp-mode' after saving the file.

;; It also chooses to compile to byte-code or native code depending if
;; the feature is available on the current build.


;;; Code:


;;;; Variables:
;; We define a variable to enable/disable modules compilation.
(defvar crafted-compile-modules t
  "When non-nil, compile emacs lisp sources for the modules on startup.")

;; We define a variable to enable/disable user's modules compilation.
(defvar crafted-compile-user-modules t
  "When non-nil, compile user's emacs lisp sources for the modules on startup.")

(defvar crafted-compile-module-list '()
  "A list of modules to compile.")

(defvar crafted-compile-modules-path
  `(,(expand-file-name "custom-modules" crafted-config-path)
    ,(expand-file-name "modules" user-emacs-directory))
  "Path where to locate modules.")

(defvar crafted-compile-user-configuration t
  "When non-nil, compile emacs lisp sources for the modules on startup.")

(defvar crafted-compile-init-files nil
  "When non-nil, compile init files on startup.")

(defvar crafted-compile-on-save nil
  "When non-nil, compile the file after saving it.")

(defvar crafted-compile-extra-directories-list nil
  "List of extra directories to search for source-files.

This could be another directory added to `load-path', that is not
standard, nor is it part of crafted-emacs.")

(defvar crafted-compile-init-files-list
  '("early-init.el" "init.el")
  "List of initialization file names.")

(defvar crafted-compile-config-files-list
  '("early-config.el" "config.el")
  "List of configuration file names.")


;;;; Functions:
;; A function to locate a module's source file:
(defun crafted-locate-module-file (module &optional path)
  "Locates the source-file for the MODULE.

MODULE must be a symbol.

If the optional argument PATH is non-nil, it asumes it is a list
containing the name of the directories to search for the
source-file.  If it is ommited or nil, it will search in the
directories defined in `crafted-compile-modules-path' and
`crafted-compile-extra-directories-list'.

It returns the first source-file that matches for the MODULE in
the order specified search path.  If it finds none, it retorns
nil."
  (let ((dir (seq-find (lambda (dir)
               (file-exists-p (expand-file-name (format "%s.el" (symbol-name module)) dir)))
             (or path
                 (flatten-list `(,crafted-compile-modules-path
                                 ,crafted-compile-extra-directories-list))))))
    (when dir
      (expand-file-name (format "%s.el" (symbol-name module)) dir))))

;; A function to compile a specific file
(defun crafted-compile-file (f)
  "Compiles (native or byte-code) the file F.

F could be a single file or a list of files.

If F is a directory or contains a directory, the content of that
directory will be compiled, but not it's subdirectories."
  (setq f (flatten-list (list f)))
  (message "Compiling file(s): %s" f)
  (if (featurep 'native-compile)
    (dolist (source f)
      (if (file-newer-than-file-p
             (crafted-compile-locate-eln-file (file-name-base source))
             source)
          (native-compile-async f)
        (message "Skipping compilation of file %s" source)))
    (dolist (source f)
      (when (file-exists-p source)
        (if (file-directory-p source)
            (byte-recompile-directory source 0)
          (byte-recompile-file source nil 0))))))

;; A function to compile the buffer's file
(defun crafted-compile-buffer (&optional b)
  "Compiles (native or byte-code) the file of buffer B."
  (when (and b ;; Let's be sure it is not nil
             (not (bufferp b)))
    (cl-return nil))
  (let ((file (buffer-file-name b)))
    (when file
      (crafted-compile-file file))))

;; A function to compile a specific directory
(defun crafted-compile-directory (d)
  "Compiles (native or byte-ocde) the files within directory D.

D could be a single directory or a list of directories."
  (setq d (flatten-list (list d)))
  (message "Compiling directory/ies: %s" d)
  (if (featurep 'native-compile)
      (native-compile-async d t)
    (dolist (source d)
      (byte-recompile-directory source 0))))

;; A function to get the list of modules directories with full path:
(defun crafted-compile--config-dirs-list ()
  "Returns a list of configured directories to autocompile."
  (flatten-list `(,(and crafted-compile-modules
                        (expand-file-name "modules/" user-emacs-directory))
                  ,(and crafted-compile-user-modules
                        (expand-file-name "custom-modules/" crafted-config-path))
                  ,crafted-compile-extra-directories-list)))

;; A function to get the list of init files with full path:
(defun crafted-compile--init-files-list ()
  "Returns a list of the init files."
  (mapcar (lambda (f)
            (expand-file-name f user-emacs-directory))
          crafted-compile-init-files-list))

(defun crafted-compile--config-files-list ()
  "Returns a list of the init files."
  (mapcar (lambda (f)
            (expand-file-name f crafted-config-path))
          crafted-compile-config-files-list))
;;(crafted-compile--config-files-list)

;; A function to execute the compilation of the modules within the list:
(defun crafted-compile-modules ()
  "Compile specific files defined in `crafted-compile-module-list'.

If `crafted-compile-init-files' is non-nil, then it compiles
the init-files (`crafted-compile-init-files-list') within
`user-emacs-directory'.

If `crafted-compile-user-configuration' is non-nil, then it
compiles the config-files (`crafted-compile-config-files-list')
within `crafted-config-path'.

If `crafted-compile-modules' is non-nil, then it compiles all
the source-files for the modules defined in
`crafted-compile-module-list' within the subdirectory 'modules'
in `user-emacs-directory', or in any directory specified in
`crafted-compile-extra-directories-list'.

If `crafted-compile-user-modules' is non-nil, then it compiles
all the source-files for the modules defined in
`crafted-compile-module-list' within the subdirectory 'modules'
in `crafted-config-path'.

If any source-file for any module specified in
`crafted-compile-module-list' doesn't exist within the paths
specified, then it is ignored without a warning."
  (interactive)
  (when crafted-compile-init-files
    (crafted-compile-init))
  (when crafted-compile-user-configuration
    (crafted-compile-config))
  (when crafted-compile-modules
    (dolist (module crafted-compile-module-list)
      (let ((module-src (crafted-locate-module-file module
                                                     (flatten-list
                                                      `(,(expand-file-name "modules/" user-emacs-directory)
                                                        ,crafted-compile-extra-directories-list)))))
        (when module-src
          (crafted-compile-file module-src)))))
  (when crafted-compile-user-modules
    (dolist (module crafted-compile-module-list)
      (let ((module-src (crafted-locate-module-file module
                                                     `(,(expand-file-name "custom-modules/" crafted-config-path)))))
        (when module-src
          (crafted-compile-file module-src))))))

;; A funciton to execute the compilation of the init files:
(defun crafted-compile-init ()
  "Compile (native or byte-code) the initialization files.

The files to be compiled is defined in
`crafted-compile-init-files-list'."
  (interactive)
  (crafted-compile-file (crafted-compile--init-files-list)))

;; A funciton to execute the compilation of the config files:
(defun crafted-compile-config ()
  "Compile (native or byte-code) the configuration files.

The files to be compiled is defined in
`crafted-compile-config-files-list'."
  (interactive)
  (crafted-compile-file (crafted-compile--config-files-list)))


;;;; Hooks:

;;;;; Modules
;; To autocompile modules.  This could be toggled by setting
;; `crafted-compile-modules' or
;; `crafted-compile-user-configuration' either to `nil' or
;; `t'.
(add-hook 'emacs-startup-hook ;; or kill-emacs-hook?
          (lambda ()
            (when (or crafted-compile-modules
                      crafted-compile-user-modules
                      crafted-compile-init-files
                      crafted-compile-user-configuration)
              (crafted-compile-modules))))

;;;;; On save files
;; To auto compile after saving the file.  This could be toggled by
;; seting `crafted-compile-on-save' to `nil' or `t'
(add-hook 'after-save-hook
          (lambda ()
            (when (and crafted-compile-on-save
                       (string-equal major-mode "emacs-lisp-mode"))
              (crafted-compile-buffer))))

(defun crafted-compile-locate-eln-file (library)
  ""
  (let ((return nil))
    (dolist (dir (mapcar (lambda (path)
                           (expand-file-name comp-native-version-dir path))
                         native-comp-eln-load-path))
      (when (and (not return)
                 (file-exists-p dir))
        (let ((prefix (expand-file-name library dir)))
          (dolist (file-base (directory-files dir))
            (let ((file (expand-file-name file-base dir)))
              (when (and (not return)
                         (string-prefix-p prefix file)
                         (string-suffix-p ".eln" file))
                (setq return (expand-file-name file-base dir))))))))
    return))


;;; Package:
(provide 'crafted-compile)
;;; crafted-compile.el ends here
