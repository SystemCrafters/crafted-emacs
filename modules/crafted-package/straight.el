;;;; crafted-package/straight.el --- Configuration to use `straigt.el`.  -*- lexical-binding: t; -*-

;; Copyright (C) 2022
;; SPDX-License-Identifier: MIT

;; Author: System Crafters Community

;;; Commentary:

;; This code loads the bootstrap code for `straight.el', according as
;; described in <https://github.com/radian-software/straight.el>.

;; It also adds a couple of macros to make an abstraction of the API:
;; `crafted-package-install-package' and
;; `crafted-package-installed-p'.

;;; Code:

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" crafted-config-path))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(defmacro crafted-package-install-package (package)
  "Only install the package if it is not already installed."
  `(straight-use-package ,package))

(defmacro crafted-package-installed-p (package)
  `(straight--installed-p ,package))

(provide 'crafted-package/straight)
;;; crafted-package/straight.el ends here
