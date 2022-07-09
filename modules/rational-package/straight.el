;;;; rational-package/straight.el --- Configuration to use `straigt.el`.  -*- lexical-binding: t; -*-

;; Copyright (C) 2022
;; SPDX-License-Identifier: MIT

;; Author: System Crafters Community

;;; Commentary:

;;

;;; Code:

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" rational-config-path))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(defmacro rational-package-install-package (package)
  "Only install the package if it is not already installed."
  `(straight-use-package ,package))

(defmacro rational-package-installed-p (package)
  `(straight--installed-p ,package))

(provide 'rational-package/straight)
;;; rational-package/straight.el ends here
