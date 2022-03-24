;;;; rational-project.el --- Starting configuration for project management  -*- lexical-binding: t; -*-

;; Copyright (C) 2022
;; SPDX-License-Identifier: MIT

;; Author: System Crafters Community

;;; Commentary

;; Provides default settings for project management with project.el

;;; Code:

(customize-set-variable 'project-list-file (expand-file-name "projects" rational-config-var-directory))

(provide 'rational-project)
;;; rational-project.el ends here
