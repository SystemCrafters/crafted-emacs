;;;; crafted-project.el --- Starting configuration for project management  -*- lexical-binding: t; -*-

;; Copyright (C) 2022
;; SPDX-License-Identifier: MIT

;; Author: System Crafters Community

;;; Commentary

;; Provides default settings for project management with project.el

;;; Code:

(customize-set-variable 'project-list-file (expand-file-name "projects" crafted-config-var-directory))

(provide 'crafted-project)
;;; crafted-project.el ends here
