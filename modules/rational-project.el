;;;; rational-project.el --- Starting configuration for project management  -*- lexical-binding: t; -*-

;; Copyright (C) 2022
;; SPDX-License-Identifier: MIT

;; Author: System Crafters Community

;;; Commentary

;;

;;; Code:

(let ((user-projects (expand-file-name "projects" rational-config-path)))
  (when (file-exists-p user-projects)
    (setq project-list-file user-projects)))

(provide 'rational-project)
;;; rational-project.el ends here
