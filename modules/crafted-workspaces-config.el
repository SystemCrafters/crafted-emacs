;;; crafted-workspaces-config.el --- Workspaces configuration  -*- lexical-binding: t; -*-

;; Copyright (C) 2022
;; SPDX-License-Identifier: MIT

;; Author: System Crafters Community
;; Keywords: project, workspace

;;; Commentary:

;; Use tabspaces to manage workspaces

;;; Code:

(with-eval-after-load 'tabspaces
  (customize-save-variable 'tabspaces-use-filtered-buffers-as-default t)
  (customize-save-variable 'tabspaces-remove-to-default t)
  (customize-save-variable 'tabspaces-include-buffers '("*scratch*")))

;; Activate it
(tabspaces-mode 1)

;; Make sure project is initialized
(project--ensure-read-project-list)

(provide 'crafted-workspaces-config)
;;; crafted-workspaces-config.el ends here
