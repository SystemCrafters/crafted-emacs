;;; rational-org.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2022
;; SPDX-License-Identifier: MIT

;; Author: System Crafters Community

;; Commentary

;; Provides basic configuration for Org Mode.

;;; Code:

(straight-use-package 'org-appear)

;; Return or left-click with mouse follows link
(setq org-return-follows-link t)
(setq org-mouse-1-follows-link t)

;; Display links as the description provided
(setq org-descriptive-links t)

;; Hide markup markers
(setq org-hide-emphasis-markers t)
(add-hook 'org-mode 'org-appear-mode)

(provide 'rational-org)
;;; rational-org.el ends here
