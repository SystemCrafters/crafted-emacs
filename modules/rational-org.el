;;; rational-org.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2022
;; SPDX-License-Identifier: MIT

;; Author: System Crafters Community

;; Commentary

;; Provides basic configuration for Org Mode.

;;; Code:

(rational-package-install-package 'org-appear)

;; Return or left-click with mouse follows link
(customize-set-variable 'org-return-follows-link t)
(customize-set-variable 'org-mouse-1-follows-link t)

;; Display links as the description provided
(customize-set-variable 'org-descriptive-links t)

;; Hide markup markers
(customize-set-variable 'org-hide-emphasis-markers t)
(add-hook 'org-mode-hook 'org-appear-mode)

(provide 'rational-org)
;;; rational-org.el ends here
