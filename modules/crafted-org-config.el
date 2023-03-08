;;; crafted-org-config.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2022
;; SPDX-License-Identifier: MIT

;; Author: System Crafters Community

;; Commentary

;; Provides basic configuration for Org Mode.

;;; Code:

;; Return or left-click with mouse follows link
(customize-set-variable 'org-return-follows-link t)
(customize-set-variable 'org-mouse-1-follows-link t)

;; Display links as the description provided
(customize-set-variable 'org-link-descriptive t)

;; Hide markup markers
(customize-set-variable 'org-hide-emphasis-markers t)
(when (featurep 'org-appear)
  (add-hook 'org-mode-hook 'org-appear-mode))

;; Disable auto-pairing of "<" in org-mode
(add-hook 'org-mode-hook (lambda ()
                           (setq-local electric-pair-inhibit-predicate
                                       `(lambda (c)
                                          (if (char-equal c ?<) t (,electric-pair-inhibit-predicate c))))))


;;; Org Roam
(when (featurep 'org-roam)
  (unless (eq (custom-variable-state 'org-roam-directory nil)
              'standard)
    (customize-set-variable 'org-roam-directory
                            (expand-file-name "org-roam" user-emacs-directory)))
  ;; suggested keymap based on example from project docs
  ;; (keymap-global-set "C-c r l" #'org-roam-buffer-toggle)
  ;; (keymap-global-set "C-c r f" #'org-roam-node-find)
  ;; (keymap-global-set "C-c r g" #'org-roam-graph)
  ;; (keymap-global-set "C-c r i" #'org-roam-node-insert)
  ;; (keymap-global-set "C-c r c" #'org-roam-capture)
  ;; (keymap-global-set "C-c r j" . org-roam-dailies-capture-today)

  ;; If you're using a vertical completion framework, you might want a
  ;; more informative completion interface
  (when (or (bound-and-true-p fido-vertical-mode)
            (bound-and-true-p icomplete-vertical-mode)
            (bound-and-true-p vertico))
    (customize-set-variable 'org-roam-node-display-template
                            (concat "${title:*} "
                                    (propertize "${tags:10}" 'face 'org-tag))))
  (org-roam-db-autosync-mode))


(provide 'crafted-org-config)
;;; crafted-org-config.el ends here
