;;; init.el --- Evil Example -*- lexical-binding: t; -*-

;; Copyright (C) 2023
;; SPDX-License-Identifier: MIT

;; Author: System Crafters Community

;;; Commentary:

;; Example init.el for advanced evil-mode configurations.

;;; Code:

;;; Settings for examples
;; Set one or more of these to `t' or `nil' to enable/disable configurations.
(defvar ce-example-use-evil-escape t
  "Load `evil-escape' package with example.
`evil-escape': Bind Escape to an alternative key combination (e.g. `jj').")


;;; Bootstrap
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (and custom-file
	   (file-exists-p custom-file))
  (load custom-file nil :nomessage))

(load (expand-file-name "../../modules/crafted-init-config"
                        user-emacs-directory))

;;; Packages

(require 'crafted-evil-packages)

;; evil-escape
(when ce-example-use-evil-escape
  (add-to-list 'package-selected-packages 'evil-escape))

(package-install-selected-packages :noconfirm)

;;; Configuration
(require 'crafted-evil-config)

;; evil-escape
(when ce-example-use-evil-escape
  ;; Configure `evil-escape' with the preferred key combination "jj".
  (customize-set-variable 'evil-escape-key-sequence (kbd "jj"))

  ;; Allow typing "jj" literally without exiting from insert-mode
  ;; if the keys are pressed 0.2s apart.
  (customize-set-variable 'evil-escape-delay 0.2)

  ;; Prevent "jj" from escaping any mode other than insert-mode.
  (defun my/not-insert-state-p ()
    "Inverse of `evil-insert-state-p`"
    (not (evil-insert-state-p)))

  (customize-set-variable 'evil-escape-inhibit-functions
                          (list #'my/not-insert-state-p))

  (evil-escape-mode))

;;; _
(provide 'init)
;; init.el ends here
