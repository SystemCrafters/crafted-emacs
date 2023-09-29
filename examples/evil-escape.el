;;; init.el -*- lexical-binding: t; -*-

;;; Commentary:

;; An example Emacs Init file that extends `crafted-evil-packages'
;; with `evil-escape', allowing alternative key combinations to escape
;; from Vim's insert-mode.

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (and custom-file
	   (file-exists-p custom-file))
  (load custom-file nil :nomessage))

(load "~/crafted-emacs/modules/crafted-init-config")

;;; Packages phase

(require 'crafted-evil-packages)

;; Add `evil-escape', the package responsible for binding Escape to an
;; alternative key combination.
(add-to-list 'package-selected-packages 'evil-escape)

(package-install-selected-packages :noconfirm)

;;; Configuration phase

(require 'crafted-evil-config)

;; Configure `evil-escape' with the preferred key combination "jj".
(customize-set-variable 'evil-escape-key-sequence (kbd "jj"))

;; Allow typing "jj" literally without exiting from insert-mode
;; if the keys are pressed 0.2s apart.
(customize-set-variable 'evil-escape-delay 0.2)

;; Prevent "jj" from escaping any mode other than insert-mode.
(defun my/not-insert-state-p ()
  "Inverse of `evil-insert-state-p`"
  (not (evil-insert-state-p)))

(customize-set-variable 'evil-escape-inhibit-functions (list #'my/not-insert-state-p))

;; Enable `evil-escape' mode globally.
(evil-escape-mode)
