;;; rational-windows.el -*- lexical-binding: t; -*-

;; Copyright (C) 2022
;; SPDX-License-Identifier: MIT

;; Author: System Crafters Community

;; Commentary

;; Emacs windows configuration.

;;; Code:

(defgroup rational-windows '()
  "Window related configuration for Rational Emacs."
  :tag "Rational Windows"
  :group 'rational)

(defcustom rational-windows-evil-style nil
  "When non-nil, window movement will use evil-style bindings."
  :group 'rational-windows
  :type 'boolean)

(defcustom rational-windows-prefix-key "C-c w"
  "Configure the prefix key for `rational-windows' bindings."
  :group 'rational-windows
  :type 'key)

(winner-mode 1)

(define-prefix-command 'rational-windows-key-map)

(define-key 'rational-windows-key-map (kbd "u") 'winner-undo)
(define-key 'rational-windows-key-map (kbd "n") 'windmove-down)
(define-key 'rational-windows-key-map (kbd "p") 'windmove-up)
(define-key 'rational-windows-key-map (kbd "b") 'windmove-left)
(define-key 'rational-windows-key-map (kbd "f") 'windmove-right)

(global-set-key (kbd rational-windows-prefix-key) 'rational-windows-key-map)

(provide 'rational-windows)
;;; rational-windows.el ends here
