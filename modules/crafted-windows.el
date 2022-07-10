;;; crafted-windows.el -*- lexical-binding: t; -*-

;; Copyright (C) 2022
;; SPDX-License-Identifier: MIT

;; Author: System Crafters Community

;; Commentary

;; Emacs windows configuration.

;;; Code:

(defgroup crafted-windows '()
  "Window related configuration for Crafted Emacs."
  :tag "Crafted Windows"
  :group 'crafted)

(defcustom crafted-windows-evil-style nil
  "When non-nil, window movement will use evil-style bindings."
  :group 'crafted-windows
  :type 'boolean)

(defcustom crafted-windows-prefix-key "C-c w"
  "Configure the prefix key for `crafted-windows' bindings."
  :group 'crafted-windows
  :type 'key)

(winner-mode 1)

(define-prefix-command 'crafted-windows-key-map)

(define-key 'crafted-windows-key-map (kbd "u") 'winner-undo)
(define-key 'crafted-windows-key-map (kbd "n") 'windmove-down)
(define-key 'crafted-windows-key-map (kbd "p") 'windmove-up)
(define-key 'crafted-windows-key-map (kbd "b") 'windmove-left)
(define-key 'crafted-windows-key-map (kbd "f") 'windmove-right)

(global-set-key (kbd crafted-windows-prefix-key) 'crafted-windows-key-map)

(provide 'crafted-windows)
;;; crafted-windows.el ends here
