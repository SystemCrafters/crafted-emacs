;;; crafted-windows-config.el -*- lexical-binding: t; -*-

;; Copyright (C) 2022
;; SPDX-License-Identifier: MIT

;; Author: System Crafters Community

;;; Commentary:

;; Emacs windows configuration.

;;; Code:

(defgroup crafted-windows '()
  "Window related configuration for Crafted Emacs."
  :tag "Crafted Windows"
  :group 'crafted)

(defcustom crafted-windows-prefix-key "C-c w"
  "Configure the prefix key for window movement bindings.

Movment commands provided by `windmove' package, `winner-mode'
also enables undo functionality if the window layout changes."
  :group 'crafted-windows
  :type 'string)

(winner-mode 1)

(define-prefix-command 'crafted-windows-key-map)

(define-key 'crafted-windows-key-map (kbd "u") 'winner-undo)
(define-key 'crafted-windows-key-map (kbd "n") 'windmove-down)
(define-key 'crafted-windows-key-map (kbd "p") 'windmove-up)
(define-key 'crafted-windows-key-map (kbd "b") 'windmove-left)
(define-key 'crafted-windows-key-map (kbd "f") 'windmove-right)

(global-set-key (kbd crafted-windows-prefix-key) 'crafted-windows-key-map)

(provide 'crafted-windows-config)
;;; crafted-windows-config.el ends here
