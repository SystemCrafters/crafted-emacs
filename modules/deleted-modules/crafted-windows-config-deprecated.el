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

(keymap-set 'crafted-windows-key-map "u" 'winner-undo)
(keymap-set 'crafted-windows-key-map "n" 'windmove-down)
(keymap-set 'crafted-windows-key-map "p" 'windmove-up)
(keymap-set 'crafted-windows-key-map "b" 'windmove-left)
(keymap-set 'crafted-windows-key-map "f" 'windmove-right)

(keymap-global-set crafted-windows-prefix-key 'crafted-windows-key-map)

(provide 'crafted-windows-config)
;;; crafted-windows-config.el ends here
