;;; crafted-speedbar-config.el --- Speedbar configuration -*- mode: emacs-lisp; mode: outline-minor; lexical-binding: t; -*-

;;; License
;; Copyright (C) 2022
;; SPDX-License-Identifier: MIT

;; Author: Erik Lundstedt, System Crafters Community

;;; Commentary:

;; This file was made with outline-minor-mode in mind
;; and therefore have ";;;+"-comments as headders.

;; Configuration for speedbar, a file-tree (and more), that comes
;; builtin to Emacs it also has integration with some packages like
;; Rmail and projectile

;;; Code:
(require 'speedbar)

;;; Look & Feel
;; Auto-update when the attached frame changes directory
(customize-set-variable 'speedbar-update-flag t)

;; Disable icon images, instead use text
(customize-set-variable 'speedbar-use-images nil)

;; Customize Speedbar Frame
(customize-set-variable 'speedbar-frame-parameters
                        '((name . "speedbar")
                          (title . "speedbar")
                          (minibuffer . nil)
                          (border-width . 2)
                          (menu-bar-lines . 0)
                          (tool-bar-lines . 0)
                          (unsplittable . t)
                          (left-fringe . 10)))

;;; Keybindings
(defun crafted-speedbar-switch-to-quick-buffers ()
  "Temporarily switch to quick-buffers expansion list.
Useful for quickly switching to an open buffer."
  (interactive)
  (speedbar-change-initial-expansion-list "quick buffers"))

;; map switch-to-quick-buffers in speedbar-mode
(keymap-set speedbar-mode-map "b" #'crafted-speedbar-switch-to-quick-buffers)

;;; File Extensions
(speedbar-add-supported-extension
 (list
;;;; General Lisp Languages
  ".cl"
  ".li?sp"
;;;; Lua/Fennel (Lisp that transpiles to lua)
  ".lua"
  ".fnl"
  ".fennel"
;;;; JVM languages (Java, Kotlin, Clojure)
  ".kt"
  ".mvn"
  ".gradle"
  ".properties"
  ".cljs?"
;;;; shellscript
  ".sh"
  ".bash"
;;;; Web Languages and Markup/Styling
  ".php"
  ".ts"
  ".html?"
  ".css"
  ".less"
  ".scss"
  ".sass"
;;;; Makefile
  "makefile"
  "MAKEFILE"
  "Makefile"
;;;; Data formats
  ".json"
  ".yaml"
  ".toml"
;;;; Notes and Markup
  ".md"
  ".markdown"
  ".org"
  ".txt"
  "README"))

;;; _
(provide 'crafted-speedbar-config)
;;; crafted-speedbar-config.el ends here
