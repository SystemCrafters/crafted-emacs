;;; crafted-module.el -*- mode: emacs-lisp; mode: outline-minor; lexical-binding: t; -*-

;;; License
;; Copyright (C) 2022
;; SPDX-License-Identifier: MIT

;; Author: Erik Lundstedt, System Crafters Community
;; history:
;;;: https://gitlab.com/Erik.Lundstedt/crafted-speedbar
;;; Commentary:

;; this file was made with outline-minor-mode in mind
;; and therefore have ";;;+"-comments as headders.

;; crafted-moodule to setup "crafted defaults" for speedbar
;; speedbar is a file-tree (and more) that comes builtin to emacs
;; it also has integreation with some packages like Rmail and projectile

;;; Code:
;; require the module
(require 'speedbar)

;;; simple function for use in keybindings
(defun speedbar-switch-to-quick-buffers ()
  "Switch to quick-buffers expansion list."
  (interactive)
  (speedbar-change-initial-expansion-list "quick buffers"))


;;; keybindings
;;;; evil
;;;: im assuming that the "default" builtin keybinds are ok for non-evil users
;;;: feel free to contribute vanilla keybinds
(with-eval-after-load 'speedbar
  (with-eval-after-load 'evil
    ;;edit/open file under point
    (define-key speedbar-mode-map (kbd "<return>") 'speedbar-edit-line)
    ;;toggle thing at point
    (define-key speedbar-mode-map (kbd "<tab>") 'speedbar-toggle-line-expansion)
    ;;evaluate as elisp if file at point is elisp, I dont use this too much but might be useful
    (define-key speedbar-mode-map (kbd "<C-return>") 'speedbar-item-load)
    ;;temporarly change mode in speedbar to "buffer-switching mode".
    ;;useful for quickly switching to an open buffer
    (define-key speedbar-mode-map (kbd "b") 'speedbar-switch-to-quick-buffers)))

;; this only runs in some cases
(with-eval-after-load 'evil-collection
  (evil-collection-speedbar-setup))

;;; set some sane defaults, can be easily extended by user
(setq-default speedbar-frame-parameters
              '((name . "speedbar")
                (title . "speedbar")
                (minibuffer . nil)
                (border-width . 2)
                (menu-bar-lines . 0)
                (tool-bar-lines . 0)
                (unsplittable . t)
                (left-fringe . 10)))

;;; list of supported file-extensions
;; feel free to add to this list
(speedbar-add-supported-extension
 (list
;;;; lua and fennel(lisp that transpiles to lua)
  ".lua"
  ".fnl"
  ".fennel"
;;;; shellscript
  ".sh"
  ".bash";;is this ever used?
;;;; web languages
;;;;; Hyper-Text-markup-language(html) and php
  ".php"
  ".html"
  ".htm"
;;;;; ecma(java/type)-script
  ".js"
  ".json"
  ".ts"
;;;;; stylasheets
  ".css"
  ".less"
  ".scss"
  ".sass"
;;;; c/c++ and makefiles
  ".c"
  ".cpp"
  ".h"
  "makefile"
  "MAKEFILE"
  "Makefile"
;;;; runs on JVM, java,kotlin etc
  ".java"
  ".kt";;this is for kotlin
  ".mvn"
  ".gradle" ".properties";; this is for gradle-projects
  ".clj";;lisp on the JVM
;;;; lisps
  ".cl"
  ".el"
  ".scm"
  ".lisp"
;;;; configuration
  ".yaml"
  ".toml"
  ;; json is already in this list
;;;; notes,markup and orgmode
  ".md"
  ".markdown"
  ".org"
  ".txt"
  "README"
  ))

;;; make speedbar update automaticaly, and dont use ugly icons(images)
(setq-default speedbar-update-flag t)
(setq-default speedbar-use-images nil)
;; this can be set back to use icons by simply doing
;;`(setq-default speedbar-use-images t)` in your own config
(provide 'crafted-speedbar)
;;; crafted-speedbar.el ends here
