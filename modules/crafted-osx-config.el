;;; crafted-osx-config.el --- osx specific config -*- lexical-binding: t -*-

;; Copyright (C) 2022
;; SPDX-License-Identifier: MIT

;; Author: System Crafters Community

;;; Commentary:

;; Make OSX feel more at home

;;; Code:

;; titlebar

(defun crafted-osx-transparent-titlebar ()
  "Set the titlebar to be transparent."
  (interactive)
  (customize-set-variable 'frame-resize-pixelwise t)
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t)))

;; Special keys
(when (featurep 'ns)
  (customize-set-variable mac-right-option-modifier nil)
  (customize-set-variable mac-command-modifier 'super)
  (customize-set-variable ns-function-modifier 'hyper))

;; Keybinds

(keymap-global-set "s-W" #'delete-frame) ; ⌘-W = Close window
(keymap-global-set "s-}" #'tab-bar-switch-to-next-tab) ; ⌘-} = Next tab
(keymap-global-set "s-{" #'tab-bar-switch-to-prev-tab) ; ⌘-{ = Previous tab
(keymap-global-set "s-t" #'tab-bar-new-tab) ;⌘-t = New tab
(keymap-global-set "s-w" #'tab-bar-close-tab) ; ⌘-w = Close tab

(unless (version< emacs-version "28")
  (keymap-global-set "s-Z" 'undo-redo)) ; ⌘-Z = Redo

;; Better compatibility with osx based window managers

(when (featurep 'ns)
  (defun ns-raise-emacs ()
    "Raise Emacs."
    (ns-do-applescript "tell application \"Emacs\" to activate"))
  (defun ns-raise-emacs-with-frame (frame)
    "Raise Emacs and select the provided frame."
    (with-selected-frame frame
      (when (display-graphic-p)
        (ns-raise-emacs))))
  (add-hook 'after-make-frame-functions 'ns-raise-emacs-with-frame)
  (when (display-graphic-p)
    (ns-raise-emacs)))

(provide 'crafted-osx-config)
;;; crafted-osx-config.el ends here
