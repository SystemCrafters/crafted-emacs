;;; rational-osx.el --- osx specific config -*- lexical-binding: t -*-

;; Copyright (C) 2022
;; SPDX-License-Identifier: MIT

;; Author: System Crafters Community

;;; Commentary:

;; Make OSX feel more at home

;;; Code:

  (defgroup rational-osx '()
    "Osx specific configurations for Rational Emacs."
    :tag "Rational Osx"
    :group 'rational)

  ;; Define configuration variables
  (defcustom rational-osx-transparent-titlebar nil
    "When set the osx title bar will become the same color as the emacs frame"
    :group 'rational-osx
    :type 'boolean)

;; titlebar

  (when rational-osx-transparent-titlebar
    (setq frame-resize-pixelwise t)
    (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
    (add-to-list 'default-frame-alist '(selected-frame) 'name nil)
    (add-to-list 'default-frame-alist '(ns-appearance . dark))) ;; assuming a dark theme is in use

;; Special keys

  (setq mac-right-option-modifier nil)
  (setq mac-command-modifier 'hyper)

;; Keybinds

  (global-set-key (kbd "H-c") 'kill-ring-save) ; ⌘-c = Copy
  (global-set-key (kbd "H-s") 'save-buffer) ; ⌘-s = Save
  (global-set-key (kbd "H-v") 'yank) ; ⌘-v = Paste
  (global-set-key (kbd "H-f") 'kill-region) ; ⌘-x = Cut
  (global-set-key (kbd "H-a") 'mark-whole-buffer) ; ⌘-a = Select all
  (global-set-key (kbd "H-z") 'undo) ; ⌘-z = Undo
  (global-set-key (kbd "H-q") 'kill-emacs) ; ⌘-q = Quit Emacs
  (global-set-key (kbd "H-n") 'make-frame) ; ⌘-n = New window
  (global-set-key (kbd "H-w") 'delete-frame) ; ⌘-t = Close window
  (global-set-key (kbd "H-`") 'ns-next-frame) ; ⌘-` = Next visible frame
  (global-set-key (kbd "H-}") 'tab-bar-switch-to-next-tab) ; ⌘-} = Next tab
  (global-set-key (kbd "H-{") 'tab-bar-switch-to-prev-tab) ; ⌘-{ = Previous tab
  (global-set-key (kbd "H-t") 'tab-bar-new-tab) ; ⌘-t = New tab

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

(provide 'rational-osx)
;;; rational-osx.el ends here
