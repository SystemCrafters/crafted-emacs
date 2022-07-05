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
    (customize-set-variable frame-resize-pixelwise t)
    (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
    (add-to-list 'default-frame-alist '(selected-frame) 'name nil)
    (add-to-list 'default-frame-alist '(ns-appearance . dark))) ;; assuming a dark theme is in use

;; Special keys

  (customize-set-variable mac-right-option-modifier nil)
  (customize-set-variable mac-command-modifier 'super)
  (customize-set-variable ns-function-modifier 'hyper)

;; Keybinds

  (global-set-key (kbd "s-W") 'delete-frame) ; ⌘-W = Close window
  (global-set-key (kbd "s-}") 'tab-bar-switch-to-next-tab) ; ⌘-} = Next tab
  (global-set-key (kbd "s-{") 'tab-bar-switch-to-prev-tab) ; ⌘-{ = Previous tab
  (global-set-key (kbd "s-t") 'tab-bar-new-tab) ;⌘-t = New tab
  (global-set-key (kbd "s-w") 'tab-bar-close-tab) ; ⌘-w = Close tab

  (unless (< emacs-major-version 28)
    (global-set-key (kbd "s-Z") 'undo-redo)) ; ⌘-Z = Redo

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
