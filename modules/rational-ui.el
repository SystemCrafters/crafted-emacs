;;; rational-ui.el -*- lexical-binding: t; -*-

;; Copyright (C) 2022
;; SPDX-License-Identifier: MIT

;; Author: System Crafters Community

;; Commentary

;; User interface customizations. Examples are the modeline and how
;; help buffers are displayed.

;;; Code:

(straight-use-package 'all-the-icons)
(straight-use-package 'doom-themes)
(straight-use-package 'elisp-demos)
(straight-use-package 'helpful)

;;;; Font

(defcustom rational-ui-default-font nil
  "The configuration of the `default' face.
Use a plist with the same key names as accepted by `set-face-attribute'.")

(when rational-ui-default-font
  (apply 'set-face-attribute 'default nil (cons :font rational-ui-default-font)))

;;;; Mode-Line


(defcustom rational-ui-use-doom-modeline t
  "weather or not to use doom-modeline, on by default"
:type 'boolean
  )

(when rational-ui-use-doom-modeline
  (straight-use-package 'doom-modeline)
  ;; Start up the modeline after initialization is finished
  (add-hook 'after-init-hook 'doom-modeline-init)

  ;; Configure `doom-modeline'
  (customize-set-variable 'doom-modeline-height 15)
  (customize-set-variable 'doom-modeline-bar-width 6)
  (customize-set-variable 'doom-modeline-minor-modes t)
  (customize-set-variable 'doom-modeline-buffer-file-name-style 'truncate-except-project)
)

;;;; Help Buffers

;; Make `describe-*' screens more helpful
(require 'helpful)
(define-key helpful-mode-map [remap revert-buffer] #'helpful-update)
(global-set-key [remap describe-command] #'helpful-command)
(global-set-key [remap describe-function] #'helpful-callable)
(global-set-key [remap describe-key] #'helpful-key)
(global-set-key [remap describe-symbol] #'helpful-symbol)
(global-set-key [remap describe-variable] #'helpful-variable)
(global-set-key (kbd "C-h F") #'helpful-function)

;; Bind extra `describe-*' commands
(global-set-key (kbd "C-h K") #'describe-keymap)

;;;; Line Numbers

(defcustom rational-ui-display-line-numbers nil
  "Whether line numbers should be enabled."
  :type 'boolean)

(when rational-ui-display-line-numbers
  (add-hook 'conf-mode-hook #'display-line-numbers-mode)
  (add-hook 'prog-mode-hook #'display-line-numbers-mode)
  (add-hook 'text-mode-hook #'display-line-numbers-mode)
  (setq-default
   display-line-numbers-grow-only t
   display-line-numbers-type t
   display-line-numbers-width 2))

;;;; Elisp-Demos

;; also add some examples
(require 'elisp-demos)
(advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update)

;; add visual pulse when changing focus, like beacon but built-in
;; from from https://karthinks.com/software/batteries-included-with-emacs/
(defun pulse-line (&rest _)
  "Pulse the current line."
  (pulse-momentary-highlight-one-line (point)))

(dolist (command '(scroll-up-command scroll-down-command
				                     recenter-top-bottom other-window))
  (advice-add command :after #'pulse-line))

(provide 'rational-ui)
;;; rational-ui.el ends here
