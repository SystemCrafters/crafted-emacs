;;; crafted-editing.el -*- lexical-binding: t; -*-

;; Copyright (C) 2022
;; SPDX-License-Identifier: MIT

;; Author: System Crafters Community

;; Commentary

;; Editing text configuration.

;;; Code:

;; Customization group for the Crafted Editing module.
(defgroup crafted-editing '()
  "Editing related configuration for Crafted Emacs."
  :tag "Crafted Editing"
  :group 'crafted)

(defun crafted-editing--prefer-tabs (enable)
  "Adjust whitespace configuration to support tabs based on ENABLE."
  (if enable
      (customize-set-variable 'whitespace-style
                              '(face empty trailing indentation::tab
                                     space-after-tab::tab
                                     space-before-tab::tab))
    (customize-set-variable 'whitespace-style
                            '(face empty trailing tab-mark
                                   indentation::space))))

(defun crafted-editing--enable-whitespace-modes (modes)
  "Enable whitespace-mode for each mode specified by MODES."
  (dolist (mode modes)
    (add-hook (intern (format "%s-hook" mode))
              #'whitespace-mode)))

(defun crafted-editing--disable-whitespace-modes (modes)
  "Do not enable whitespace-mode for each mode specified by MODES."
  (dolist (mode modes)
    (remove-hook (intern (format "%s-hook" mode))
                 #'whitespace-mode)))

;; provide an option for users who prefer tabs over spaces
(defcustom crafted-editing-prefer-tabs nil
  "Prefer using tabs instead of spaces."
  :tag "Prefer tabs over spaces"
  :group 'crafted-editing
  :set (lambda (sym val)
         (set-default sym val)
         (crafted-editing--prefer-tabs val)))

;; whitespace cleanup configuration
(defcustom crafted-editing-whitespace-cleanup-enabled-modes
  '(conf-mode prog-mode)
  "Modes which should have whitespace cleanup enabled."
  :type 'list
  :tag "Whitespace cleanup enabled modes"
  :group 'crafted-editing
  :set (lambda (sym val)
         (set-default sym val)
         (crafted-editing--enable-whitespace-modes val)))

(defcustom crafted-editing-whitespace-cleanup-disabled-modes
  '(makefile-mode)
  "Modes which should not have whitespace cleanup enabled."
  :type 'list
  :tag "Whitespace cleanup disabled modes"
  :group 'crafted-editing
  :set (lambda (sym val)
         (set-default sym val)
         (crafted-editing--disable-whitespace-modes val)))

;; cleanup whitespace
(customize-set-variable 'whitespace-action '(cleanup auto-cleanup))

;; parentheses
(electric-pair-mode 1) ; auto-insert matching bracket
(show-paren-mode 1)    ; turn on paren match highlighting

(provide 'crafted-editing)
;;; crafted-editing.el ends here
