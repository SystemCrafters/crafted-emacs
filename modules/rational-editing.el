;;; rational-editing.el -*- lexical-binding: t; -*-

;; Copyright (C) 2022
;; SPDX-License-Identifier: MIT

;; Author: System Crafters Community

;; Commentary

;; Editing text configuration.

;;; Code:

;; Customization group for the Rational Editing module.
(defgroup rational-editing '()
  "Editing related configuration for Rational Emacs."
  :tag "Editing"
  :group 'rational)

(defun rational-editing--prefer-tabs (enable)
  "Adjust whitespace configuration to support tabs based on ENABLE."
  (if enable
      (customize-set-variable 'whitespace-style
                              '(face empty trailing indentation::tab
                                     space-after-tab::tab
                                     space-before-tab::tab))
    (customize-set-variable 'whitespace-style
                            '(face empty trailing tab-mark
                                   indentation::space))))

(defun rational-editing--enable-whitespace-modes (modes)
  "Enable whitespace-mode for each mode specified by MODES."
  (dolist (mode modes)
    (add-hook (intern (format "%s-hook" mode))
              #'whitespace-mode)))

(defun rational-editing--disable-whitespace-modes (modes)
  "Do not enable whitespace-mode for each mode specified by MODES."
  (dolist (mode modes)
    (remove-hook (intern (format "%s-hook" mode))
                 #'whitespace-mode)))

;; provide an option for users who prefer tabs over spaces
(defcustom rational-editing-prefer-tabs nil
  "Prefer using tabs instead of spaces."
  :tag "Prefer tabs over spaces"
  :group 'rational-editing
  :set (lambda (sym val)
         (set-default sym val)
         (rational-editing--prefer-tabs val)))

;; whitespace cleanup configuration
(defcustom rational-editing-whitespace-cleanup-enabled-modes
  '(conf-mode prog-mode)
  "Modes which should have whitespace cleanup enabled."
  :type 'list
  :tag "Whitespace cleanup enabled modes"
  :group 'rational-editing
  :set (lambda (sym val)
         (set-default sym val)
         (rational-editing--enable-whitespace-modes val)))

(defcustom rational-editing-whitespace-cleanup-disabled-modes
  '(makefile-mode)
  "Modes which should not have whitespace cleanup enabled."
  :type 'list
  :tag "Whitespace cleanup disabled modes"
  :group 'rational-editing
  :set (lambda (sym val)
         (set-default sym val)
         (rational-editing--disable-whitespace-modes val)))

;; cleanup whitespace
(customize-set-variable 'whitespace-action '(cleanup auto-cleanup))

;; parentheses
(electric-pair-mode 1) ; auto-insert matching bracket
(show-paren-mode 1)    ; turn on paren match highlighting

(provide 'rational-editing)
;;; rational-editing.el ends here
