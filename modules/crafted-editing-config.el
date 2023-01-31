;;; crafted-editing-config.el -*- lexical-binding: t; -*-

;; Copyright (C) 2022
;; SPDX-License-Identifier: MIT

;; Author: System Crafters Community

;; Commentary

;; Editing text configuration.

;;; Code:


(defun crafted-editing-configure-whitespace (use-tabs &optional use-globally &rest enabled-modes)
  "Configure `whitespace' mode.

Enable using TAB characters if USE-TABS is non-nil.  If
USE-GLOBALLY is non-nil, turn on `global-whitespace-mode'.  If
ENABLED-MODES is non-nil, it will be a list of modes to activate
whitespace mode using hooks.  The hooks will be the name of the
mode in the list with `-hook' appended.  If USE-GLOBALLY is
non-nil, ENABLED-MODES is ignored.

Example usage:

;; configure whitespace mode, does not turn on whitespace mode,
;; you will need to do that in your configuration. 
(crafted-editing-configure-whitespace)

;; configure whitespace mode, but turn it on globally
(crafted-editing-configure-whitespace t)

;; configure whitespace mode and turn it on only for prog-mode
;; and derived modes
(crafted-editing-configure-whitespace nil '(prog-mode))"
  (if use-tabs
      (customize-set-variable 'whitespace-style
                              '(face empty trailing indentation::tab
                                     space-after-tab::tab
                                     space-before-tab::tab))
    ;; use spaces instead of tabs
    (customize-set-variable 'whitespace-style
                            '(face empty trailing tab-mark
                                   indentation::space)))

  (if use-globally
      (global-whitespace-mode 1)
    (when enabled-modes
      (dolist (mode enabled-modes)
	(add-hook (intern (format "%s-hook" mode)) #'whitespace-mode))))
  
  ;; cleanup whitespace
  (customize-set-variable 'whitespace-action '(cleanup auto-cleanup)))


;;; parentheses
(electric-pair-mode 1) ; auto-insert matching bracket
(show-paren-mode 1)    ; turn on paren match highlighting

(provide 'crafted-editing-config)
;;; crafted-editing-config.el ends here
