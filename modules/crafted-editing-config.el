;;; crafted-editing-config.el -*- lexical-binding: t; -*-

;; Copyright (C) 2022
;; SPDX-License-Identifier: MIT

;; Author: System Crafters Community

;; Commentary

;; Editing text configuration.

;;; Code:


(defun crafted-editing-configure-whitespace (use-tabs &optional use-globally &rest enabled-modes)
  "Helper function to configure `whitespace' mode.

Enable using TAB characters if USE-TABS is non-nil.  If
USE-GLOBALLY is non-nil, turn on `global-whitespace-mode'.  If
ENABLED-MODES is non-nil, it will be a list of modes to activate
whitespace mode using hooks.  The hooks will be the name of the
mode in the list with `-hook' appended.  If USE-GLOBALLY is
non-nil, ENABLED-MODES is ignored.

Configuring whitespace mode is not buffer local.  So calling this
function twice with different settings will not do what you
think.  For example, if you wanted to use spaces instead of tabs
globally except for in Makefiles, doing the following won't work:

;; turns on global-whitespace-mode to use spaces instead of tabs
(crafted-editing-configure-whitespace nil t)

;; overwrites the above to turn to use tabs instead of spaces,
;; does not turn off global-whitespace-mode, adds a hook to
;; makefile-mode-hook
(crafted-editing-configure-whitespace t nil 'makefile-mode)

Instead, use a configuration like this:
;; turns on global-whitespace-mode to use spaces instead of tabs
(crafted-editing-configure-whitespace nil t)

;; turn on the buffer-local mode for using tabs instead of spaces.
(add-hook 'makefile-mode-hook #'indent-tabs-mode)

For more information on `indent-tabs-mode', See the info
node `(emacs)Just Spaces'

Example usage:

;; Configure whitespace mode, does not turn on whitespace mode
;; since we don't know which modes to turn it on for.
;; You will need to do that in your configuration by adding
;; whitespace mode to the appropriate mode hooks.
(crafted-editing-configure-whitespace nil)

;; Configure whitespace mode, but turn it on globally.
(crafted-editing-configure-whitespace nil t)

;; Configure whitespace mode and turn it on only for prog-mode
;; and derived modes.
(crafted-editing-configure-whitespace nil nil 'prog-mode)"
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
