;;;; crafted-writing-config.el --- Configuration for writing text documents  -*- lexical-binding: t; -*-

;; Copyright (C) 2023
;; SPDX-License-Identifier: MIT

;; Author: System Crafters Community

;;; Commentary:

;; Configuration for editing text or writing documents of different
;; kinds. Markdown and LaTeX documents are supported, general text
;; editing is also supported.  Not configured here are second brain /
;; zettlekasten systems, those are found in the `crafted-org-config'
;; module.

;;; Code:


;;; Whitespace
(defun crafted-writing-configure-whitespace (use-tabs &optional use-globally &rest enabled-modes)
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
      (customize-save-variable 'whitespace-style
                               '(face empty trailing indentation::tab
                                      space-after-tab::tab
                                      space-before-tab::tab))
    ;; use spaces instead of tabs
    (customize-save-variable 'whitespace-style
                             '(face empty trailing tab-mark
                                    indentation::space)))

  (if use-globally
      (global-whitespace-mode 1)
    (when enabled-modes
      (dolist (mode enabled-modes)
        (add-hook (intern (format "%s-hook" mode)) #'whitespace-mode))))

  ;; cleanup whitespace
  (customize-save-variable 'whitespace-action '(cleanup auto-cleanup)))

;;; parentheses
(electric-pair-mode 1) ; auto-insert matching bracket
(show-paren-mode 1)    ; turn on paren match highlighting


;;; LaTeX configuration
(with-eval-after-load 'latex
  (customize-save-variable 'TeX-auto-save t)
  (customize-save-variable 'TeX-parse-self t)
  (setq-default TeX-master nil)

  ;; compile to pdf
  (tex-pdf-mode)

  ;; correlate the source and the output
  (TeX-source-correlate-mode)

  ;; set a correct indentation in a few additional environments
  (add-to-list 'LaTeX-indent-environment-list '("lstlisting" current-indentation))
  (add-to-list 'LaTeX-indent-environment-list '("tikzcd" LaTeX-indent-tabular))
  (add-to-list 'LaTeX-indent-environment-list '("tikzpicture" current-indentation))

  ;; add a few macros and environment as verbatim
  (add-to-list 'LaTeX-verbatim-environments "lstlisting")
  (add-to-list 'LaTeX-verbatim-environments "Verbatim")
  (add-to-list 'LaTeX-verbatim-macros-with-braces "lstinline")
  (add-to-list 'LaTeX-verbatim-macros-with-delims "lstinline")

  ;; electric pairs in auctex
  (customize-save-variable 'TeX-electric-sub-and-superscript t)
  (customize-save-variable 'LaTeX-electric-left-right-brace t)
  (customize-save-variable 'TeX-electric-math (cons "$" "$"))

  ;; open all buffers with the math mode and auto-fill mode
  (add-hook 'LaTeX-mode-hook #'auto-fill-mode)
  (add-hook 'LaTeX-mode-hook #'LaTeX-math-mode)

  ;; add support for references
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  (customize-save-variable 'reftex-plug-into-AUCTeX t)

  ;; to have the buffer refresh after compilation
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer))

(defun crafted-latex-use-pdf-tools ()
  "Use PDF Tools instead of docview, requires a build environment
to compile PDF Tools.

Depends on having `pdf-tools' installed.  See
`crafted-pdf-reader-packages.el' and
`crafted-pdf-reader-config.el'"

  (with-eval-after-load 'latex
    (customize-save-variable 'TeX-view-program-selection '((output-pdf "PDF Tools")))
    (customize-save-variable 'TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view)))
    (customize-save-variable 'TeX-source-correlate-start-server t)))

;; message the user if the latex executable is not found
(add-hook 'tex-mode-hook
          #'(lambda ()
              (unless (executable-find "latex")
                (message "latex executable not found"))))

(when (and (executable-find "latex")
           (executable-find "latexmk"))
  (with-eval-after-load 'latex
    (when (require 'auctex-latexmk nil 'noerror)
      (with-eval-after-load 'auctex-latexmk
        (auctex-latexmk-setup)
        (customize-save-variable 'auctex-latexmk-inherit-TeX-PDF-mode t))
      (add-hook 'TeX-mode-hook #'(lambda () (setq TeX-command-default "LatexMk"))))))


;;; Markdown
(when (locate-library "markdown-mode")
  ;; because the markdown-command variable may not be loaded (yet),
  ;; check manually for the other markdown processors.  If it is
  ;; loaded, the others are superfluous but `or' fails fast, so they
  ;; are not checked if `markdown-command' is set and the command is
  ;; indeed found.
  (unless (or (and (boundp 'markdown-command)
                   (executable-find markdown-command))
              (executable-find "markdown")
              (executable-find "pandoc"))
    (message "No markdown processor found, preview may not possible."))

  (with-eval-after-load 'markdown-mode
    (customize-save-variable 'markdown-enable-math t)
    (customize-save-variable 'markdown-enable-html t)
    (add-hook 'markdown-mode-hook #'conditionally-turn-on-pandoc)))


;;; PDF Support when using pdf-tools
(when (locate-library "pdf-tools")
  (add-hook 'doc-view-mode-hook
            (lambda ()
              (require 'pdf-tools nil :noerror)))

  (with-eval-after-load 'pdf-tools
    (pdf-tools-install)
    (setq-default pdf-view-display-size 'fit-width)))

(provide 'crafted-writing-config)
;;; crafted-writing-config.el ends here
