;;; crafted-latex.el -*- lexical-binding: t; -*-

;; Copyright (C) 2022
;; SPDX-License-Identifier: MIT

;; Author: System Crafters Community

;; Commentary

;; Sane defaults for LaTeX editing

;;; Code:

(defgroup crafted-latex '()
  "LaTeX configuration for Crafted Emacs."
  :tag "Crafted LaTeX"
  :group 'crafted)

(defcustom crafted-latex-latexp (executable-find "latex")
  "Fully qualified path to the `latex' executable"
  :tag "`latex' executable location"
  :group 'crafted-latex
  :type 'string)

(defcustom crafted-latex-latexmkp (executable-find "latexmk")
  "Fully qualified path to the `latexmk' executable"
  :tag "`latexmk' executable location"
  :group 'crafted-latex
  :type 'string)

(defcustom crafted-latex-use-pdf-tools nil
  "Use pdf-tools as the pdf reader
   (this is automatic if you load `crafted-pdf-reader')"
  :tag "Use pdf-tools as pdf reader"
  :group 'crafted-latex
  :type 'boolean)

(defcustom crafted-latex-inhibit-latexmk t
  "When set to `nil', the package auctex-latexmk gets installed if the
latex and latexmk executable are found

This package contains a bug which might make it crash during loading
(with a bug related to tex-buf) on newer systems. For this reason, we inhibit
the installation of this package by default.


If you encounter the bug, you keep this package inhibited. You can install
a fix (not on melpa) with the following recipe, and the configuration in this file
will still work
'(auctex-latexmk :fetcher git :host github :repo \"wang1zhen/auctex-latexmk\")"
  :tag "Inhibit using `latexmk' command"
  :group 'crafted-latex
  :type 'boolean)


;; only install and load auctex when the latex executable is found,
;; otherwise it crashes when loading

(when crafted-latex-latexp
  (crafted-package-install-package 'auctex)

  (with-eval-after-load 'latex
    (customize-set-variable 'TeX-auto-save t)
    (customize-set-variable 'TeX-parse-self t)
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

    ;; to use pdfview with auctex
    (when crafted-latex-use-pdf-tools
      (customize-set-variable 'TeX-view-program-selection '((output-pdf "PDF Tools")))
      (customize-set-variable 'TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view)))
      (customize-set-variable 'TeX-source-correlate-start-server t))

    ;; electric pairs in auctex
    (customize-set-variable 'TeX-electric-sub-and-superscript t)
    (customize-set-variable 'LaTeX-electric-left-right-brace t)
    (customize-set-variable 'TeX-electric-math (cons "$" "$"))

    ;; open all buffers with the math mode and auto-fill mode
    (add-hook 'LaTeX-mode-hook #'auto-fill-mode)
    (add-hook 'LaTeX-mode-hook #'LaTeX-math-mode)

    ;; add support for references
    (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
    (customize-set-variable 'reftex-plug-into-AUCTeX t)

    ;; to have the buffer refresh after compilation
    (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
    ))

;; message the user if the latex executable is not found
(add-hook 'tex-mode-hook
          (lambda () (unless crafted-latex-latexp (message "latex executable not found"))))


;; The following is to use auctex with latexmk.
(defun crafted-latex--install-latexmk ()
  "install the auctex-latexmk package when the latex and latexmk executable
are found (see `crafted-latex-inhibit-latexmk' )"
  (when (and crafted-latex-latexp
             crafted-latex-latexmkp)
  (crafted-package-install-package 'auctex-latexmk)))

(defun crafted-latex--watch-inhibit-latexmk (sym val op buf)
  "watcher for the `crafted-latex-inhibit-latexmk' variable"
  (unless val
    (crafted-latex--install-latexmk)))

(add-variable-watcher 'crafted-latex-inhibit-latexmk
                      #'crafted-latex--watch-inhibit-latexmk)

(when (and crafted-latex-latexp
           crafted-latex-latexmkp)
  (with-eval-after-load 'latex
    (when (require 'auctex-latexmk nil 'noerror)
      (with-eval-after-load 'auctex-latexmk
        (auctex-latexmk-setup)
        (customize-set-variable 'auctex-latexmk-inherit-TeX-PDF-mode t))
          (add-hook 'TeX-mode-hook '(lambda () (setq TeX-command-default "LatexMk"))))))

(provide 'crafted-latex)
;;; crafted-latex.el ends here
