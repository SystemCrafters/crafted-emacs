;;; crafted-latex.el -*- lexical-binding: t; -*-

;; Copyright (C) 2022
;; SPDX-License-Identifier: MIT

;; Author: System Crafters Community

;; Commentary

;; Configure AUCTEX for editing LaTeX files.  Provides customization
;; for various environments to provide some useful additions related
;; to drawing graphs and mathematical diagrams, and code listings.

;;; Code:

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
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer))

(defun crafted-latex-use-pdf-tools ()
  "Use PDF Tools instead of docview, requires a build environment
to compile PDF Tools.

Depends on having `pdf-tools' installed.  See
`crafted-pdf-reader-packages.el' and
`crafted-pdf-reader-config.el'"
  (with-eval-after-load 'latex
    (customize-set-variable 'TeX-view-program-selection '((output-pdf "PDF Tools")))
    (customize-set-variable 'TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view)))
    (customize-set-variable 'TeX-source-correlate-start-server t)))

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
        (customize-set-variable 'auctex-latexmk-inherit-TeX-PDF-mode t))
      (add-hook 'TeX-mode-hook #'(lambda () (setq TeX-command-default "LatexMk"))))))

(provide 'crafted-latex)
;;; crafted-latex.el ends here
