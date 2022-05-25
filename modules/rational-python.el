;;; rational-python.el --- python configuration      -*- lexical-binding: t; -*-

;; Copyright (C) 2022
;; SPDX-License-Identifier: MIT

;; Author: System Crafters Community
;; Keywords: python

;;; Commentary:

;; Python development environment configuration.  Several python
;; packages can be installed with `pip'. Many of these are needed by
;; the Emacs packages used in this configuration.

;; * autopep8      -- automatically formats python code to conform to PEP 8 style guide
;; * black         -- uncompromising code formatter
;; * flake8        -- style guide enforcement
;; * importmagic   -- automatically add, remove, manage imports
;; * ipython       -- interactive python shell
;; * yapf          -- formatter for python code

;; Emacs packages to support python development:
;; * anaconda      -- code navigation, documentation and completion
;; * blacken       -- buffer formatting on save using black
;;                    (need to pip install black)
;; * eglot         -- language server integration
;;                    (need to pip install pyright)
;; * numpydoc      -- python doc templates, uses `yasnippets'
;; * pythonic      -- utility packages for running python in different
;;                    environments (dependency of anaconda)
;; * pyvenv        -- virtualenv wrapper

;; Suggested additional keybindings for python-mode
;; (with-eval-after-load "python"
;;   (define-key python-mode-map (kbd "C-c C-n") #'numpydoc-generate)
;;   (define-key python-mode-map (kbd "C-c e n") #'flymake-goto-next-error)
;;   (define-key python-mode-map (kbd "C-c e p") #'flymake-goto-prev-error))

;; Suggested keybindings for pyvenv mode
;; (with-eval-after-load "pyvenv"
;;   (define-key pyvenv-mode-map (kbd "C-c p a") #'pyvenv-activate)
;;   (define-key pyvenv-mode-map (kbd "C-c p d") #'pyvenv-deactivate)
;;   (define-key pyvenv-mode-map (kbd "C-c p w") #'pyvenv-workon))

;;; Code:

(rational-package-install-package 'anaconda-mode)
(rational-package-install-package 'blacken)
(rational-package-install-package 'eglot)
(rational-package-install-package 'numpydoc)
(rational-package-install-package 'pyvenv)

;; Hooks
(add-hook 'python-mode-hook #'anaconda-mode)
(add-hook 'python-mode-hook #'blacken-mode)
(add-hook 'python-mode-hook #'eldoc-mode)
(add-hook 'python-mode-hook #'eglot-ensure)
(add-hook 'python-mode-hook #'pyvenv-mode)
(add-hook 'python-mode-hook #'pyvenv-tracking-mode)


;;; pyvenv
;; restart python when the virtual environment changes
(add-hook 'pyvenv-post-activate-hooks #'pyvenv-restart-python)

;; default to the commonly used "venv" folder for the virtual
;; environment
(customize-set-variable 'pyvenv-default-virtual-env-name "venv")


;;; python mode
(customize-set-variable 'python-indent-guess-indent-offset-verbose nil)



;;; numpydoc
(customize-set-variable 'numpydoc-insert-examples-block nil)
(customize-set-variable 'numpydoc-template-long nil)

(provide 'rational-python)
;;; rational-python.el ends here
