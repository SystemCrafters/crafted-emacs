;;;; crafted-lisp-config.el --- Lisp development configuration  -*- lexical-binding: t; -*-

;; Copyright (C) 2022
;; SPDX-License-Identifier: MIT

;; Author: System Crafters Community

;;; Commentary:

;; Configuration for the Lisp family of languages, including Common
;; Lisp, Clojure, Scheme, and Racket.

;; For Common Lisp, configure SLY and a few related packages.
;;    An implementation of CL will need to be installed, examples are:
;;    * CLISP (GNU Common Lisp)
;;    * CMUCL (Carnegie-Mellon Common Lisp)
;;    * SBCL (Steel-Bank Common Lisp)

;; For Clojure, configure cider, clj-refactor

;; For Scheme and Racket, configure geiser.
;;   Out of the box, geiser already supports some scheme
;;   implementations.  However, there are several modules which can be
;;   added to geiser for specific implementations:
;;   * geiser-chez
;;   * geiser-chibi
;;   * geiser-chicken
;;   * geiser-gambit
;;   * geiser-gauche
;;   * geiser-guile
;;   * geiser-kawa
;;   * geiser-mit
;;   * geiser-racket
;;   * geiser-stklos

;;; Code:

;; Global defaults
(require 'eldoc)

;; aggressive-indent-mode for all lisp modes
(when (locate-library "aggressive-indent")
  (add-hook 'lisp-mode-hook #'aggressive-indent-mode)
  (add-hook 'clojure-mode-hook #'aggressive-indent-mode)
  (add-hook 'scheme-mode-hook #'aggressive-indent-mode))


;;; Common Lisp

(with-eval-after-load 'sly
  ;; Uncomment and update if you need to set the path to an
  ;; implementation of common lisp. This would be needed only if you
  ;; have multiple instances of common lisp installed, for example,
  ;; both CLISP and SBCL. In this case, we are assuming SBCL.
  ;; (setq inferior-lisp-program "/usr/bin/sbcl")
  (require 'sly-quicklisp "sly-quicklisp" :no-error)
  (require 'sly-repl-ansi-color "sly-repl-ansi-color" :no-error)
  (require 'sly-asdf "sly-asdf" :no-error))

(when (locate-library "sly")
  (add-hook 'lisp-mode-hook #'sly-editing-mode))


;;; Clojure
(with-eval-after-load "clojure-mode"
  (require 'cider "cider" :no-error)
  (require 'clj-refactor "clj-refactor" :no-error)

  (defun crafted-lisp-load-clojure-refactor ()
    "Load `clj-refactor' toooling and fix keybinding conflicts with cider."
    (when (locate-library "clj-refactor")
      (clj-refactor-mode 1)
      ;; keybindings mentioned on clj-refactor github page
      ;; conflict with cider, use this by default as it does
      ;; not conflict and is a better mnemonic
      (cljr-add-keybindings-with-prefix "C-c r")))
  (add-hook 'clojure-mode-hook #'crafted-lisp-load-clojure-refactor)

  (with-eval-after-load "flycheck"
    (flycheck-clojure-setup)))


;;; Scheme and Racket
;; The default is "scheme" which is used by cmuscheme, xscheme and
;; chez (at least). We are configuring guile, so use the apporpriate
;; command for that implementation.
(customize-set-variable 'scheme-program-name "guile")


(provide 'crafted-lisp-config)
;;; crafted-lisp-config.el ends here
