;;;; crafted-lisp.el --- Lisp development configuration  -*- lexical-binding: t; -*-

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
;; keeps code indented even when copy/pasting.
(crafted-package-install-package 'aggressive-indent)



;;; Common Lisp
(crafted-package-install-package 'sly)
(crafted-package-install-package 'sly-asdf)
(crafted-package-install-package 'sly-quicklisp)
(crafted-package-install-package 'sly-repl-ansi-color)

(with-eval-after-load 'sly
  ;; Uncomment and update if you need to set the path to an
  ;; implementation of common lisp. This would be needed only if you
  ;; have multiple instances of common lisp installed, for example,
  ;; both CLISP and SBCL. In this case, we are assuming SBCL.
  ;; (setq inferior-lisp-program "/usr/bin/sbcl")
  (require 'sly-quicklisp)
  (require 'sly-repl-ansi-color)
  (require 'sly-asdf))

(add-hook 'lisp-mode-hook #'sly-editing-mode)
(add-hook 'lisp-mode-hook #'aggressive-indent-mode)


;;; Clojure
(crafted-package-install-package 'cider)
(crafted-package-install-package 'clj-refactor)
(crafted-package-install-package 'clojure-mode)
(crafted-package-install-package 'flycheck-clojure)

(with-eval-after-load "clojure-mode"
  (require 'cider)
  (require 'clj-refactor)
  (add-hook 'clojure-mode-hook
            (lambda ()
              (clj-refactor-mode 1)
              ;; keybindings mentioned on clj-refactor github page
              ;; conflict with cider, use this by default as it does
              ;; not conflict and is a better mnemonic
              (cljr-add-keybindings-with-prefix "C-c r")))

  (with-eval-after-load "flycheck"
    (flycheck-clojure-setup)))

(add-hook 'clojure-mode #'aggressive-indent-mode)


;;; Scheme and Racket
;; As we wish to make Crafted Emacs work well with GNU Guix, we start
;; the configuration with the GNU Guile loaded. Additional
;; implementations can be added by installing the appropriate package
;; and (possibly) adding that implementation to the list of geiser
;; active implmentations. Racket configuration is also provided out of
;; the box.

(crafted-package-install-package 'geiser)
(crafted-package-install-package 'geiser-guile)
(crafted-package-install-package 'geiser-racket)
(add-hook 'scheme-mode #'aggressive-indent-mode)

;; The default is "scheme" which is used by cmuscheme, xscheme and
;; chez (at least). We are configuring guile, so use the apporpriate
;; command for that implementation.
(customize-set-variable 'scheme-program-name "guile")


(provide 'crafted-lisp)
;;; crafted-lisp.el ends here
