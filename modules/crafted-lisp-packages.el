;;;; crafted-lisp-packages.el --- Lisp development packages  -*- lexical-binding: t; -*-

;; Copyright (C) 2022
;; SPDX-License-Identifier: MIT

;; Author: System Crafters Community

;;; Commentary:

;; Packages used with the crafted-lisp-config.

;;; Code:

;; Indentation

(add-to-list 'package-selected-packages 'aggressive-indent)


;; Common Lisp

(add-to-list 'package-selected-packages 'sly)
(add-to-list 'package-selected-packages 'sly-asdf)
(add-to-list 'package-selected-packages 'sly-quicklisp)
(add-to-list 'package-selected-packages 'sly-repl-ansi-color)

;; Clojure
(add-to-list 'package-selected-packages 'cider)
(add-to-list 'package-selected-packages 'clj-refactor)
(add-to-list 'package-selected-packages 'clojure-mode)
(add-to-list 'package-selected-packages 'flycheck-clojure)

;; Scheme and Racket
(add-to-list 'package-selected-packages 'geiser)
(add-to-list 'package-selected-packages 'geiser-guile)
(add-to-list 'package-selected-packages 'geiser-racket)

(provide 'crafted-lisp-packages)
;;; crafted-lisp-packages.el ends here
