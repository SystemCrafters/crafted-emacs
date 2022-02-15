;;; rational-use-package.el -*- lexical-binding: t; -*-

(straight-use-package 'use-package)

(require 'package)

;; Emacs 27.x has the gnu elpa as the default for `package-archives'
;; Emacs 28.x has the gnu and nongnu elpa as the default.
;; Only add nongnu elpa for Emacs version < 28
(when (version< emacs-version "28.0")
  (add-to-list 'package-archives (cons "nongnu" "https://elpa.nongnu.org/nongnu/")))

;; Always add melpa to `package-archives'
(add-to-list 'package-archives (cons "melpa" "https://melpa.org/packages/"))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(provide 'rational-use-package)
