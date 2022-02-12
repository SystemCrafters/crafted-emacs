;;; rational-use-package.el -*- lexical-binding: t; -*-

(straight-use-package 'use-package)

(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(provide 'rational-use-package)
