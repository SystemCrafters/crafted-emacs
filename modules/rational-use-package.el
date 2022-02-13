;;; rational-use-package.el -*- lexical-binding: t; -*-

(straight-use-package 'use-package)

(require 'package)

;; gnu and nongnu is needed for packages like Org-mode
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")))
(add-to-list package-archives
	     '("nongnu" . "https://elpa.nongnu.org/nongnu") 'append)
(add-to-list package-archives
	     '("melpa" . "https://melpa.org/packages/") 'append)
(add-to-list package-archives
	     '("elpa" . "https://elpa.gnu.org/packages/") 'append)

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(provide 'rational-use-package)
