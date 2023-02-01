;;;; crafted-init-config.el --- Crafted Emacs initial configuration  -*- lexical-binding: t; -*-

;; Copyright (C) 2023
;; SPDX-License-Identifier: MIT

;; Author: System Crafters Community

;;; Commentary:

;; Perform some initialization for use by Crafted Emacs modules among
;; other things.

;;; Code:

;; Use this file to find the project root where you cloned
;; `crafted-emacs' and use that as the value for the
;; `crafted-emacs-home' value which is needed by a few modules,
;; including the template below used for writing Crafted Emacs
;; modules.

(require 'project)

(message (format "symbol-file 'crafted-emacs-home: %s"
		 (symbol-file 'crafted-emacs-home)))

(setq crafted-emacs-home
      (expand-file-name
       (project-root 
	(project-current nil (file-name-directory
			      (symbol-file 'crafted-emacs-home))))))

;; update the `load-path' to include the Crafted Emacs modules path

(let ((modules (expand-file-name "./modules/" crafted-emacs-home)))
  (when (file-directory-p modules)
    (message (concat "adding modules to load-path: " modules))
    (add-to-list 'load-path modules)))

;; When writing crafted-modules, insert header from skeleton
(auto-insert-mode)
(with-eval-after-load "autoinsert"
  (define-auto-insert
    (cons (concat (expand-file-name crafted-emacs-home) "modules/crafted-.*\\.el")
          "Crafted Emacs Lisp Skeleton")
    '("Crafted Emacs Module Description: "
      ";;;; " (file-name-nondirectory (buffer-file-name)) " --- " str
      (make-string (max 2 (- 80 (current-column) 27)) ?\s)
      "-*- lexical-binding: t; -*-" '(setq lexical-binding t)
      "

;; Copyright (C) " (format-time-string "%Y") "
;; SPDX-License-Identifier: MIT

;; Author: System Crafters Community

;;; Commentary:

;; " _ "

;;; Code:

(provide '"
      (file-name-base (buffer-file-name))
      ")
;;; " (file-name-nondirectory (buffer-file-name)) " ends here\n")))

;; Add the Crafted Emacs documentation to the info nodes
(let ((crafted-info-dir (expand-file-name "docs/dir" crafted-emacs-home)))
  (when (file-exists-p crafted-info-dir)
    (require 'info)
    (info-initialize)
    (push (file-name-directory crafted-info-dir) Info-directory-list)))

(provide 'crafted-init-config)
;;; crafted-init-config.el ends here
