;; crafted-ide-config.el -*- lexical-binding: t; -*-

;; Copyright (C) 2022
;; SPDX-License-Identifier: MIT

;; Author: System Crafters Community

;; Commentary

;; Eglot configuration.

;; Suggested additional keybindings
;; (with-eval-after-load "prog-mode"
;;   (define-key prog-mode-map (kbd "C-c e n") #'flymake-goto-next-error)
;;   (define-key prog-mode-map (kbd "C-c e p") #'flymake-goto-prev-error))

;;; Code:


;;; Eglot
(defun crafted-ide--add-eglot-hooks (mode-list)
  "Iterates over MODE-LIST recursively to add eglot-ensure to
existing mode hooks.

The mode must be loaded, ie. found with `fboundp'. A mode which
is not loaded will not have a hook added, in which case add it
manually with something like this:

`(add-hook 'some-mode-hook #'eglot-ensure)'
"
  (dolist (mode-def mode-list)
    (let ((mode (if (listp mode-def) (car mode-def) mode-def)))
      (cond
       ((listp mode) (crafted-ide--add-eglot-hooks mode))
       (t
        (when (and (fboundp mode)
                   (not (eq 'clojure-mode mode))  ; prefer cider
                   (not (eq 'lisp-mode mode))     ; prefer sly/slime
                   (not (eq 'scheme-mode mode))   ; prefer geiser
                   )
          (let ((hook-name (concat (symbol-name mode) "-hook")))
            (message (concat "adding eglot to " hook-name))
            (add-hook (intern hook-name) #'eglot-ensure))))))))

;; add eglot to existing programming modes when eglot is loaded.
(with-eval-after-load "eglot"
  (crafted-ide--add-eglot-hooks eglot-server-programs))

;; Shutdown server when last managed buffer is killed
(customize-set-variable 'eglot-autoshutdown t)

;;; tree-sitter
;; Emacs versions prior to 29
(when (version< emacs-version "29")
  (when (featurep 'tree-sitter-langs)
    (require 'tree-sitter-indent nil :noerror)

    (defun crafted-tree-sitter-load (lang-symbol)
      "Setup tree-sitter for a language.

This must be called in the user's configuration to configure
tree-sitter for LANG-SYMBOL. 

Example: `(crafted-tree-sitter-load 'python)'"
      (tree-sitter-require lang-symbol) 
      (let ((mode-hook-name
             (intern (concat (symbol-name lang-symbol) "-mode-hook"))))
        (add-hook mode-hook-name #'tree-sitter-mode)))))

;; Emacs versions after 29
(when (>= (string-to-number emacs-version) 29)
  ;; only attempt to use tree-sitter when Emacs was built with it.
  (when (and (member "TREE_SITTER" (split-string system-configuration-features))
             (executable-find "tree-sitter"))
    (when (featurep 'treesit-auto)
      ;; prefer tree-sitter modes
      (global-treesit-auto-mode)
      (with-eval-after-load 'treesit-auto
        ;; install all the tree-sitter grammars
        (treesit-auto-install-all)))
    (when (featurep 'combobulate)
      ;; perhaps too gross of an application, but the *-ts-modes
      ;; eventually derive from this mode.
      (add-hook 'prog-mode-hook #'combobulate-mode))))

(provide 'crafted-ide-config)
;;; crafted-ide-config.el ends here
