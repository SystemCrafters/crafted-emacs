;;; crafted-ide-config.el --- Provide IDE-like features -*- lexical-binding: t; -*-

;; Copyright (C) 2023
;; SPDX-License-Identifier: MIT

;; Author: System Crafters Community

;;; Commentary:

;; Eglot configuration.

;; Suggested additional keybindings
;; (with-eval-after-load "prog-mode"
;;   (keymap-set prog-mode-map "C-c e n" #'flymake-goto-next-error)
;;   (keymap-set prog-mode-map "C-c e p" #'flymake-goto-prev-error))

;;; Code:


;;; Eglot
(defun crafted-ide--add-eglot-hooks (mode-list)
  "Add `eglot-ensure' to modes in MODE-LIST.

The mode must be loaded, i.e. found with `fboundp'.  A mode which
is not loaded will not have a hook added, in which case add it
manually with something like this:

`(add-hook 'some-mode-hook #'eglot-ensure)'"
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
          (let ((hook-name (format "%s-hook" (symbol-name mode))))
            (message "adding eglot to %s" hook-name)
            (add-hook (intern hook-name) #'eglot-ensure))))))))

;; add eglot to existing programming modes when eglot is loaded.
(with-eval-after-load "eglot"
  (crafted-ide--add-eglot-hooks eglot-server-programs))

;; Shutdown server when last managed buffer is killed
(customize-set-variable 'eglot-autoshutdown t)

;;; tree-sitter
;; Emacs versions prior to 29
(when (version< emacs-version "29")
  (when (require 'tree-sitter-indent nil :noerror)

    (defun crafted-tree-sitter-load (lang-symbol)
      "Setup tree-sitter for a language.

This must be called in the user's configuration to configure
tree-sitter for LANG-SYMBOL.

Example: `(crafted-tree-sitter-load 'python)'"
      (tree-sitter-require lang-symbol)
      (let ((mode-hook-name
             (intern (format "%s-mode-hook" (symbol-name lang-symbol)))))
        (add-hook mode-hook-name #'tree-sitter-mode)))))

;; Emacs versions after 29
(when (version< "29" emacs-version)
  ;; only attempt to use tree-sitter when Emacs was built with it.
  (when (member "TREE_SITTER" (split-string system-configuration-features))
    (when (require 'treesit-auto nil :noerror)
      ;; prefer tree-sitter modes
      (global-treesit-auto-mode)
      ;; install all the tree-sitter grammars
      (treesit-auto-install-all))
    (when (locate-library "combobulate")
      ;; perhaps too gross of an application, but the *-ts-modes
      ;; eventually derive from this mode.
      (add-hook 'prog-mode-hook #'combobulate-mode))))


;; turn on aggressive indent if it is available, otherwise use
;; electric indent.
(if (require 'aggressive-indent nil :noerror)
    (add-hook 'prog-mode-hook #'aggressive-indent-mode)
  (add-hook 'prog-mode-hook #'electric-indent-mode))

;; turn on editorconfig if it is available
(when (require 'editorconfig nil :noerror)
  (add-hook 'prog-mode-hook #'editorconfig-mode))

;; enhance ibuffer with ibuffer-project if it is available.
(when (require 'ibuffer-project nil :noerror)
  (defun crafted-ide-enhance-ibuffer-with-ibuffer-project ()
    "Set up integration for `ibuffer' with `ibuffer-project'."
    (setq ibuffer-filter-groups (ibuffer-project-generate-filter-groups))
    (unless (eq ibuffer-sorting-mode 'project-file-relative)
      (ibuffer-do-sort-by-project-file-relative)))
  (add-hook 'ibuffer-hook #'crafted-ide-enhance-ibuffer-with-ibuffer-project))

(provide 'crafted-ide-config)
;;; crafted-ide-config.el ends here
