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
(defun crafted-ide--configure-tree-sitter-pre-29 ()
  "Configure tree-sitter for Emacs 28 or earlier."

  (defun crafted-tree-sitter-load (lang-symbol)
    "Setup tree-sitter for a language.

This must be called in the user's configuration to configure
tree-sitter for LANG-SYMBOL.

Example: `(crafted-tree-sitter-load 'python)'"
    (tree-sitter-require lang-symbol)
    (let ((mode-hook-name
           (intern (format "%s-mode-hook" (symbol-name lang-symbol)))))
      (add-hook mode-hook-name #'tree-sitter-mode))))

(defun crafted-ide--configure-tree-sitter (opt-out)
  "Configure tree-sitter for Emacs 29 or later.
OPT-OUT is a list of symbols of language grammars to opt out before auto-install."
  ;; only attempt to use tree-sitter when Emacs was built with it.
  (when (member "TREE_SITTER" (split-string system-configuration-features))
    (when (require 'treesit-auto nil :noerror)
      ;; add all items of opt-out to the `treesit-auto-opt-out-list'.
      (when opt-out
        (mapc (lambda (e) (add-to-list 'treesit-auto-opt-out-list e)) opt-out))
      ;; prefer tree-sitter modes
      (global-treesit-auto-mode)
      ;; install all the tree-sitter grammars
      (treesit-auto-install-all)
      ;; configure `auto-mode-alist' for tree-sitter modes relying on
      ;; `fundamental-mode'
      (crafted-ide--configure-tree-sitter-modes))
    (when (locate-library "combobulate")
      ;; perhaps too gross of an application, but the *-ts-modes
      ;; eventually derive from this mode.
      (add-hook 'prog-mode-hook #'combobulate-mode))))

(defvar crafted-ide-tree-sitter-auto-mode-alist
  '((typescript . ("\\.ts\\'" . typescript-ts-mode))
    (tsx . ("\\.tsx\\'" . tsx-ts-mode))
    (rust . ("\\.rs\\'" . rust-ts-mode))
    (go . ("\\.go\\'" . go-ts-mode))
    (gomod . ("go.mod" . go-mod-ts-mode))
    (yaml . ("\\.ya?ml\\'" . yaml-ts-mode))
    (julia . ("\\.jl\\'" . julia-ts-mode))
    (lua . ("\\.lua\\'" . lua-ts-mode))
    (markdown . ("\\.md\\'" . markdown-ts-mode))
    (dockerfile . ("Dockerfile\\'" . dockerfile-ts-mode)))
  "Map of tree-sitter languages to `auto-mode-alist' entries.")

(defun crafted-ide--configure-tree-sitter-modes ()
  "Add tree-sitter modes to `auto-mode-alist'.

`global-treesit-auto-mode' only remaps major modes to their
tree-sitter equivalent if a prerequisite major mode is active.
Oftentimes that major mode requires installing an additional
package, since many languages are only supported by
`fundamental-mode' out of the box.  This function registers
`auto-mode-alist' entries for all tree-sitter parsers that are
installed but lack the required remap mode."
  (let ((recipes-lacking-modes
         (seq-filter
          (lambda (recipe)
            (and
             (treesit-ready-p (treesit-auto-recipe-lang recipe) t)
             (not (crafted-ide--tree-sitter-recipe-bound-p recipe))))
          treesit-auto-recipe-list)))
    (dolist (recipe recipes-lacking-modes)
      (when-let ((automode (alist-get (treesit-auto-recipe-lang recipe)
                                      crafted-ide-tree-sitter-auto-mode-alist)))
        (add-to-list 'auto-mode-alist automode)))))

(defun crafted-ide--tree-sitter-recipe-bound-p (recipe)
  "Return t if RECIPE remap function definition is not void."
  (when-let ((remap (treesit-auto-recipe-remap recipe)))
    (if (listp remap)
        (seq-some (lambda (mode) (fboundp mode)) remap)
      (fboundp remap))))

(defun crafted-ide-configure-tree-sitter (&optional opt-out)
  "Configure tree-sitter.
Requires a C compiler (gcc, cc, c99) installed on the system.
Note that OPT-OUT only affects setups with Emacs 29 or later.

For Emacs 29 or later:
Requires Emacs to be built using \"--with-tree-sitter\".
All language grammars are auto-installed unless they are a member of OPT-OUT."
  (if (version< emacs-version "29")
      (crafted-ide--configure-tree-sitter-pre-29)
    (crafted-ide--configure-tree-sitter opt-out)))

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
