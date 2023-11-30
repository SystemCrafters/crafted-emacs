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

(defun crafted-ide--lsp-bin-exists-p (mode-def)
  "Return non-nil if LSP binary of MODE-DEF is found via `executable-find'."
  (let ((lsp-program (cdr mode-def)))
    ;; `lsp-program' is either a list of strings or a function object
    ;; calling `eglot-alternatives'.
    (if (functionp lsp-program)
        (condition-case nil
            (car (funcall lsp-program))
          ;; When an error occurs it's because Eglot checked for a
          ;; binary and didn't find one among alternatives.
          (error nil))
      (executable-find (car lsp-program)))))

(defun crafted-ide-eglot-auto-ensure-all ()
  "Add `eglot-ensure' to major modes that offer LSP support.

Major modes are only selected if the major mode's associated LSP
binary is detected on the system."
  (when (require 'eglot nil :noerror)
    (crafted-ide--add-eglot-hooks (seq-filter
                                   #'crafted-ide--lsp-bin-exists-p
                                   eglot-server-programs))))

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
      (treesit-auto-add-to-auto-mode-alist))
    (when (locate-library "combobulate")
      ;; perhaps too gross of an application, but the *-ts-modes
      ;; eventually derive from this mode.
      (add-hook 'prog-mode-hook #'combobulate-mode))))

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
