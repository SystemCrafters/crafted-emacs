;;; init.el -*- lexical-binding: t; -*-

;;; Commentary:

;; A starter config for editing Rust code.
;;
;; This configuration provides syntax highlighting via tree sitter
;; (with rust-ts-mode), LSP code completion via Eglot and Corfu, and
;; some helpful keybindings for Rust tooling.
;;
;; Prerequisites:
;;
;; - Emacs with Tree Sitter installed (this comes with Emacs 29).
;;
;; - A Rust language server (e.g. rust-analyzer).  You can install
;;   rust-analyzer via "rustup component add rust-analyzer".

;;; Code:

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (and custom-file
	   (file-exists-p custom-file))
  (load custom-file nil :nomessage))

(load "~/crafted-emacs/modules/crafted-init-config")

;;; Packages phase

(require 'crafted-completion-packages)
(require 'crafted-ide-packages)

;; Although this setup uses the tree sitter mode (rust-ts-mode) for
;; Rust language buffers, we still want to have the official rust-mode
;; around so we can use its various commands.
;; https://github.com/rust-lang/rust-mode
(add-to-list 'package-selected-packages 'rust-mode)

(package-install-selected-packages :noconfirm)

;;; Configuration phase

(require 'crafted-defaults-config)
(require 'crafted-completion-config)
(require 'crafted-ide-config)

;; Automatically register `eglot-ensure' hooks for relevant major
;; modes (notably `rust-ts-mode').
(crafted-ide-eglot-auto-ensure-all)

;; The first time you run Emacs with this enabled, the Rust tree
;; sitter parser will be installed for you automatically.
(crafted-ide-configure-tree-sitter)

;; You will probably want to tweak this variable, it determines how
;; quickly the completion prompt provides LSP suggestions when
;; typing. Be careful if you set it to 0 in a large project!
(customize-set-variable 'corfu-auto-delay 0.25)

;; Reassign the rust-mode keybindings to the rust-ts-mode map.
(with-eval-after-load 'rust-ts-mode
  (require 'rust-mode)
  (keymap-set rust-ts-mode-map "C-c C-c C-u" #'rust-compile)
  (keymap-set rust-ts-mode-map "C-c C-c C-k" #'rust-check)
  (keymap-set rust-ts-mode-map "C-c C-c C-t" #'rust-test)
  (keymap-set rust-ts-mode-map "C-c C-c C-r" #'rust-run)
  (keymap-set rust-ts-mode-map "C-c C-c C-l" #'rust-run-clippy)
  (keymap-set rust-ts-mode-map "C-c C-f" #'rust-format-buffer)
  (keymap-set rust-ts-mode-map "C-c C-n" #'rust-goto-format-problem))
