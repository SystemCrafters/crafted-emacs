;;; init.el -*- lexical-binding: t; -*-

;;; Commentary:

;; A starter config for editing Rust code.

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
;; modes. Note that you'll need rust-analyzer installed beforehand for
;; Eglot to work properly.
;;
;; e.g. "rustup component add rust-analyzer"
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
  (define-key rust-ts-mode-map (kbd "C-c C-c C-u") #'rust-compile)
  (define-key rust-ts-mode-map (kbd "C-c C-c C-k") #'rust-check)
  (define-key rust-ts-mode-map (kbd "C-c C-c C-t") #'rust-test)
  (define-key rust-ts-mode-map (kbd "C-c C-c C-r") #'rust-run)
  (define-key rust-ts-mode-map (kbd "C-c C-c C-l") #'rust-run-clippy)
  (define-key rust-ts-mode-map (kbd "C-c C-f") #'rust-format-buffer)
  (define-key rust-ts-mode-map (kbd "C-c C-n") #'rust-goto-format-problem))
