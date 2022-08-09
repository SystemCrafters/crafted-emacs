;;; crafted-erlang.el --- Erlang development configuration  -*- lexical-binding: t; -*-

;; Copyright (C) 2022
;; SPDX-License-Identifier: MIT

;; Author: System Crafters Community
;; Keywords: erlang

;;; Commentary:

;; Erlang development environment configuration with LSP.

;; Emacs packages to support Erlang development:
;; * erlang        -- major-mode
;; * erlang-edoc   -- highlights keywords in comments
;; * eglot         -- language server integration

;;; Code:

(crafted-package-install-package 'erlang)
(crafted-package-install-package 'eglot)

;; Hooks

(add-hook 'erlang-mode-hook 'erlang-edoc-mode)
(add-hook 'erlang-mode-hook #'eglot-ensure)

(provide 'crafted-erlang)
;;; crafted-erlang.el ends here
