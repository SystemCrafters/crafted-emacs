;;; crafted-erlang-config.el --- Erlang development configuration  -*- lexical-binding: t; -*-

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

(add-hook 'erlang-mode-hook 'erlang-edoc-mode)
(add-hook 'erlang-mode-hook #'eglot-ensure)

(provide 'crafted-erlang-config)
;;; crafted-erlang-config.el ends here
