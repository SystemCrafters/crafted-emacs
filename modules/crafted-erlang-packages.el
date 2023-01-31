;;; crafted-erlang-packages.el --- Erlang development packages  -*- lexical-binding: t; -*-

;; Copyright (C) 2022
;; SPDX-License-Identifier: MIT

;; Author: System Crafters Community
;; Keywords: erlang

;;; Commentary:

;; Packages for Erlang development environment.

;;; Code:

(add-to-list 'package-selected-packages 'erlang)

;; Only need to install eglot if using Emacs prior to version 29.
(unless (version< emacs-version "29")
  (add-to-list 'package-selected-packages 'eglot))

(provide 'crafted-erlang-packages)
;;; crafted-erlang-packages.el ends here
