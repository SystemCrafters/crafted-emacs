;;;; crafted-emacs-home-config.el --- Declare the crafted-emacs-home variable  -*- lexical-binding: t; -*-

;; Copyright (C) 2023
;; SPDX-License-Identifier: MIT

;; Author: System Crafters Community

;;; Commentary:

;; This file is used to find where in the filesystem the crafted-emacs
;; code lives.  This file must be loaded before loading
;; `crafted-init-config.el'.  Loading `crafted-init-config.el' will
;; then try to find this file, and use the `project-root' to set the
;; value for the variable `crafted-emacs-home' which is then used in
;; various modules.

;;; Code:

(defvar crafted-emacs-home nil
  "Defines where the Crafted Emacs project was cloned to.

This is set when loading the crafted-init-config.el module during
initialization.  Alternatively, it can be set by the user
explicitly.")

(provide 'crafted-emacs-home-config)
;;; crafted-emacs-home-config.el ends here

