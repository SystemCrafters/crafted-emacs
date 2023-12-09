;;; init.el --- Crafted Emacs Elpaca Example -*- lexical-binding: t; -*-

;; Copyright (C) 2023
;; SPDX-License-Identifier: MIT

;; Author: System Crafters Community

;;; Commentary:

;; Example init.el for using the Elpaca package manager.

;;; Code:

;;; Initial phase
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (and custom-file
           (file-exists-p custom-file))
  (load custom-file nil :nomessage))

(load (expand-file-name "../../modules/crafted-init-config"
                        user-emacs-directory))

;;; Packages phase
(require 'crafted-completion-packages)

;; Use the crafted-package helper and wait for elpaca to finish installing everything
(crafted-package-install-selected-packages)
(elpaca-wait)

;;; Configuration phase
(require 'crafted-completion-config)
(require 'crafted-defaults-config)
(require 'crafted-startup-config)

;;; _
(provide 'init)
;;; init.el ends here
