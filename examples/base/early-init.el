;;; early-init.el --- Crafted Emacs Base Example -*- lexical-binding: t; -*-

;; Copyright (C) 2023
;; SPDX-License-Identifier: MIT

;; Author: System Crafters Community

;;; Commentary:

;; Base example early-init.el
;; Modified from the info file version to be runnable.

;;; Code:

;; Set up package archives (configuring `package.el')
(load (expand-file-name "../../modules/crafted-early-init-config"
                        user-emacs-directory))
;; Adjust the path (e.g. to an absolute one)
;; depending where you cloned Crafted Emacs.
;; (load "/path/to/crafted-emacs/modules/crafted-early-init-config")

;;; _
(provide 'early-init)
;;; early-init.el ends here
