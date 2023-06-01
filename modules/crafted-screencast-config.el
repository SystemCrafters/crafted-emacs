;;; crafted-screencast.el -*- lexical-binding: t; -*-

;; Copyright (C) 2022
;; SPDX-License-Identifier: MIT

;; Author: System Crafters Community

;;; Commentary:

;; Screencast configuration

;;; Code:

(when (featurep 'keycast)
  (customize-set-variable 'keycast-remove-tail-elements nil)
  (customize-set-variable 'keycast-insert-after 'mode-line-misc-info)
  (keycast-mode))

(provide 'crafted-screencast-config)
;;; crafted-screencast.el ends here
