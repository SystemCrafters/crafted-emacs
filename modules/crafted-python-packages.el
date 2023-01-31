;;; crafted-python-packages.el --- python configuration      -*- lexical-binding: t; -*-

;; Copyright (C) 2022
;; SPDX-License-Identifier: MIT

;; Author: System Crafters Community
;; Keywords: python

;;; Commentary:

;; Packages to support python development.

;;; Code:

(add-to-list 'package-selected-packages 'anaconda-mode)
(add-to-list 'package-selected-packages 'blacken)
(add-to-list 'package-selected-packages 'eglot)
(add-to-list 'package-selected-packages 'numpydoc)
(add-to-list 'package-selected-packages 'pyvenv)

(provide 'crafted-python-packages)
;;; crafted-python-packages.el ends here
