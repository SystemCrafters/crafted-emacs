;;; crafted-org-packages.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2022
;; SPDX-License-Identifier: MIT

;; Author: System Crafters Community

;; Commentary

;; Packages to augment Org mode configuration

;;; Code:

;; Second brain/zettlekasten by Protesilaos Stavrou (also known as
;; Prot), similar features as Org-Roam, but keeps everything in a
;; single directory, does not use a database preferring filenameing
;; conventions and grep instead.
(add-to-list 'package-selected-packages 'denote)

;; Toggle the visibility of some Org elements.
(add-to-list 'package-selected-packages 'org-appear)

(provide 'crafted-org-packages)
;;; crafted-org-packages.el ends here
