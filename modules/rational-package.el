;;;; rational-package.el --- Configuration to use `package.el'.  -*- lexical-binding: t; -*-

;; Copyright (C) 2022
;; SPDX-License-Identifier: MIT

;; Author: System Crafters Community

;;; Commentary:

;; This library helps to the user to select a package manager backend
;; for `rational-emacs'.

;; So far, it has two backends:
;; - `package.el' -- The default.
;; - `straight.el' -- A popular option.

;; Other backends could be added.

;;; Code:

(defvar rational-package-system 'package
  "What package system to use.

By default, it uses 'package for `package.el'.  Another option is
'straight for `staright.el'.")

(defun rational-package-initialize (&optional system)
  "Loads the configuration and defaults to the selected package.

This will check for the value of the variable
`rational-package-system', but could be overriden with the
optional parameter SYSTEM."
  (let ((module (make-symbol (format "rational-package/%s"
                                (symbol-name (or system
                                                 rational-package-system
                                                 ;; In case both above are nil
                                                 'package))))))
    (if (locate-library (symbol-name module))
        (require module)
      (error "Could not find module %s" module)
      )))

(provide 'rational-package)
;;; rational-packages.el ends here
