;;;; crafted-package.el --- Configuration to use `package.el'.  -*- lexical-binding: t; -*-

;; Copyright (C) 2022
;; SPDX-License-Identifier: MIT

;; Author: System Crafters Community

;;; Commentary:

;; This library helps to the user to select a package manager backend
;; for `crafted-emacs'.

;; So far, it has two backends:
;; - `package.el' -- The default.
;; - `straight.el' -- A popular option.

;; Other backends could be added.

;; This still needs to manage packages installed using `guix' (or
;; other system-wide methods).

;; For `guix' see Androew Tropin's video on the theme:
;; <https://youtu.be/gqmZjovuomc>

;;; Code:

(defvar crafted-package-system 'package
  "What package system to use.

By default, it uses 'package for `package.el'.  Another option is
'straight for `straight.el'.")

(defun crafted-package-bootstrap (&optional system)
  "Loads the configuration and defaults to the selected package.

This will check for the value of the variable
`crafted-package-system', but could be overriden with the
optional parameter SYSTEM."
  (let* ((module (make-symbol (format "%s-bootstrap.el"
                                      (symbol-name (or system
                                                       crafted-package-system
                                                       ;; In case both above are nil
                                                       'package)))))
         (module-path (expand-file-name (symbol-name module) crafted-boostrap-directory)))
    (if (file-exists-p module-path)
        (load module-path)
      (error "Could not find module %s" module))))

(provide 'crafted-package)
;;; crafted-packages.el ends here
