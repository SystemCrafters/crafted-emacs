;;;; crafted-package.el --- Configuration to use `package.el'.  -*- lexical-binding: t; -*-

;; Copyright (C) 2022
;; SPDX-License-Identifier: MIT

;; Author: System Crafters Community

;;; Commentary:

;; This library provides a package related interface for
;; `crafted-emacs'.

;; So far, it has two backends:
;; - `package.el' -- The default.
;; - `straight.el' -- A popular option.

;; Other backends could be added.  To add a backend, add the name to
;; the list above, provide a bootstrap file (the name must match
;; `crafted-%s-bootstrap.el' or it will fail to be loaded with this
;; code), and make sure to implement the following macros (at a
;; minimum):
;;
;; `crafted-package-install-package' which should receive a package to
;; install and perform the appropriate operations to install that
;; package.
;;
;; `crafted-package-installed-p' which should identify if a package is
;; installed (ie, it should return `t' if the package is installed and
;; `nil' otherwise)
;;
;; See the bootstrap files in this directory for examples.  The macros
;; mentioned above are intended to provide a consistent interface for
;; the modules to use when installing packages.  The user is not
;; expected to use them in their own configuration, but they may if
;; they choose.  Or they may choose a different interface, like
;; `use-package' or `leaf'.

;;; Code:

(defvar crafted-package-system 'package
  "What package system to use.

By default, it uses 'package for `package.el'.  Another option is
'straight for `straight.el'.")

(defun crafted-package-bootstrap (&optional system)
  "Load the configuration and defaults to the selected package.

This will check for the value of the variable
`crafted-package-system', but could be overriden with the
optional parameter SYSTEM.

This is called when `early-init.el' runs."
  (let* ((module (make-symbol (format "crafted-%s-bootstrap.el"
                                      (symbol-name (or system
                                                       crafted-package-system
                                                       ;; In case both above are nil
                                                       'package)))))
         (module-path (expand-file-name (symbol-name module) crafted-bootstrap-directory)))
    (if (file-exists-p module-path)
        (load module-path)
      (error "Could not find module %s" module))))

(provide 'crafted-package)

;;; crafted-package.el ends here
