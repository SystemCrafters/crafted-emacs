;;;; rational-modeline.el --- rational module for setting up the modeline  -*- lexical-binding: t; -*-

;; Copyright (C) 2022
;; SPDX-License-Identifier: MIT

;; Authors: System Crafters Community, David "System Crafters" Wilson, Erik Lundstedt

;; Commentary

;;

;;; Code:

(defcustom rational-modeline-type "telephone"
  "one of 'GNU' 'doom' 'telephone' ";;'space WIP' "
  )


(when (string-equal rational-modeline-type "GNU")
  ;;TODO make it look as good as possible without external packages
  )




(when (string-equal rational-modeline-type "doom")
  (straight-use-package 'doom-modeline)
  (add-hook 'after-init-hook 'doom-modeline-init)
  (customize-set-variable 'doom-modeline-bar-width 6)
  (customize-set-variable 'doom-modeline-buffer-file-name-style 'truncate-except-project)
  (customize-set-variable 'doom-modeline-height 15)
  (customize-set-variable 'doom-modeline-minor-modes t)
  )






(when (string-equal rational-modeline-type "telephone")
  ;;(or "telephoneline" "telephone-line" "phone-line" "phoneline" "telephone"))


  (defun telephoneline-examples-abs ()
    (setq telephone-line-lhs
          '((evil   . (telephone-line-evil-tag-segment))
            (accent . (telephone-line-vc-segment
                       telephone-line-erc-modified-channels-segment
                       telephone-line-process-segment))
            (nil    . (telephone-line-minor-mode-segment
                       telephone-line-buffer-segment))))
    (setq telephone-line-rhs
          '((nil    . (telephone-line-misc-info-segment))
            (accent . (telephone-line-major-mode-segment))
            (evil   . (telephone-line-airline-position-segment))))
    )

  (defun telephoneline-examples-abs-right ()
    (setq telephone-line-primary-right-separator 'telephone-line-abs-left
          telephone-line-secondary-right-separator 'telephone-line-abs-hollow-left)
    (setq telephone-line-height 24
          telephone-line-evil-use-short-tag t)
    )

  (defun telephoneline-examples-gradient ()
    (setq telephone-line-primary-left-separator 'telephone-line-gradient
          telephone-line-secondary-left-separator 'telephone-line-nil
          telephone-line-primary-right-separator 'telephone-line-gradient
          telephone-line-secondary-right-separator 'telephone-line-nil)
    (setq telephone-line-height 24
          telephone-line-evil-use-short-tag t)
    )

  (defun telephoneline-examples-cubed ()
    (setq telephone-line-primary-left-separator 'telephone-line-cubed-left
          telephone-line-secondary-left-separator 'telephone-line-cubed-hollow-left
          telephone-line-primary-right-separator 'telephone-line-cubed-right
          telephone-line-secondary-right-separator 'telephone-line-cubed-hollow-right)
    (setq telephone-line-height 24
          telephone-line-evil-use-short-tag nil)
    )


  (defun telephoneline-examples-subsep-colour ()
    (setq telephone-line-subseparator-faces '())
    (setq telephone-line-height 24
          telephone-line-evil-use-short-tag t)
    )

  (defun telephoneline-examples-subsep-color ()
    (telephoneline-examples-susbsep-colour)
    )





  (straight-use-package 'telephone-line)


  (defcustom rational-modeline-telephoneline-style 'telephoneline-examples-cubed
    "function to run before enabling telephone-line-mode"
    )




  (funcall rational-modeline-telephoneline-style)
  ;; TODO allow for mode customization
  ;;(telephoneline-examples-cubed)

  ;; Start up the modeline after initialization is finished
  (add-hook 'after-init-hook (telephone-line-mode t))


  )


(provide 'rational-modeline)
;;; rational-modeline.el ends here
