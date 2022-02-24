;;; rational-code-completion.el -*- lexical-binding: t; -*-

;; Copyright (C) 2022
;; SPDX-License-Identifier: MIT

;; Author: System Crafters Community

;; Commentary

;; Setup code completion packages. Code completion is done with corfu
;; rather than the traditional company to better utilize native
;; completion features provided by emacs.

;;; Code:

(straight-use-package 'corfu)
(straight-use-package 'cape)
(straight-use-package 'orderless)

(require 'corfu)

(defcustom rational-code-completion-ycm-style nil
  "Make corfu act closer to YouCompleteMe of Vim fame."
  :type 'boolean)

(when rational-code-completion-ycm-style
  (setq corfu-quit-at-boundary nil
        corfu-preselect-first nil)
  (define-key corfu-map (kbd "RET") nil) ;; Don't overwrite the enter key
  (define-key corfu-map (kbd "S-RET") 'corfu-insert))

(setq
  corfu-cycle t                    ; Allows cycling through candidates
  corfu-auto t                     ; Enable auto completion
  corfu-auto-prefix 2              ; Complete with less prefix keys
  corfu-auto-delay 0.0             ; No delay for completion
  corfu-echo-documentation 0.25    ; Echo docs for current completion option
  )

(define-key corfu-map (kbd "TAB") 'corfu-next)
(define-key corfu-map [tab] 'corfu-next)
(define-key corfu-map (kbd "S-TAB") 'corfu-previous)
(define-key corfu-map [backtab] 'corfu-previous)

(corfu-global-mode 1)

;; Set up Orderless for better fuzzy matching
(require 'orderless)
(customize-set-variable 'completion-styles '(orderless))
(customize-set-variable 'completion-category-overrides '((file (styles . (partial-completion)))))
(setq completion-category-defaults nil)

;; Setup cape for additional completion functionality and compatability

(add-to-list 'completion-at-point-functions #'cape-file)
(add-to-list 'completion-at-point-functions #'cape-dabbrev)
(require 'cape)
;; Silence the pcomplete capf, no errors or messages!
(advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)

;; Ensure that pcomplete does not write to the buffer
;; and behaves as a pure `completion-at-point-function'.
(advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify)
(add-hook 'eshell-mode-hook
          (lambda () (setq-local corfu-quit-at-boundary t
                            corfu-quit-no-match t
                            corfu-auto nil)
            (corfu-mode)))


(provide 'rational-code-completion)
;;; rational-code-completion.el ends here
