;;; crafted-completion.el --- Crafted Completion Configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2022
;; SPDX-License-Identifier: MIT

;; Author: System Crafters Community

;;; Commentary:

;; Setup completion packages.  Completion in this sense is more like
;; narrowing, allowing the user to find matches based on minimal
;; inputs and "complete" the commands, variables, etc from the
;; narrowed list of possible choices.

;;; Code:

(crafted-package-install-package 'cape)
(crafted-package-install-package 'consult)
(crafted-package-install-package 'corfu)
(crafted-package-install-package 'embark)
(crafted-package-install-package 'embark-consult)
(crafted-package-install-package 'marginalia)
(crafted-package-install-package 'orderless)
(crafted-package-install-package 'vertico)

(defun crafted-completion/minibuffer-backward-kill (arg)
  "Delete word or delete up to parent folder when completion is a file.

ARG is the thing being completed in the minibuffer."
  (interactive "p")
  (if minibuffer-completing-file-name
      ;; Borrowed from https://github.com/raxod502/selectrum/issues/498#issuecomment-803283608
      (if (string-match-p "/." (minibuffer-contents))
          (zap-up-to-char (- arg) ?/)
        (delete-minibuffer-contents))
    (backward-kill-word arg)))


;;; Vertico
(require 'vertico)

;; Straight and Package bundle the vertico package differently. When
;; using `package.el', the extensions are built into the package and
;; available on the load-path. When using `straight.el', the
;; extensions are not built into the package, so have to add that path
;; to the load-path manually to enable the following require.
(when (eq crafted-package-system 'straight)
  (add-to-list 'load-path
               (expand-file-name "straight/build/vertico/extensions"
                                 straight-base-dir)))
(require 'vertico-directory)

(with-eval-after-load 'evil
  (define-key vertico-map (kbd "C-j") 'vertico-next)
  (define-key vertico-map (kbd "C-k") 'vertico-previous)
  (define-key vertico-map (kbd "M-h") 'vertico-directory-up))

;; Cycle back to top/bottom result when the edge is reached
(customize-set-variable 'vertico-cycle t)

;; Start Vertico
(vertico-mode 1)


;;; Marginalia

;; Configure Marginalia
(require 'marginalia)
(customize-set-variable 'marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
(marginalia-mode 1)

;; Set some consult bindings
(global-set-key (kbd "C-s") 'consult-line)
(define-key minibuffer-local-map (kbd "C-r") 'consult-history)

(setq completion-in-region-function #'consult-completion-in-region)


;;; Orderless

;; Set up Orderless for better fuzzy matching
(require 'orderless)
(customize-set-variable 'completion-styles '(orderless basic))
(customize-set-variable 'completion-category-overrides '((file (styles . (partial-completion)))))


;;; Embark
(require 'embark)
(require 'embark-consult)

(global-set-key [remap describe-bindings] #'embark-bindings)
(global-set-key (kbd "C-.") 'embark-act)

;; Use Embark to show bindings in a key prefix with `C-h`
(setq prefix-help-command #'embark-prefix-help-command)

(with-eval-after-load 'embark-consult
  (add-hook 'embark-collect-mode-hook #'consult-preview-at-point-mode))


;;; Corfu
(when (eq crafted-package-system 'straight)
  (add-to-list 'load-path
               (expand-file-name "straight/build/corfu/extensions"
                                 straight-base-dir)))
(require 'corfu-popupinfo)

(require 'corfu)
;; Setup corfu for popup like completion
(customize-set-variable 'corfu-cycle t) ; Allows cycling through candidates
(customize-set-variable 'corfu-auto t)  ; Enable auto completion
(customize-set-variable 'corfu-auto-prefix 2) ; Complete with less prefix keys
(customize-set-variable 'corfu-auto-delay 0.0) ; No delay for completion
(customize-set-variable 'corfu-echo-documentation 0.25) ; Echo docs for current completion option

(global-corfu-mode 1)
(corfu-popupinfo-mode 1)
(eldoc-add-command #'corfu-insert)
(define-key corfu-map (kbd "M-p") #'corfu-popupinfo-scroll-down)
(define-key corfu-map (kbd "M-n") #'corfu-popupinfo-scroll-up)
(define-key corfu-map (kbd "M-d") #'corfu-popupinfo-toggle)


;;; Cape

;; Setup Cape for better completion-at-point support and more
(require 'cape)

;; Add useful defaults completion sources from cape
(add-to-list 'completion-at-point-functions #'cape-file)
(add-to-list 'completion-at-point-functions #'cape-dabbrev)

;; Silence the pcomplete capf, no errors or messages!
;; Important for corfu
(advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)

;; Ensure that pcomplete does not write to the buffer
;; and behaves as a pure `completion-at-point-function'.
(advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify)
(add-hook 'eshell-mode-hook
          (lambda () (setq-local corfu-quit-at-boundary t
                            corfu-quit-no-match t
                            corfu-auto nil)
            (corfu-mode)))

(provide 'crafted-completion)
;;; crafted-completion.el ends here
