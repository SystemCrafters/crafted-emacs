(straight-use-package 'vertico)
(straight-use-package 'consult)
(straight-use-package 'orderless)
(straight-use-package 'marginalia)
(straight-use-package 'embark)

(defun rational-completion/minibuffer-backward-kill (arg)
  "When minibuffer is completing a file name delete up to parent
folder, otherwise delete a word"
  (interactive "p")
  (if minibuffer-completing-file-name
      ;; Borrowed from https://github.com/raxod502/selectrum/issues/498#issuecomment-803283608
      (if (string-match-p "/." (minibuffer-contents))
          (zap-up-to-char (- arg) ?/)
        (delete-minibuffer-contents))
    (backward-kill-word arg)))

(require 'vertico)

;; TODO Should these bindings be moved to another module?
(define-key vertico-map (kbd "C-j") 'vertico-next)
(define-key vertico-map (kbd "C-k") 'vertico-previous)
(define-key vertico-map (kbd "C-f") 'vertico-exit)

;; Add a key binding for killing backward to go up parent directories, etc
(define-key minibuffer-local-map (kbd "M-h") 'rational-completion/minibuffer-backward-kill)

;; Cycle back to top/bottom result when the edge is reached
(setq vertico-cycle t)

;; Start Vertico
(vertico-mode 1)

;; Configure Marginalia
(require 'marginalia)
(setq marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
(marginalia-mode 1)

;; Set some consult bindings
(global-set-key (kbd "C-s") 'consult-line)
(define-key minibuffer-local-map (kbd "C-r") 'consult-history)

(setq completion-in-region-function #'consult-completion-in-region)

;; Set up Orderless for better fuzzy matching
(require 'orderless)
(setq completion-styles '(orderless)
      completion-category-defaults nil
      completion-category-overrides '((file (styles . (partial-completion)))))

;(global-set-key (kbd "C-S-a") 'embark-act)
;(define-key minibuffer-local-map (kbd "C-d") 'embark-act)

;; Use Embark to show bindings in a key prefix with `C-h`
(setq prefix-help-command #'embark-prefix-help-command)

(provide 'rational-completion)
