;;; rational-ui.el -*- lexical-binding: t; -*-

(defcustom rational-ui-default-font nil
  "The configuration of the `default' face, uses a plist with
the same key names as accepted by `set-face-attribute'")

(when rational-ui-default-font
  (apply 'set-face-attribute 'default nil (cons :font rational-ui-default-font)))

(straight-use-package 'all-the-icons)
(straight-use-package 'doom-modeline)
(straight-use-package 'doom-themes)
(straight-use-package 'helpful)

;; Start up the modeline after initialization is finished
(add-hook 'after-init-hook 'doom-modeline-init)

;; Configure `doom-modeline'
(setq doom-modeline-height 15
      doom-modeline-bar-width 6
      doom-modeline-minor-modes t
      doom-modeline-buffer-file-name-style 'truncate-except-project)

;; Make `describe-*' screens more helpful!
(require 'helpful)
(global-set-key [remap describe-function] #'helpful-callable)
(global-set-key [remap describe-variable] #'helpful-variable)
(global-set-key [remap describe-key] #'helpful-key)
(global-set-key (kbd "C-h F") #'helpful-function)
(global-set-key (kbd "C-h C") #'helpful-command)

(provide 'rational-ui)
