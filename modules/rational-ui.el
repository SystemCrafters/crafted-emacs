(straight-use-package 'all-the-icons)
(straight-use-package 'doom-modeline)
(straight-use-package 'doom-themes)
(straight-use-package 'which-key)

;; Make sure all-the-icons is set up

;; Start up the modline after initialization is finished
(add-hook 'after-init-hook 'doom-modeline-init)

(setq doom-modeline-height 15
      doom-modeline-bar-width 6
      doom-modeline-minor-modes t
      doom-modeline-buffer-file-name-style 'truncate-except-project)

(setq which-key-idle-delay 0.3)
;(which-key-mode 1)

(provide 'rational-ui)
