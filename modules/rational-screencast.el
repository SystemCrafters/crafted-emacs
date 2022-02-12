;;; rational-screencast.el -*- lexical-binding: t; -*-

(straight-use-package 'keycast)

(setq keycast-remove-tail-elements nil)
(setq keycast-insert-after 'mode-line-misc-info)
(keycast-mode)

(provide 'rational-screencast)
