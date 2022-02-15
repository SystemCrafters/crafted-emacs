;;; rational-screencast.el -*- lexical-binding: t; -*-

(straight-use-package 'keycast)

(customize-set-variable 'keycast-remove-tail-elements nil)
(customize-set-variable 'keycast-insert-after 'mode-line-misc-info)
(keycast-mode)

(provide 'rational-screencast)
