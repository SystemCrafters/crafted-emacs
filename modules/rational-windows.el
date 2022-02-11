;;; rational-windows.el -*- lexical-binding: t; -*-

(defcustom rational-windows-evil-style nil
  "When t, window movement bindings will be evil-style.")

(defcustom rational-windows-prefix-key "C-c w"
  "Configure the prefix key for `rational-windows' bindings.")

(winner-mode 1)

(define-prefix-command 'rational-windows-key-map)

(define-key 'rational-windows-key-map (kbd "u") 'winner-undo)
(define-key 'rational-windows-key-map (kbd "n") 'windmove-down)
(define-key 'rational-windows-key-map (kbd "p") 'windmove-up)
(define-key 'rational-windows-key-map (kbd "b") 'windmove-left)
(define-key 'rational-windows-key-map (kbd "f") 'windmove-right)

(global-set-key (kbd rational-windows-prefix-key) 'rational-windows-key-map)
