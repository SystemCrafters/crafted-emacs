;;; early-init.el -*- lexical-binding: t; -*-


(if (load (concat user-emacs-directory "rational-emacs")  'noerror)
    (rational-emacs-early-init)
  (message "Failed to load Rational Emacs"))
