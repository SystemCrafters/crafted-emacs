;;; rational-startup.el --- Rational Emacs splash screen on startup  -*- lexical-binding: t; -*-

;; Copyright (C) 2022
;; SPDX-License-Identifier: MIT

;; Author: System Crafters Community

;;; Commentary:

;; Provide a fancy splash screen similar to the Emacs default splash
;; screen or the Emacs about page.

;;; Code:

(defgroup rational-startup '()
  "Startup configuration for Rational Emacs"
  :tag "Rational Startup"
  :group 'rational)

(defcustom rational-startup-inhibit-splash nil
  "Disable the Rational Emacs Splash screen"
  :type 'boolean
  :group 'rational-startup)

(defcustom rational-startup-recentf-count 10
  "The number of recent files to display on the splash screen"
  :type 'number
  :group 'rational-startup)

(defconst rational-startup-text
  `((:face (variable-pitch font-lock-comment-face (:height 1.5) bold)
           ,(let* ((welcome-text "Welcome to Rational Emacs!\n\n")
                   (welcome-len (length welcome-text))
                   (welcome-mid (/ welcome-len 2)))
              (concat
               (make-string (- (/ (window-width) 2)
                               welcome-mid)
                            ? )
               welcome-text))
           :face variable-pitch
           :link ("View Rational Emacs Manual" ,(lambda (_button) (info "rational-emacs")))
           "\tView the Rational Emacs manual using Info\n"
           "\n"))
  "A list of texts to show in the middle part of splash screens.
Each element in the list should be a list of strings or pairs
`:face FACE', like `fancy-splash-insert' accepts them.")

(defvar rational-startup-screen-inhibit-startup-screen nil)

(defun rational-startup-tail (&optional concise)
  "Insert the tail part of the splash screen into the current buffer."
  (fancy-splash-insert
   :face 'variable-pitch
   "\nTo start...     "
   :link `("Open a File"
           ,(lambda (_button) (call-interactively 'find-file))
           "Specify a new file's name, to edit the file")
   "     "
   :link `("Open Home Directory"
           ,(lambda (_button) (dired "~"))
           "Open your home directory, to operate on its files")
   "     "
   :link `("Customize Rational Emacs"
           ,(lambda (_button) (customize-group 'rational))
           "Change initialization settings including this screen")
   "\n")

  (fancy-splash-insert
   :face '(variable-pitch (:height 0.7))
   "\n\nTurn this screen off by adding:\n"
   :face '(default font-lock-keyword-face)
   "`(customize-set-variable 'rational-startup-inhibit-splash t)'\n"
   :face '(variable-pitch (:height 0.7))
   " to your " rational-config-file "\n"
   "Or check the box and click the link below, which will do the same thing.")

  (fancy-splash-insert
   :face 'variable-pitch "\n"
   :link `("Dismiss this startup screen"
           ,(lambda (_button)
              (when rational-startup-screen-inhibit-startup-screen
                (customize-set-variable 'rational-startup-inhibit-splash t)
                (customize-mark-to-save 'rational-startup-inhibit-splash)
                (custom-save-all))
              (quit-windows-on "*Rational Emacs*" t)))
   "  ")
   (when custom-file
     (let ((checked (create-image "checked.xpm"
                                  nil nil :ascent 'center))
           (unchecked (create-image "unchecked.xpm"
                                    nil nil :ascent 'center)))
       (insert-button
        " "
        :on-glyph checked
        :off-glyph unchecked
        'checked nil 'display unchecked 'follow-link t
        'action (lambda (button)
                  (if (overlay-get button 'checked)
                      (progn (overlay-put button 'checked nil)
                             (overlay-put button 'display
                                          (overlay-get button :off-glyph))
                             (setq rational-startup-screen-inhibit-startup-screen
                                   nil))
                    (overlay-put button 'checked t)
                    (overlay-put button 'display
                                 (overlay-get button :on-glyph))
                    (setq rational-startup-screen-inhibit-startup-screen t))))))
  (fancy-splash-insert :face '(variable-pitch (:height 0.9))
                       " Never show it again."))

(defun rational-startup-recentf ()
  (message "Showing recents on splash screen")
  (fancy-splash-insert
   :face '(variable-pitch font-lock-string-face italic)
   (condition-case recentf-list
       (if (not (seq-empty-p recentf-list))
           "Recent Files:\n"
         "\n")
     (error "\n")))
  (condition-case recentf-list
      (if (not (seq-empty-p recentf-list))
          (dolist (file (seq-take recentf-list rational-startup-recentf-count))
            (fancy-splash-insert
             :face 'default
             :link `(,file ,(lambda (_button) (find-file file)))
             "\n"))
        "\n")
    (error "\n")))

(defun rational-startup-screen (&optional concise)
  "Display fancy startup screen.
If CONCISE is non-nil, display a concise version of the
splash screen in another window."
  (message "Loaing Rational Startup Screen")
  (let ((splash-buffer (get-buffer-create "*Rational Emacs*")))
    (with-current-buffer splash-buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (setq default-directory command-line-default-directory)
        (make-local-variable 'rational-startup-screen-inhibit-startup-screen)
        (if pure-space-overflow
            (insert pure-space-overflow-message))
        ;; (unless concise
        ;;   (fancy-splash-head))
        (dolist (text rational-startup-text)
          (apply #'fancy-splash-insert text)
          (insert "\n"))
        ;; (skip-chars-backward "\n")
        ;; (delete-region (point) (point-max))
        ;; (insert "\n")
        (rational-startup-recentf)
        (skip-chars-backward "\n")
        (delete-region (point) (point-max))
        (insert "\n")
        (rational-startup-tail concise))
      (use-local-map splash-screen-keymap)
      (setq-local browse-url-browser-function 'eww-browse-url)
      (setq tab-width 22
            buffer-read-only t)
      (set-buffer-modified-p nil)
      (if (and view-read-only (not view-mode))
          (view-mode-enter nil 'kill-buffer))
      (goto-char (point-min))
      (forward-line (if concise 2 4)))
    (if concise
        (progn
          (display-buffer splash-buffer)
          ;; If the splash screen is in a split window, fit it.
          (let ((window (get-buffer-window splash-buffer t)))
            (or (null window)
                (eq window (selected-window))
                (eq window (next-window window))
                (fit-window-to-buffer window))))
      (switch-to-buffer splash-buffer))))

(provide 'rational-startup)
;;; rational-startup.el ends here
