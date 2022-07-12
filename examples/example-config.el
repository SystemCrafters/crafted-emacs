;;; example-config.el -- Example Rational Emacs user customization file -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Rational Emacs supports user customization through a `config.el' file
;; similar to this one.  You can copy this file as `config.el' to your
;; Rational Emacs configuration directory as an example.
;;
;; In your configuration you can set any Emacs configuration variable, face
;; attributes, themes, etc as you normally would.
;;
;; See the README.org file in this repository for additional information.

;;; Code:
;; At the moment, Rational Emacs offers the following modules.
;; Comment out everything you don't want to use.
;; At the very least, you should decide whether or not you want to use
;; evil-mode, as it will greatly change how you interact with Emacs.
;; So, if you prefer Vim-style keybindings over vanilla Emacs keybindings
;; remove the comment in the line about `rational-evil' below.
(require 'rational-defaults)    ; Sensible default settings for Emacs
(require 'rational-use-package) ; Configuration for `use-package`
(require 'rational-updates)     ; Tools to upgrade Rational Emacs
(require 'rational-completion)  ; selection framework based on `vertico`
(require 'rational-ui)          ; Better UI experience (modeline etc.)
(require 'rational-windows)     ; Window management configuration
(require 'rational-editing)     ; Whitspace trimming, auto parens etc.
;(require 'rational-evil)        ; An `evil-mode` configuration
(require 'rational-org)         ; org-appear, clickable hyperlinks etc.
(require 'rational-project)     ; built-in alternative to projectile
(require 'rational-speedbar)    ; built-in file-tree
(require 'rational-screencast)  ; show current command and binding in modeline
(require 'rational-compile)     ; automatically compile some emacs lisp files

;; Set the default face. The default face is the basis for most other
;; faces used in Emacs. A "face" is a configuration including font,
;; font size, foreground and background colors and other attributes.
;; The fixed-pitch and fixed-pitch-serif faces are monospace faces
;; generally used as the default face for code. The variable-pitch
;; face is used when `variable-pitch-mode' is turned on, generally
;; whenever a non-monospace face is preferred.
(add-hook 'emacs-startup-hook
          (lambda ()
            (custom-set-faces
             `(default ((t (:font "JetBrains Mono Light 18"))))
             `(fixed-pitch ((t (:inherit (default)))))
             `(fixed-pitch-serif ((t (:inherit (default)))))
             `(variable-pitch ((t (:font "Arial 18")))))))

;; Themes are color customization packages which coordinate the
;; various colors, and in some cases, font-sizes for various aspects
;; of text editing within Emacs, toolbars, tabbars and
;; modeline. Several themes are built-in to Emacs, by default,
;; Rational Emacs uses the `deeper-blue' theme. Here is an example of
;; loading a different theme from the venerable Doom Emacs project.
(rational-package-install-package 'doom-themes)
(progn
  (disable-theme 'deeper-blue)          ; first turn off the deeper-blue theme
  (load-theme 'doom-palenight t))       ; load the doom-palenight theme

;; To not load `custom.el' after `config.el', uncomment this line.
;; (setq rational-load-custom-file nil)

;;; example-config.el ends here
