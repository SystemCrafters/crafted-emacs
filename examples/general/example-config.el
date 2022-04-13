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
(require 'rational-defaults)
(require 'rational-screencast)
(require 'rational-ui)
(require 'rational-editing)
(require 'rational-evil)
(require 'rational-completion)
(require 'rational-windows)

;; Set further font and theme customizations
(custom-set-variables
   '(rational-ui-default-font
     '(:font "JetBrains Mono" :weight light :height 185)))

(load-theme 'doom-snazzy t)

;; To not load `custom.el' after `config.el', uncomment this line.
;; (setq rational-load-custom-file nil)

;;; example-config.el ends here
