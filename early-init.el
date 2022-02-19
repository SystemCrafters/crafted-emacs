;;; early-init.el -*- lexical-binding: t; -*-

;; Increase the GC threshold for faster startup
;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

;; Prefer loading newest compiled .el file
(setq load-prefer-newer noninteractive)

;; Native compilation settings
(when (featurep 'native-compile)
  ;; Silence compiler warnings as they can be pretty disruptive
  (setq native-comp-async-report-warnings-errors nil)

  ;; Make native compilation happens asynchronously
  (setq native-comp-deferred-compilation t)

  ;; Set the right directory to store the native compilation cache
  (add-to-list 'native-comp-eln-load-path (expand-file-name "eln-cache/" user-emacs-directory)))

;; Don't use package.el, we'll use straight.el instead
(setq package-enable-at-startup nil)

;; Remove some unneeded UI elements (the user can turn back on anything they wish)
(setq inhibit-startup-message t)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(background-color . "#232635") default-frame-alist)
(push '(foreground-color . "#FFFFFF") default-frame-alist)
(push '(mouse-color . "white") default-frame-alist)

;; Make the initial buffer load faster by setting its mode to fundamental-mode
(setq initial-major-mode 'fundamental-mode)

;; Find the user configuration path
;; In order do these checks:
;; * using chemacs?
;; ** yes, and have specified a location with the RATIONAL_EMACS_HOME
;;    environment variable
;; ** yes, but no environment variable, assume the rational-emacs
;;    folder in the profile
;; * use RATIONAL_EMACS_HOME environment variable
;; * XDG_CONFIG_HOME or the path .config/rational-emacs
;;   exists. XDG_CONFIG_HOME usually defaults to $HOME/.config/, so
;;   these are the same thing
;; * use HOME environment variable
(defvar rational-config-path
  (cond
   ((featurep 'chemacs)
    (if (getenv  "RATIONAL_EMACS_HOME")
        (expand-file-name (getenv "RATIONAL_EMACS_HOME"))
      (expand-file-name "rational-emacs" user-emacs-directory)))
   ((getenv "RATIONAL_EMACS_HOME") (expand-file-name (getenv "RATIONAL_EMACS_HOME")))
   ((or (getenv "XDG_CONFIG_HOME") (file-exists-p (expand-file-name ".config/rational-emacs" (getenv "HOME"))))
    (if (getenv "XDG_CONFIG_HOME")
	(expand-file-name "rational-emacs" (getenv "XDG_CONFIG_HOME"))
      (expand-file-name ".config/rational-emacs" (getenv "HOME"))))
   ((getenv "HOME") (expand-file-name ".rational-emacs" (getenv "HOME"))))
  "The user's rational-emacs configuration path.")

(unless (file-exists-p rational-config-path)
  (mkdir rational-config-path t))

(defun rational-runs-guix-emacs-p ()
  "Verifies if the running emacs executable is under the `/gnu/store/' path."
  (unless (or (equal system-type 'ms-dos)
              (equal system-type 'windows-nt))
    ;; Since there is no windows implementation of guix
    (string-prefix-p "/gnu/store/"
                     (shell-command-to-string
                      (format "which `realpath '%s`'"
                              (car command-line-args))))))

(defvar rational-prefer-guix-packages (rational-runs-guix-emacs-p)
  "If t, expect packages to be installed via Guix by default.")

;; Load the early config file if it exists
(let ((early-config-path (expand-file-name "early-config.el" rational-config-path)))
  (when (file-exists-p early-config-path)
    (load early-config-path nil 'nomessage)))
