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
(defvar rational-config-path
  (let ((home-dir (if (and (string-equal "windows-nt" (symbol-name system-type))
                           (getenv "APPDATA"))
                      (getenv "APPDATA")
                    (getenv "HOME"))))
    (if (file-exists-p (expand-file-name ".rational-emacs" home-dir))
        (expand-file-name ".rational-emacs" home-dir)
      (expand-file-name ".config/rational-emacs" home-dir)))
  "The user's rational-emacs configuration path.")

(defvar rational-prefer-guix-packages nil
  "If t, expect packages to be installed via Guix by default.")

;; Load the early config file if it exists
(let ((early-config-path (expand-file-name "early-config.el" rational-config-path)))
  (when (file-exists-p early-config-path)
    (load early-config-path nil 'nomessage)))
