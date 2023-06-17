;;;;; early-init.el --- Early init file -*- lexical-binding: t -*-

;;; Don't use package.el, use straight.el instead
(setq package-enable-at-startup nil)
(advice-add #'package--ensure-init-file :override #'ignore)

;;; Prefer loading newest compiled .el file
(setq load-prefer-newer t)

;;; Inhibit resizing frame
(setq frame-inhibit-implied-resize t)

;;; Make the initial buffer load faster by setting its mode to fundamental-mode
(setq initial-major-mode 'fundamental-mode)

;;; Minimal UI
(setq inhibit-startup-message t)
(setq inhibit-startup-screen t)
(setq inhibit-startup-buffer-menu t)
(setq inhibit-splash-screen t)
(setq inhibit-startup-buffer-menu t)
(setq inhibit-x-resources t)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(horizontal-scroll-bars) default-frame-alist)

;;; Do not compact font cache
(setq inhibit-compacting-font-caches t)

;; Temporarily increase the garbage collection threshold.  These
;; changes help shave off about half a second of startup time.
;; https://gitlab.com/protesilaos/dotfiles/-/blob/master/emacs/.emacs.d/early-init.el
(defvar p-emacs--gc-cons-threshold gc-cons-threshold)
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold p-emacs--gc-cons-threshold)))

;;; Native compilation settings
(when (featurep 'native-compile)
  ;; silence compiler warnings as they can be pretty disruptive
  (setq native-comp-async-report-warnings-errors nil)
  ;; Make native compilation happens asynchronously
  (setq native-comp-deferred-compilation t)
  (setq compilation-scroll-output t))

;;; No titlebar
(add-to-list 'default-frame-alist '(undecorated . t))

;;; Maximize frame at startup
(setq initial-frame-alist (quote ((fullscreen . maximized))))

;;; Premature redisplays can substantially affect startup times and produce ugly flashes of unstyled Emacs
;; https://github.com/doomemacs/doomemacs/blob/master/early-init.el
(setq-default inhibit-redisplay t)
(add-hook 'window-setup-hook
          (lambda ()
            (setq-default inhibit-redisplay nil)
            (redisplay)))

;; Avoid startup flash
;; https://gitlab.com/protesilaos/dotfiles/-/blob/master/emacs/.emacs.d/early-init.el
(defun prot-emacs-re-enable-frame-theme (_frame)
  "Re-enable active theme, if any, upon FRAME creation.
Add this to `after-make-frame-functions' so that new frames do
not retain the generic background set by the function
`prot-emacs-avoid-initial-flash-of-light'."
  (when-let ((theme (car custom-enabled-themes)))
    (enable-theme theme)))

;; NOTE 2023-02-05: The reason the following works is because (i) the
;; `mode-line-format' is specified again and (ii) the
;; `prot-emacs-theme-gsettings-dark-p' will load a dark theme.
(defun prot-emacs-avoid-initial-flash-of-light ()
  "Avoid flash of light when starting Emacs, if needed.
New frames are instructed to call `prot-emacs-re-enable-frame-theme'."
    (setq mode-line-format nil)
    (set-face-attribute 'default nil :background "#000000" :foreground "#ffffff")
    (set-face-attribute 'mode-line nil :background "#000000" :foreground "#ffffff" :box 'unspecified)
    (add-hook 'after-make-frame-functions #'prot-emacs-re-enable-frame-theme))

(prot-emacs-avoid-initial-flash-of-light)

;;; Consistent color with theme
;; (when (featurep 'ns)
;;   (push '(ns-transparent-titlebar . t) default-frame-alist))

;;;;; early-init.el ends here
