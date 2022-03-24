;;;;; early-init.el --- Emacs>=27 pre-initialisation config  -*- lexical-binding: t; -*-

;;; Increase the GC threshold for faster startup
;; The default is 800 kilobytes. Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

;;; Prefer loading newest compiled .el file
(setq load-prefer-newer noninteractive)

;;; Native compilation settings
(when (featurep 'native-compile)
  ;; Silence compiler warnings as they can be pretty disruptive
  (setq native-comp-async-report-warnings-errors nil)
  ;; Make native compilation happens asynchronously
  (setq native-comp-deferred-compilation t)
  (setq native-comp-async-jobs-number 6)
  (setq  compilation-scroll-output t)
  ;; Set the right directory to store the native compilation cache
  (add-to-list 'native-comp-eln-load-path (expand-file-name "eln-cache/" user-emacs-directory)))

;;; Don't use package.el, we'll use straight.el instead
(setq package-enable-at-startup nil)

;;; inhibit resizing frame
(setq frame-inhibit-implied-resize t)

;;; no titlebar
(add-to-list 'default-frame-alist '(undecorated . t))

;;; make titlebar color consistent with system
(when (featurep 'ns)
  (push '(ns-transparent-titlebar . t) default-frame-alist))

;;; disable too bar and scroll bar
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;;; Make the initial buffer load faster by setting its mode to fundamental-mode
(setq initial-major-mode 'fundamental-mode)

;;;;; early-init.el ends here
