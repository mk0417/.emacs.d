;;; early-init.el --- Emacs>=27 pre-initialisation config  -*- lexical-binding: t; -*-

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; adjust garbage collection thresholds during startup, and thereafter
;; https://github.com/purcell/emacs.d/blob/master/init.el
(let ((normal-gc-cons-threshold (* 20 1024 1024)))
  (setq gc-cons-threshold most-positive-fixnum)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

;;(setq package-enable-at-startup nil)

;; inhibit resizing frame
;;(setq frame-inhibit-implied-resize t)

;; use straight to manage packages
(require 'init-straight)


;;; early-init.el ends here
