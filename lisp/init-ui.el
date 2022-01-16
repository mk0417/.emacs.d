;;; init-ui.el --- UI config  -*- lexical-binding: t; -*-

;; package
(straight-use-package 'rainbow-delimiters)

;; make titlebar color consistent with system
(push '(ns-transparent-titlebar . t) default-frame-alist)

;; titlebar format
(if (< emacs-major-version 28)
    (setq frame-title-format
	  '((:eval (if (buffer-file-name)
		       (concat " " (abbreviate-file-name (buffer-file-name)))
		     " %b"))))
  ;; https://emacs.stackexchange.com/questions/33680/how-to-remove-the-icon-in-the-titlebar
  (setq frame-title-format nil
	us-use-proxy-icon nil))

;; minimal UI
(tool-bar-mode -1)
(menu-bar-mode -1)
(set-scroll-bar-mode nil)

;; no startup message
(setq inhibit-startup-screen t)

;; maximize frame at startup
(setq initial-frame-alist (quote ((fullscreen . maximized))))

;; highlight current line
(global-hl-line-mode 1)

;; full path in mode-line
(setq-default mode-line-buffer-identification
	      (list 'buffer-file-name
		    '(:eval (propertize (format "  %s" buffer-file-truename) 'face 'bold))))

;; no fringe
(fringe-mode '(0 . 0))

;; line number
(when (fboundp 'display-line-numbers-mode)
  (setq-default display-line-numbers-width 3)
  (add-hook 'prog-mode-hook 'display-line-numbers-mode))

;; column number
(setq  column-number-mode t)

;; column indicator
(when (boundp 'display-fill-column-indicator)
  (setq-default display-fill-column-indicator-column 80
		display-fill-column-indicator-character ?|))

;; cursor type in minibuffer
(defun p-minibuffer-cursor-type ()
  (setq cursor-type 'hbar))
(add-hook 'minibuffer-setup-hook 'p-minibuffer-cursor-type)

;; cursor blink
(setq blink-cursor-delay 0.2)
(setq blink-cursor-interval 0.3)
(setq blink-cursor-blinks 30)

;; disable face of completions-first-difference
(custom-set-faces
   '(completions-first-difference ((t (:background nil :weight normal)))))

;; font
(setq prot-fonts-typeface-sets-alist
      '((small . ( :fixed-pitch-family "Hack"
		   :fixed-pitch-regular-weight regular
		   :fixed-pitch-heavy-weight bold
		   :fixed-pitch-height 75
		   :fixed-pitch-line-spacing 1
		   :variable-pitch-family "FiraGO"
		   :variable-pitch-height 1.05
		   :variable-pitch-regular-weight normal))
	(regular . ( :fixed-pitch-family "Hack"
		     :fixed-pitch-regular-weight regular
		     :fixed-pitch-heavy-weight bold
		     :fixed-pitch-height 105
		     :fixed-pitch-line-spacing nil
		     :variable-pitch-family "FiraGO"
		     :variable-pitch-height 1.05
		     :variable-pitch-regular-weight normal))
	(large . ( :fixed-pitch-family "Hack"
		   :fixed-pitch-regular-weight normal
		   :fixed-pitch-heavy-weight bold
		   :fixed-pitch-height 130
		   :fixed-pitch-line-spacing nil
		   :variable-pitch-family "FiraGO"
		   :variable-pitch-height 1.05
		   :variable-pitch-regular-weight normal))
	(large-alt . ( :fixed-pitch-family "Iosevka Comfy"
		       :fixed-pitch-regular-weight book
		       :fixed-pitch-heavy-weight extrabold
		       :fixed-pitch-height 155
		       :fixed-pitch-line-spacing nil
		       :variable-pitch-family "Noto Sans"
		       :variable-pitch-height 1.0
		       :variable-pitch-regular-weight normal))))

(autoload 'prot-fonts-fonts-per-monitor "prot-fonts")
(prot-fonts-fonts-per-monitor)

;; rainbow-delimiters
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; show whitespace and delete on saving
;; https://github.com/zilongshanren/emacs.d/blob/develop/lisp/init-basic.el
(defun p-enable-trailing-whitespace ()
  (setq show-trailing-whitespace t)
  (add-hook 'before-save-hook #'delete-trailing-whitespace nil t))
(add-hook 'prog-mode-hook 'p-enable-trailing-whitespace)

(provide 'init-ui)
;;; init-ui.el ends here
