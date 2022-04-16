;;;;; init-ui.el --- UI config  -*- lexical-binding: t; -*-

;;; package
(straight-use-package 'rainbow-delimiters)
(straight-use-package 'pulsar)

;;; titlebar format
(if (< emacs-major-version 28)
    (setq frame-title-format
          '((:eval (if (buffer-file-name)
                       (concat " " (abbreviate-file-name (buffer-file-name)))
                     " %b"))))
  ;; https://emacs.stackexchange.com/questions/33680/how-to-remove-the-icon-in-the-titlebar
  (setq frame-title-format nil
        us-use-proxy-icon nil))

;;; disable menu bar
(menu-bar-mode -1)

;;; no startup message
(setq inhibit-startup-screen t)

;;; maximize frame at startup
(setq initial-frame-alist (quote ((fullscreen . maximized))))

(setq frame-resize-pixelwise t)

;;; highlight current line
(global-hl-line-mode 1)

;;; full path in mode-line
(setq-default mode-line-buffer-identification
              (list 'buffer-file-name
                    '(:eval (propertize (format "  %s" buffer-file-truename) 'face 'regular))))

;;; no fringe
(fringe-mode '(0 . 0))

;;; line number
(when (fboundp 'display-line-numbers-mode)
  (setq-default display-line-numbers-width 3)
  (add-hook 'prog-mode-hook 'display-line-numbers-mode))

;;; column number
(setq  column-number-mode t)

;;; column indicator
(when (boundp 'display-fill-column-indicator)
  (setq-default display-fill-column-indicator-column 80)
  (custom-set-faces
   '(fill-column-indicator
     ((t (:background nil :foreground "grey30"))))))

;;; cursor type in minibuffer
(defun p-minibuffer-cursor-type ()
  (setq cursor-type 'hbar))
(add-hook 'minibuffer-setup-hook 'p-minibuffer-cursor-type)

;;; cursor blink
(setq blink-cursor-delay 0.2)
(setq blink-cursor-interval 0.3)
(setq blink-cursor-blinks 30)

;;; disable face of completions-first-difference
;; (custom-set-faces
;;    '(completions-first-difference ((t (:background nil :weight normal)))))

;;; font
;; (defvar p-default-font "Hack")
(defvar p-default-font "Iosevka Comfy")
(defvar p-variable-pitch-font "FiraGo")

(defun p-set-regular-font ()
  (interactive)
  (set-face-attribute 'default nil :font p-default-font :height 120 :weight 'regular)
  (set-face-attribute 'variable-pitch nil :font p-variable-pitch-font :height 1.05 :weight 'regular))

(defun p-set-large-font ()
  (interactive)
  (set-face-attribute 'default nil :font p-default-font :height 160 :weight 'regular)
  (set-face-attribute 'variable-pitch nil :font p-variable-pitch-font :height 1.05 :weight 'regular))

(defun p-set-extra-large-font ()
  (interactive)
  (set-face-attribute 'default nil :font p-default-font :height 190 :weight 'regular)
  (set-face-attribute 'variable-pitch nil :font p-variable-pitch-font :height 1.05 :weight 'regular))

(add-hook 'after-init-hook 'p-set-regular-font)

;; mode-line variable-pitch font
(defun p-set-mode-line-font ()
  (interactive)
  (set-face-attribute 'mode-line nil :inherit 'variable-pitch :height 1))

(add-hook 'after-init-hook 'p-set-mode-line-font)

;;; rainbow-delimiters
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;;; pulsar
(setq pulsar-pulse t)
(setq pulsar-delay 0.1)
(setq pulsar-iterations 10)
(setq pulsar-face 'pulsar-red)
(setq pulsar-highlight-face 'pulsar-cyan)

(pulsar-global-mode 1)
(diminish 'pulsar-mode)

(defun p-pulse-line (&rest _)
  (pulsar-pulse-line))

(dolist (command
	 '(evil-goto-first-line
	   winum-select-window-1
	   winum-select-window-2
	   winum-select-window-3
	   p-switch-to-scratch
	   p-switch-to-messages
	   p-switch-to-previous-buffer))
  (advice-add command :after #'p-pulse-line))

(add-hook 'consult-after-jump-hook #'pulsar-recenter-top)
(add-hook 'consult-after-jump-hook #'pulsar-reveal-entry)

(define-key global-map (kbd "C-c h p") #'pulsar-pulse-line)
(define-key global-map (kbd "C-c h h") #'pulsar-highlight-line)

;;; show whitespace and delete on saving
;; https://github.com/zilongshanren/emacs.d/blob/develop/lisp/init-basic.el
(defun p-enable-trailing-whitespace ()
  (setq show-trailing-whitespace t)
  (add-hook 'before-save-hook #'delete-trailing-whitespace nil t))

(dolist (hook '(prog-mode-hook markdown-mode-hook org-mode-hook conf-mode-hook text-mode-hook))
  (add-hook hook 'p-enable-trailing-whitespace))

(provide 'init-ui)
;;;;; init-ui.el ends here
