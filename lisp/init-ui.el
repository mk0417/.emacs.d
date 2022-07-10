;;;;; init-ui.el --- UI -*- lexical-binding: t -*-

;;; Pixelwise
(setq frame-resize-pixelwise t)

;;; Disable menu bar
(menu-bar-mode -1)

;;; Font
(defvar emacs-ui-default-font "Iosevka Comfy")
(defvar emacs-ui-variable-pitch-font "Iosevka Comfy Duo")

(defun p-set-regular-font ()
  (interactive)
  (set-face-attribute 'default nil :font emacs-ui-default-font :height 120 :weight 'regular)
  (set-face-attribute 'variable-pitch nil :font emacs-ui-variable-pitch-font :height 1.05 :weight 'regular))

(defun p-set-large-font ()
  (interactive)
  (set-face-attribute 'default nil :font emacs-ui-default-font :height 160 :weight 'regular)
  (set-face-attribute 'variable-pitch nil :font emacs-ui-variable-pitch-font :height 1.05 :weight 'regular))

(defun p-set-extra-large-font ()
  (interactive)
  (set-face-attribute 'default nil :font emacs-ui-default-font :height 190 :weight 'regular)
  (set-face-attribute 'variable-pitch nil :font emacs-ui-variable-pitch-font :height 1.05 :weight 'regular))

(add-hook 'after-init-hook 'p-set-regular-font)

;; mode-line variable-pitch font
(defun emacs-ui--set-mode-line-font ()
  (set-face-attribute 'mode-line nil :inherit 'variable-pitch :height 1))
(add-hook 'after-init-hook 'emacs-ui--set-mode-line-font)

;;; Highlight current line
(global-hl-line-mode 1)

;;; Full path in mode-line
(setq-default mode-line-buffer-identification
              (list 'buffer-file-name
                    '(:eval (propertize (format "  %s" buffer-file-truename)))))

;;; Line number
(when (fboundp 'display-line-numbers-mode)
  (setq-default display-line-numbers-width 3)
  (add-hook 'prog-mode-hook 'display-line-numbers-mode))

;;; Column number
(setq  column-number-mode t)

;;; Column indicator
(when (boundp 'display-fill-column-indicator)
  (setq-default display-fill-column-indicator-column 80)
  (custom-set-faces
   '(fill-column-indicator
     ((t (:background nil :foreground "grey30"))))))

;;; Add visual pulse when changing focus, like beacon but built-in
;; from from https://karthinks.com/software/batteries-included-with-emacs/
(defun emacs-ui--pulse-line (&rest _)
  "Pulse the current line."
  (pulse-momentary-highlight-one-line (point)))

(dolist (command '(evil-goto-line evil-window-right evil-window-left evil-window-up evil-window-down
                                     scroll-up-command scroll-down-command
                                     recenter-top-bottom other-window))
  (advice-add command :after #'emacs-ui--pulse-line))

(provide 'init-ui)
;;;;; init-ui.el ends here
