;;;;; init-ui.el --- UI -*- lexical-binding: t -*-

;;; Install packages
(straight-use-package 'fontaine)

;;; Disable menu bar
(menu-bar-mode -1)

;;; Font
;; https://gitlab.com/protesilaos/fontaine
(setq fontaine-presets
      '((tiny
         :default-family "Iosevka Comfy Wide Fixed"
         :default-height 70)
        (small
         :default-family "Iosevka Comfy Fixed"
         :default-height 90)
        (regular
         :default-height 120)
        (medium
         :default-height 130)
        (large
         :default-weight semilight
         :default-height 150
         :bold-weight extrabold)
        (code-demo
         :default-weight semilight
         :default-height 170
         :bold-weight extrabold)
        (presentation
         :default-weight semilight
         :default-height 220
         :bold-weight extrabold)
        (t
         :default-family "Iosevka Comfy"
         :default-weight regular
         :default-height 100
         :fixed-pitch-family nil
         :fixed-pitch-weight nil
         :fixed-pitch-height 1.0
         :fixed-pitch-serif-family nil
         :fixed-pitch-serif-weight nil
         :fixed-pitch-serif-height 1.0
         :variable-pitch-family "Iosevka Comfy Motion Duo"
         :variable-pitch-weight nil
         :variable-pitch-height 1.0
         :bold-family nil
         :bold-weight bold
         :italic-family "Iosevka Comfy Motion"
         :italic-slant italic
         :line-spacing nil)))

(fontaine-set-preset (or (fontaine-restore-latest-preset) 'regular))
(add-hook 'kill-emacs-hook #'fontaine-store-latest-preset)
(dolist (hook '(modus-themes-after-load-theme-hook))
  (add-hook hook #'fontaine-apply-current-preset))

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
     ((t (:background unspecified :foreground "grey30"))))))

;;; Visual line
(add-hook 'text-mode-hook 'visual-line-mode)
(add-hook 'text-mode-hook 'toggle-word-wrap)

;;; Add visual pulse when changing focus, like beacon but built-in
;; from from https://karthinks.com/software/batteries-included-with-emacs/
(defun emacs-ui--pulse-line (&rest _)
  (pulse-momentary-highlight-one-line (point)))

(dolist (command '(evil-goto-line evil-window-right evil-window-left evil-window-up evil-window-down
                                  scroll-up-command scroll-down-command
                                  recenter-top-bottom other-window))
  (advice-add command :after #'emacs-ui--pulse-line))

(provide 'init-ui)
;;;;; init-ui.el ends here
