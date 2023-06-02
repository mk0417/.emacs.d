;;;;; init-ui.el --- UI -*- lexical-binding: t -*-

;;; Install packages
(straight-use-package 'fontaine)

;;; Disable menu bar
(menu-bar-mode -1)

;;; No fringe
(fringe-mode '(3 . 0))

;;; Font
;; https://gitlab.com/protesilaos/fontaine
(setq x-underline-at-descent-line nil)
(setq-default text-scale-remap-header-line t)

(setq fontaine-presets
      '((small
         :default-family "Iosevka Comfy Wide"
         :default-height 100
         :variable-pitch-family "Iosevka Comfy Wide Motion Duo")
        (regular)
        (large
         :default-weight semilight
         :default-height 150
         :bold-weight extrabold)
        (presentation
         :inherit large
         :default-weight light
         :default-height 180)
        (t
         :default-family "Iosevka Comfy"
         :default-weight regular
         :default-height 120
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
         :italic-family nil
         :italic-slant italic
         :line-spacing nil)))

(fontaine-set-preset (or (fontaine-restore-latest-preset) 'regular))
(add-hook 'kill-emacs-hook #'fontaine-store-latest-preset)
(dolist (hook '(modus-themes-after-load-theme-hook))
  (add-hook hook #'fontaine-apply-current-preset))

;; mode-line variable-pitch font
;; (defun emacs-ui--set-mode-line-font ()
;;   (set-face-attribute 'mode-line nil :inherit 'variable-pitch :height 1))
;; (add-hook 'after-init-hook 'emacs-ui--set-mode-line-font)

(defun p-enable-variable-pitch ()
  (unless (or (derived-mode-p 'mhtml-mode 'nxml-mode 'yaml-mode)
              (member (buffer-name) '("*Colors*" "*Faces*" "*Quick Help*")))
    (variable-pitch-mode 1)))

(defvar p-enable-variable-pitch-in-hooks
  '(text-mode-hook
    help-mode-hook))

(dolist (hook p-enable-variable-pitch-in-hooks)
  (add-hook hook #'p-enable-variable-pitch))

;;; Highlight current line
(global-hl-line-mode 1)

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

;;; Time
(setq display-time-format "  %a %e %b, %H:%M ")
(setq display-time-24hr-format t)
(setq display-time-interval 60)
(setq display-time-default-load-average nil)

(add-hook 'after-init-hook #'display-time-mode)

(provide 'init-ui)
;;;;; init-ui.el ends here
