;;;;; init-defaults.el -*- lexical-binding: t -*-

;;; Install packages
(straight-use-package 'diminish)
(straight-use-package 'which-key)

;;; Revert Dired and other buffers
(setq global-auto-revert-non-file-buffers t)

;;; Revert buffers when the underlying file has changed
(global-auto-revert-mode 1)

;;; Typed text replaces the selection if the selection is active,
;; pressing delete or backspace deletes the selection.
(delete-selection-mode)

;;; Set default coding system
(set-default-coding-systems 'utf-8)
(setq ring-bell-function 'ignore)  ; turn off bell sound
(setq large-file-warning-threshold 100000000) ;; change to ~100 MB

;;; Use spaces instead of tabs
(setq-default indent-tabs-mode nil)

;;; Use "y" and "n" to confirm/negate prompt instead of "yes" and "no"
(if (boundp 'use-short-answers)
    (setq use-short-answers t)
  (advice-add 'yes-or-no-p :override #'y-or-n-p))

;;; The file used by the customization
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

;;; Turn on recentf mode
(add-hook 'after-init-hook #'recentf-mode)
(setq-default recentf-max-saved-items 50)
(setq-default recentf-exclude `("/Applications/Emacs.app/Contents/Resources/lisp/" "/tmp/" "/ssh:"))

;;; Don't create .# files
(setq create-lockfiles nil)

;;; No backup file
(setq make-backup-files nil)

;;; No not autosave
(setq auto-save-default nil)

;;; Split new buffer on the right by default
(setq split-height-threshold nil)

;;; Trash
(setq delete-by-moving-to-trash t)

;;; Do not saves duplicates in kill-ring
(setq kill-do-not-save-duplicates t)

;;; Make scrolling less stuttered
(setq auto-window-vscroll nil)
(setq fast-but-imprecise-scrolling t)
(setq scroll-conservatively 101)
(setq scroll-margin 0)
(setq scroll-preserve-screen-position t)

;;; Better support for files with long lines
(setq-default bidi-paragraph-direction 'left-to-right)
(setq-default bidi-inhibit-bpa t)
(global-so-long-mode 1)

;;; Which-key
(setq-default which-key-idle-delay 0.8)
(which-key-mode)
(diminish 'which-key-mode)

;;; Eldoc
(diminish 'eldoc-mode)

;;; Keybindings
(global-set-key (kbd "C-x k") 'kill-this-buffer)
(global-set-key (kbd "C-x K") 'kill-buffer-and-window)
(global-set-key (kbd "M-i") 'forward-paragraph)
(global-set-key (kbd "M-o") 'backward-paragraph)
(define-key minibuffer-local-map (kbd "C-k") 'delete-backward-char)
(define-key minibuffer-local-map (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "C-h K") #'describe-keymap)

(provide 'init-defaults)
;;;;; init-defaults.el ends here
