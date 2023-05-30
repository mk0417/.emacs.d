;;;;; init-defaults.el --- Emacs better defaults -*- lexical-binding: t -*-

;;; Install packages
(straight-use-package 'diminish)
(straight-use-package 'which-key)

;;; Pixelwise
(setq frame-resize-pixelwise t)
(setq window-resize-pixelwise t)

;;; Some optimizations
(setq-default bidi-display-reordering 'left-to-right)
(setq-default cursor-in-non-selected-windows nil)
(unless (eq system-type 'gnu/linux) (setq command-line-x-option-alist nil))
(when (> emacs-major-version 27)
  (setq redisplay-skip-fontification-on-input t))
(setq sentence-end-double-space nil)
(setq read-process-output-max (* 1024 1024))

;;; No box
(setq use-dialog-box nil)
(setq use-file-dialog nil)

;;; Revert Dired and other buffers
(setq global-auto-revert-non-file-buffers t)

;;; Revert buffers when the underlying file has changed
(global-auto-revert-mode 1)

;;; Typed text replaces the selection if the selection is active,
;; pressing delete or backspace deletes the selection.
(delete-selection-mode)

;;; Set default coding system
(set-default-coding-systems 'utf-8)

;;; Turn off bell sound
(setq ring-bell-function 'ignore)

;;; Change to ~100 MB
(setq large-file-warning-threshold 100000000)

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
(setq-default recentf-exclude `("/Applications/Emacs.app/Contents/Resources/lisp/"
                                ".gz" ".xz" ".zip" ".gpg" ".asc"
                                "/tmp/" "/etc" "/usr" "/tmp"
                                "/ssh:" "/sudo:"
                                "~/.local" "~/.cache"
                                "~/Downloads" "~/Pictures"
                                "~/.emacs.d/straight/"))

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

;;; Keep small kill ring
(setq kill-ring-max 60)

;;; Wrap long lines in text mode
(setq-default fill-column 120)
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;;; Hide "setting up indent for shell type zsh"
;; https://emacs.stackexchange.com/questions/52846/how-to-remove-message-indentation-setup-for-shell-type-sh
(advice-add 'sh-set-shell :around
            (lambda (orig-fun &rest args)
              (let ((inhibit-message t))
                (apply orig-fun args))))

;;; Which-key
(setq-default which-key-idle-delay 0.8)
(which-key-mode)
(diminish 'which-key-mode)

;;; Eldoc
(diminish 'eldoc-mode)

;;; Themes
(setq custom-safe-themes t)

;;; Emacs server
(server-start)

;;; Keybindings
(global-set-key (kbd "C-x k") 'kill-this-buffer)
(global-set-key (kbd "C-x K") 'kill-buffer-and-window)
(global-set-key (kbd "M-i") 'forward-paragraph)
(global-set-key (kbd "M-o") 'backward-paragraph)
(global-set-key (kbd "C-h K") #'describe-keymap)
(define-key minibuffer-local-map (kbd "C-k") 'delete-backward-char)
(define-key minibuffer-local-map (kbd "C-w") 'backward-kill-word)

(provide 'init-defaults)
;;;;; init-defaults.el ends here
