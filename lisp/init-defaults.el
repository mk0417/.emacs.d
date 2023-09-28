;;;;; init-defaults.el --- Emacs better defaults -*- lexical-binding: t -*-

;;; Some basic settings
(setq mouse-yank-at-point t)
(setq confirm-kill-processes nil)
(setq process-adaptive-read-buffering nil)

(when (fboundp 'pixel-scroll-precision-mode)
  (pixel-scroll-precision-mode))

;; Enable these
(dolist (c '(narrow-to-region narrow-to-page upcase-region downcase-region))
  (put c 'disabled nil))

;; And disable these
(dolist (c '(eshell project-eshell overwrite-mode iconify-frame diary))
  (put c 'disabled t))

;;; Disable menu bar
;; Not working if place it in early-init
(menu-bar-mode -1)

;;; No fringe
(fringe-mode '(3 . 0))

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

;; Disable garbage collection when entering commands
;; https://github.com/manateelazycat/lazycat-emacs/blob/master/site-lisp/config/init-performance.el
(defun max-gc-limit ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun reset-gc-limit ()
  (setq gc-cons-threshold 800000))

(add-hook 'minibuffer-setup-hook #'max-gc-limit)
(add-hook 'minibuffer-exit-hook #'reset-gc-limit)

;;; No box
(setq use-dialog-box nil)
(setq use-file-dialog nil)

;;; Revert Dired and other buffers
(setq global-auto-revert-non-file-buffers t)

;;; Set default coding system
(set-default-coding-systems 'utf-8)

;;; Turn off bell sound
(setq ring-bell-function 'ignore)

;;; Change to ~100 MB
(setq large-file-warning-threshold 100000000)

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
(setq-default recentf-exclude `("/Applications/Emacs.app/Contents/Resources/lisp"
                                ".gz" ".xz" ".zip" ".gpg" ".asc"
                                "/var" "/etc" "/usr" "/tmp"
                                "/ssh:" "/sudo:"
                                "~/.local" "~/.cache"
                                "~/Downloads" "~/Pictures"
                                "~/.emacs.d/straight"))

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

(setq eval-expression-print-length nil)

;;; Better support for files with long lines
(setq-default bidi-paragraph-direction 'left-to-right)
(setq-default bidi-inhibit-bpa t)

;;; Text mode
(setq-default fill-column 120)
;; (add-hook 'text-mode-hook 'turn-on-auto-fill)
;; (add-hook 'text-mode-hook 'visual-line-mode)
;; (add-hook 'text-mode-hook 'toggle-word-wrap)

;;; Hide "setting up indent for shell type zsh"
;; https://emacs.stackexchange.com/questions/52846/how-to-remove-message-indentation-setup-for-shell-type-sh
;; (advice-add 'sh-set-shell :around
;;             (lambda (orig-fun &rest args)
;;               (let ((inhibit-message t))
;;                 (apply orig-fun args))))

;;; Themes
(setq custom-safe-themes t)

;;; Enable push back option after deleting a file
(when (featurep 'ns)
  ;; https://christiantietze.de/posts/2021/06/emacs-trash-file-macos/
  (setq trash-directory "~/.Trash")
  (defun system-move-file-to-trash (path)
    (shell-command (concat "trash -vF \"" path "\"" "| sed -e 's/^/Trashed: /'")
                   nil
                   "*Trash Error Buffer*")))

;;; Show whitespace and delete on saving
;; https://github.com/zilongshanren/emacs.d/blob/develop/lisp/init-basic.el
(defun p-emacs-editing-enable-trailing-whitespace ()
  (setq show-trailing-whitespace t)
  (add-hook 'before-save-hook #'delete-trailing-whitespace nil t))

(dolist (hook '(prog-mode-hook markdown-mode-hook org-mode-hook conf-mode-hook text-mode-hook))
  (add-hook hook 'p-emacs-editing-enable-trailing-whitespace))

;;; Electric pair
;; (add-hook 'prog-mode-hook (lambda () (electric-pair-mode 1)))
;; (add-hook 'text-mode-hook (lambda () (electric-pair-mode 1)))

(provide 'init-defaults)
;;;;; init-defaults.el ends here
