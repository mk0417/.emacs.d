;;; init-default.el --- My default -*- lexical-binding: t -*-

;;; Produce backtraces when errors occur: can be helpful to diagnose startup issues
(setq debug-on-error t)

;;; No fringe
(fringe-mode '(0 . 0))

;;; Recentf
;; (setq recentf-max-saved-items 10)
;; (recentf-mode 1)
;; (add-to-list 'recentf-exclude "/var/folders/.*")
;; (add-to-list 'recentf-exclude "/private/var/folders/.*")

;;; Line number
(when (fboundp 'display-line-numbers-mode)
  (setq-default display-line-numbers-width 3)
  (add-hook 'prog-mode-hook 'display-line-numbers-mode))

;;; Column indicator
(when (boundp 'display-fill-column-indicator)
  (setq-default display-fill-column-indicator-column 80)
  (custom-set-faces
   '(fill-column-indicator
     ((t (:background unspecified :foreground "grey30"))))))

(define-key minibuffer-local-map (kbd "C-k") 'delete-backward-char)
(define-key minibuffer-local-map (kbd "C-w") 'backward-kill-word)

;;; Enable push back option after deleting a file
(when (featurep 'ns)
  ;; https://christiantietze.de/posts/2021/06/emacs-trash-file-macos/
  (setq trash-directory "~/.Trash")
  (defun system-move-file-to-trash (path)
    (shell-command (concat "trash -vF \"" path "\"" "| sed -e 's/^/Trashed: /'")
                   nil
                   "*Trash Error Buffer*")))

;;; Whitespace
(setq-default show-trailing-whitespace nil)

(defun p-show-trailing-whitespace ()
  "Enable display of trailing whitespace in this buffer."
  (setq-local show-trailing-whitespace t))

(dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
  (add-hook hook 'p-show-trailing-whitespace))

(use-package whitespace-cleanup-mode
  :ensure t
  :hook (after-init . global-whitespace-cleanup-mode)
  :config
  (global-set-key [remap just-one-space] 'cycle-spacing))

;;; Better default
;; https://github.com/manateelazycat/lazycat-emacs/blob/master/site-lisp/config/init-generic.el
(setq process-adaptive-read-buffering nil)
(setq inhibit-compacting-font-caches t)
(setq confirm-kill-processes nil)

(setq-default bidi-display-reordering nil)
(setq-default bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t
      long-line-threshold 1000
      large-hscroll-threshold 1000
      syntax-wholeline-max 1000)

(setq scroll-step 1
      scroll-conservatively 10000)

;; Disable garbage collection when entering commands.
(defun max-gc-limit ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun reset-gc-limit ()
  (setq gc-cons-threshold 800000))

(add-hook 'minibuffer-setup-hook #'max-gc-limit)
(add-hook 'minibuffer-exit-hook #'reset-gc-limit)

(provide 'init-default)
