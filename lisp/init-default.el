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

(provide 'init-default)
