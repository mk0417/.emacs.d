;;;;; init-git.el --- Git -*- lexical-binding: t -*-

;;; Install packages
(straight-use-package '(color-rg :type git :host github :repo "manateelazycat/color-rg"))
(straight-use-package 'diff-hl)
(straight-use-package 'git-messenger)

;;; Color-rg
(setq color-rg-mac-load-path-from-shell nil)
(require 'color-rg)

(define-key isearch-mode-map (kbd "M-s M-s") 'isearch-toggle-color-rg)

;;; Project
(require 'project)

(setq project-list-file (expand-file-name "projects" user-emacs-directory))

;; https://macowners.club/posts/custom-functions-5-navigation/
(defun p-project-switch-project (dir)
  (interactive (list (project-prompt-project-dir)))
  (let ((project-current-directory-override dir))
    (project-find-file)))

;;; Diff-hl
(setq diff-hl-draw-borders nil)

(autoload 'diff-hl-mode "diff-hl")

(add-hook 'dired-mode-hook 'diff-hl-dired-mode)
(dolist (hook '(prog-mode-hook conf-mode-hook markdown-mode-hook))
  (add-hook hook (lambda ()
                   (diff-hl-mode)
                   (diff-hl-flydiff-mode))))

;;; Git-messenger
(setq git-messenger:show-detail t)

;;; Keybindings
(with-eval-after-load 'git-messenger
  (define-key git-messenger-map (kbd "y") 'git-messenger:copy-message))

(with-eval-after-load 'diff-hl
  (define-key evil-normal-state-map (kbd "gn") 'diff-hl-next-hunk)
  (define-key evil-normal-state-map (kbd "gp") 'diff-hl-previous-hunk)
  (define-key evil-normal-state-map (kbd "gP") 'diff-hl-diff-goto-hunk))

;; (with-eval-after-load 'vc-dir
;;   (define-key vc-dir-mode-map (kbd "a") 'vc-diff))

(provide 'init-git)
;;;;; init-git.el ends here
