;;;;; init-git.el --- Git -*- lexical-binding: t -*-

;;; Project
(require 'project)

(setq project-list-file (expand-file-name "projects" user-emacs-directory))

;; https://macowners.club/posts/custom-functions-5-navigation/
(defun p-project-switch-project (dir)
  (interactive (list (project-prompt-project-dir)))
  (let ((project-current-directory-override dir))
    (project-find-file)))

;;; color-rg
(prot-emacs-configure
  (:delay 10)
  (setq color-rg-mac-load-path-from-shell nil)
  (prot-emacs-package color-rg
    (:install "https://github.com/manateelazycat/color-rg")))

;;; diff-hl
(prot-emacs-package diff-hl
  (:install t)
  (:delay 5)
  (setq diff-hl-draw-borders nil)

  (autoload 'diff-hl-mode "diff-hl")

  (add-hook 'dired-mode-hook 'diff-hl-dired-mode)
  (dolist (hook '(prog-mode-hook conf-mode-hook markdown-mode-hook))
    (add-hook hook (lambda ()
                     (diff-hl-mode)
                     (diff-hl-flydiff-mode)))))

;;; Keybindings
(with-eval-after-load 'diff-hl
  (define-key evil-normal-state-map (kbd "gn") 'diff-hl-next-hunk)
  (define-key evil-normal-state-map (kbd "gp") 'diff-hl-previous-hunk)
  (define-key evil-normal-state-map (kbd "gP") 'diff-hl-diff-goto-hunk))

(provide 'init-git)
