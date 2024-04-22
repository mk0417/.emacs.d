;;;;; init-git.el --- Git -*- lexical-binding: t -*-

;;; Project
(use-package project
  :ensure nil
  :config
  (setq project-list-file (expand-file-name "projects" user-emacs-directory)))

;;; color-rg
(use-package color-rg
  :ensure t
  :vc (color-rg :url "https://github.com/manateelazycat/color-rg" :branch "master")
  :init
  (setq color-rg-mac-load-path-from-shell nil))

;;; diff-hl
(use-package diff-hl
  :ensure t
  :defer 5
  :config
  (setq diff-hl-draw-borders nil)

  (autoload 'diff-hl-mode "diff-hl")

  (add-hook 'dired-mode-hook 'diff-hl-dired-mode)
  (dolist (hook '(prog-mode-hook conf-mode-hook markdown-mode-hook))
    (add-hook hook (lambda ()
                     (diff-hl-mode)
                     (diff-hl-flydiff-mode))))

;;; Keybindings
  (with-eval-after-load 'evil
    (define-key evil-normal-state-map (kbd "gn") 'diff-hl-next-hunk)
    (define-key evil-normal-state-map (kbd "gp") 'diff-hl-previous-hunk)
    (define-key evil-normal-state-map (kbd "gP") 'diff-hl-diff-goto-hunk)))

(provide 'init-git)
