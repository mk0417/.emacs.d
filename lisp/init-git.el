;;;;; init-git.el --- Git -*- lexical-binding: t -*-

;;; Install packages
(straight-use-package 'diff-hl)
(straight-use-package 'git-messenger)

;;; Diff-hl
(setq diff-hl-draw-borders nil)

(autoload 'diff-hl-mode "diff-hl")

(add-hook 'dired-mode-hook 'diff-hl-dired-mode)
(dolist (hook '(prog-mode-hook conf-mode-hook markdown-mode-hook))
  (add-hook hook (lambda ()
                   (diff-hl-margin-mode)
                   (diff-hl-mode))))

(setq diff-hl-flydiff-mode t)

;;; Git-messenger
(setq git-messenger:show-detail t)

;;; Keybindings
(with-eval-after-load 'git-messenger
  (define-key git-messenger-map (kbd "y") 'git-messenger:copy-message))

(with-eval-after-load 'diff-hl
  (define-key evil-normal-state-map (kbd "gn") 'diff-hl-next-hunk)
  (define-key evil-normal-state-map (kbd "gp") 'diff-hl-previous-hunk)
  (define-key evil-normal-state-map (kbd "gP") 'diff-hl-diff-goto-hunk))

(with-eval-after-load 'vc-dir
  (define-key vc-dir-mode-map (kbd "a") 'vc-diff))

(with-eval-after-load 'evil
  (general-create-definer p-space-leader-def
    :prefix "SPC"
    :states 'normal)
  (p-space-leader-def
    "g"  '(:ignore t :which-key "git")
    "gg" '(vc-dir :which-key "vc-dir")
    "gp" '(project-vc-dir :which-key "project-vc-dir")
    "gm" '(git-messenger:popup-message :which-key "git message")))

(provide 'init-git)
;;;;; init-git.el ends here
