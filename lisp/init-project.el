;;;;; init-project.el --- Project -*- lexical-binding: t -*-

;;; Install package
(straight-use-package 'dumb-jump)
(straight-use-package 'rg)

;;; dumb-jump
(setq xref-show-definitions-function #'xref-show-definitions-completing-read)
(add-hook 'xref-backend-functions #'dumb-jump-xref-activate)

;;; rg
(with-eval-after-load 'rg
  (rg-define-search p-grep-vc-or-dir
    :query ask
    :format regexp
    :files "everything"
    :dir (let ((vc (vc-root-dir)))
           (if vc
               vc
             default-directory))
    :confirm prefix
    :flags ("--hidden -g !.git"))

  (define-key rg-mode-map (kbd "M-n") 'rg-next-file)
  (define-key rg-mode-map (kbd "M-p") 'rg-prev-file)

  (advice-add 'wgrep-change-to-wgrep-mode :after #'evil-normal-state)
  (advice-add 'wgrep-to-original-mode :after #'evil-motion-state)
  (defvar rg-mode-map)
  (add-to-list 'evil-motion-state-modes 'rg-mode)
  (evil-add-hjkl-bindings rg-mode-map 'motion
    "e" #'wgrep-change-to-wgrep-mode
    "R" #'rg-recompile
    "t" #'rg-rerun-change-literal))

;;; Project
(setq project-list-file (expand-file-name "projects" user-emacs-directory))

;; https://github.com/karthink/.emacs.d/blob/master/lisp/setup-project.el
(setq project-switch-commands
      '((?f "Find file" project-find-file)
        (?g "Find regexp" consult-ripgrep)
        (?d "Dired" project-dired)
        (?b "Buffer" project-switch-to-buffer)
        (?k "Kill buffers" project-kill-buffers)))

;; https://macowners.club/posts/custom-functions-5-navigation/
(defun p-project-switch-project (dir)
  (interactive (list (project-prompt-project-dir)))
  (let ((project-current-directory-override dir))
    (project-find-file)))

;;; Keybindings
(with-eval-after-load 'evil
  (general-create-definer p-space-leader-def
    :prefix "SPC"
    :states '(normal))
  (p-space-leader-def
    "p"  '(:ignore t :which-key "projects and packages")
    "pP" '(project-switch-project :which-key "switch project")
    "pp" '(p-project-switch-project :which-key "dwim switch project")
    "pb" '(project-switch-to-buffer :which-key "switch buffer in project")
    "pf" '(project-find-file :which-key "project find file")
    "ps" '(straight-pull-package-and-deps :which-key "straight-pull-package-and-deps")
    "pr" '(straight-remove-unused-repos :which-key "straight-remove-unused-repos")
    "pU" '(straight-pull-recipe-repositories :which-key "straight-pull-recipe-repositories")
    "pu" '(straight-pull-all  :which-key "straight update all packages")
    "s"  '(:ignore t :which-key "search")
    "s," '(p-grep-vc-or-dir :which-key "p-grep-vc-or-dir")
    "s." '(rg  :which-key "rg")))

(provide 'init-project)
;;;;; init-project.el ends here
