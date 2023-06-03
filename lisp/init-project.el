;;;;; init-project.el --- Project -*- lexical-binding: t -*-

;;; Install package
(straight-use-package 'dumb-jump)
(straight-use-package '(color-rg :type git :host github :repo "manateelazycat/color-rg"))

;;; dumb-jump
(setq xref-show-definitions-function #'xref-show-definitions-completing-read)
(add-hook 'xref-backend-functions #'dumb-jump-xref-activate)

;;; color-rg
(setq color-rg-mac-load-path-from-shell nil)
(require 'color-rg)

(define-key isearch-mode-map (kbd "M-s M-s") 'isearch-toggle-color-rg)

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
    "p" '(:ignore t :which-key "projects and packages")
    "pP" '(project-switch-project :which-key "switch project")
    "pp" '(p-project-switch-project :which-key "dwim switch project")
    "pb" '(project-switch-to-buffer :which-key "switch buffer in project")
    "pf" '(project-find-file :which-key "project find file")
    "ps" '(straight-pull-package-and-deps :which-key "straight-pull-package-and-deps")
    "pr" '(straight-remove-unused-repos :which-key "straight-remove-unused-repos")
    "pU" '(straight-pull-recipe-repositories :which-key "straight-pull-recipe-repositories")
    "pu" '(straight-pull-all  :which-key "straight update all packages")
    "g" '(:ignore t :which-key "git")
    "gd" '(color-rg-search-input :which-key "color-rg-search-input")
    "gD" '(color-rg-search-symbol :which-key "color-rg-search-symbol")
    "gf" '(color-rg-search-input-in-current-file :which-key "color-rg-search-input-in-current-file")
    "gF" '(color-rg-search-symbol-in-current-file :which-key "color-rg-search-symbol-in-current-file")
    "gp" '(color-rg-search-input-in-project :which-key "color-rg-search-input-in-project")
    "gP" '(color-rg-search-symbol-in-project :which-key "color-rg-search-symbol-in-project")
    "gt" '(color-rg-search-project-with-type :which-key "color-rg-search-project-with-type")
    "gT" '(color-rg-search-symbol-with-type :which-key "color-rg-search-symbol-with-type")))

(provide 'init-project)
;;;;; init-project.el ends here
