;;;;; init-project.el --- Project -*- lexical-binding: t -*-

(setq project-list-file (expand-file-name "projects" user-emacs-directory))

;;; List all installed package
;; https://github.com/raxod502/straight.el/issues/262
(defun p-list-installed-packages ()
  (interactive)
  (require 'magit)
  (let ((magit-repository-directories
         (list (cons (straight--repos-dir) 1))))
    (magit-list-repositories)))

(defun p-project-find-regexp ()
  (interactive)
  (defvar xref-show-xrefs-function)
  (let ((xref-show-xrefs-function #'consult-xref))
    (if-let ((tap (thing-at-point 'symbol)))
        (project-find-regexp tap)
      (call-interactively #'project-find-regexp))))

;;; Keybindings
(with-eval-after-load 'evil
  (general-create-definer p-space-leader-def
    :prefix "SPC"
    :states '(normal))
  (p-space-leader-def
    "p"  '(:ignore t :which-key "projects and packages")
    "pp" '(project-switch-project :which-key "switch project")
    "pb" '(project-switch-to-buffer :which-key "switch buffer in project")
    "pf" '(project-find-file :which-key "project find file")
    "ps" '(straight-pull-package-and-deps :which-key "straight-pull-package-and-deps")
    "pr" '(straight-remove-unused-repos :which-key "straight-remove-unused-repos")
    "pU" '(straight-pull-recipe-repositories :which-key "straight-pull-recipe-repositories")
    "pu" '(straight-pull-all  :which-key "straight update all packages")
    "pS" '(p-list-installed-packages :which-key "list installed packages")
    "s"  '(:ignore t :which-key "search")
    "sd" '(p-project-find-regexp :which-key "project find regexp")))

(provide 'init-project)
;;;;; init-project.el ends here
