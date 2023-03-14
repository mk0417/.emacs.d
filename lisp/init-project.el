;;;;; init-project.el --- Project -*- lexical-binding: t -*-

;;; Project
(setq project-list-file (expand-file-name "projects" user-emacs-directory))

;; https://github.com/karthink/.emacs.d/blob/master/lisp/setup-project.el
(setq project-switch-commands
      '((?f "Find file" project-find-file)
        (?g "Find regexp" consult-ripgrep)
        (?d "Dired" project-dired)
        (?b "Buffer" project-switch-to-buffer)
        (?v "magit" project-magit-status)
        (?k "Kill buffers" project-kill-buffers)))

(defun project-magit-status ()
  (interactive)
  (magit-status-setup-buffer (project-root (project-current t))))

;;; List all installed package
;; https://github.com/raxod502/straight.el/issues/262
(defun p-list-installed-packages ()
  (interactive)
  (require 'magit)
  (let ((magit-repository-directories
         (list (cons (straight--repos-dir) 1))))
    (magit-list-repositories)))

;;; Grep project
(defun p-project-find-xref ()
  (interactive)
  (defvar xref-show-xrefs-function)
  (let ((xref-show-xrefs-function #'consult-xref))
    (if-let ((tap (thing-at-point 'symbol)))
        (project-find-regexp tap)
      (call-interactively #'project-find-regexp))))

;; https://gitlab.com/protesilaos/dotfiles/-/blob/master/emacs/.emacs.d/prot-lisp/prot-search.el
(defvar p-project-search--grep-hist '()
  "Input history of grep searches.")

(defun p-project-search-grep (regexp &optional recursive)
  "Run grep for REGEXP.
Search in the current directory using `lgrep'.  With optional
prefix argument (\\[universal-argument]) for RECURSIVE, run a
search starting from the current directory with `rgrep'."
  (interactive
   (list
    (read-from-minibuffer (concat (if current-prefix-arg
                                      (propertize "Recursive" 'face 'warning)
                                    "Local")
                                  " grep for PATTERN: ")
                          nil nil nil 'p-search--grep-hist)
    current-prefix-arg))
  (unless grep-command
    (grep-compute-defaults))
  (if recursive
      (rgrep regexp "*" default-directory)
    (lgrep regexp "*" default-directory)
    (add-to-history 'p-search--grep-hist regexp)))

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
    "sx" '(p-project-find-xref :which-key "project find xref")
    "s." '(p-project-search-grep :which-key "grep search")))

(provide 'init-project)
;;;;; init-project.el ends here
