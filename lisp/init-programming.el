;;;;; init-programming.el --- Programming -*- lexical-binding: t -*-

;;; Jupyter
(use-package jupyter
  :ensure t
  :hook (jupyter-repl-interaction-mode . p-remove-jupyter-completion)
  :config
  ;; https://github.com/nnicandro/emacs-zmq
  ;; https://github.com/nnicandro/emacs-zmq/issues/19
  ;; do not download zmq module from released version that contains .so file
  ;; Emacs 28 needs .dylib
  ;; answer No when first installation and build it to have .dylib file
  ;; (require 'jupyter)
  (setq jupyter-eval-use-overlays t)
  (setq jupyter-org-auto-connect nil)

  (defun p-remove-jupyter-completion ()
    (setq-local completion-at-point-functions
                (delq 'jupyter-completion-at-point completion-at-point-functions)))

  ;; (add-hook 'jupyter-repl-interaction-mode-hook #'p-remove-jupyter-completion)

  (with-eval-after-load 'org
    (org-babel-do-load-languages 'org-babel-load-languages '((jupyter . t))))

  ;; After new commits of emacs-jupyter upstream,
  ;; jupyter-eval-region has three arguments
  ;; (jupyter-eval-region INSERT BEG END)
  ;; if INSERT is t, the output will replace code
  ;; and I prefer to nil
  (defun p-jupyter-eval-region-dwim ()
    (interactive)
    (p-mark-paragraph)
    (let (beg end)
      (setq beg (region-beginning) end (region-end))
      (jupyter-eval-region nil beg end)))

  (defun p-jupyter-remove-line-overlay ()
    (interactive)
    (meow-open-below)
    (kill-whole-line)
    (meow-insert-exit)
    (previous-line)))

;;; Python
(use-package python
  :ensure nil
  :hook
  ((python-mode . electric-pair-mode)
   (python-mode . (lambda () (setq tab-width 4)))
   (python-mode . display-fill-column-indicator-mode)
   ;; https://www.topbug.net/blog/2016/09/29/emacs-disable-certain-pairs-for-electric-pair-mode/
   (python-mode . (lambda ()
                         (setq-local electric-pair-inhibit-predicate
                                     `(lambda (c)
                                        (if (member c '(?{ ?\[ ?\()) t (,electric-pair-inhibit-predicate c)))))))
  :config
  (setq python-indent-offset 4)
  (setq python-indent-guess-indent-offset-verbose nil)
  (setq python-indent-guess-indent-offset t)
  (setq python-shell-interpreter "ipython"
        python-shell-interpreter-args "-i"))

;;; R
(use-package ess
  :ensure t
  :hook
  ((ess-mode . display-fill-column-indicator-mode)
   (ess-mode . electric-pair-mode))
  :config
  (setq ess-imenu-use-S nil)
  (setq ess-imenu-use-p nil)
  (setq ess-indent-offset 4)
  (setq ess-use-flymake nil)
  (setq ess-indent-with-fancy-comments nil))

;;; Julia
(use-package julia-mode
  :ensure t
  :hook
  ((julia-mode . display-fill-column-indicator-mode)
   (julia-mode . electric-pair-mode)))

(use-package eglot-jl
  :ensure t
  :hook (julia-mode . eglot-jl-init))

;;; HTML
(use-package htmlize
  :ensure t)

(add-hook 'mhtml-mode-hook #'turn-off-auto-fill)

;;; Quarto
(add-to-list 'auto-mode-alist '("\\.qmd\\'" . markdown-mode))

(defun p-quarto-render ()
  (interactive)
  (let ((file (file-name-nondirectory (buffer-file-name))))
    (shell-command (concat "quarto render " file))))

(provide 'init-programming)
