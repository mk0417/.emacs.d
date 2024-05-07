;;;;; init-programming.el --- Programming -*- lexical-binding: t -*-

;;; Jupyter
(use-package jupyter
  :ensure t
  :config
  ;; https://github.com/nnicandro/emacs-zmq
  ;; https://github.com/nnicandro/emacs-zmq/issues/19
  ;; do not download zmq module from released version that contains .so file
  ;; Emacs 28 needs .dylib
  ;; answer No when first installation and build it to have .dylib file
  ;; (require 'jupyter)
  (setq jupyter-eval-use-overlays t)
  (setq jupyter-org-auto-connect nil)

  (with-eval-after-load 'org
    (org-babel-do-load-languages 'org-babel-load-languages '((jupyter . t))))

  (defun p-mark-paragraph ()
    (interactive)
    (if (region-active-p)
        (re-search-forward "\n[ \t]*\n[ \t]*\n*" nil 1)
      (progn
        (skip-chars-forward " \n\t")
        (when (re-search-backward "\n[ \t]*\n" nil 1)
          (goto-char (match-end 0)))
        (push-mark (point) t t)
        (re-search-forward "\n[ \t]*\n" nil 1)
        (previous-line)
        (end-of-line))))
  
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
  :config
  (setq python-indent-offset 4)
  (setq python-indent-guess-indent-offset-verbose nil)
  (setq python-indent-guess-indent-offset t)
  (add-hook 'python-mode-hook (lambda () (setq tab-width 4)))
  (add-hook 'python-mode-hook #'display-fill-column-indicator-mode)
  (add-hook 'python-mode-hook #'electric-pair-mode))

;;; R
(use-package ess 
  :ensure t
  :config
  (setq ess-imenu-use-S nil)
  (setq ess-imenu-use-p nil)
  (setq ess-indent-offset 4)
  (setq ess-use-flymake nil)
  (setq ess-indent-with-fancy-comments nil)
  (with-eval-after-load 'ess
    ;; disable flymake
    ;; (add-hook 'ess-r-mode-hook (lambda () (flymake-mode -1)))
    (add-hook 'ess-mode-hook #'display-fill-column-indicator-mode)
    (add-hook 'ess-mode-hook #'electric-pair-mode)))

;;; Julia
(use-package julia-mode
  :ensure t
  :config
  (add-hook 'julia-mode-hook #'display-fill-column-indicator-mode)
  (add-hook 'julia-mode-hook #'electric-pair-mode))

;;; HTML
(use-package htmlize
  :ensure t)

(add-hook 'mhtml-mode-hook #'turn-off-auto-fill)

(provide 'init-programming)
