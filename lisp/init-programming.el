;;;;; init-programming.el --- Programming -*- lexical-binding: t -*-

;;; Jupyter
(prot-emacs-package jupyter
  (:install t)
  (:delay 2)
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
    (evil-open-below 0)
    (kill-whole-line)
    (evil-normal-state)
    (previous-line)))

;;; Python
(prot-emacs-configure
  (:delay 2)
  (setq python-indent-offset 4)
  (setq python-indent-guess-indent-offset-verbose nil)
  (setq python-indent-guess-indent-offset t)
  (add-hook 'python-mode-hook #'display-fill-column-indicator-mode)
  (add-hook 'python-mode-hook #'electric-pair-mode)

  (with-eval-after-load 'evil
    (defun p-python-indent-key ()
      (define-key evil-normal-state-map (kbd "god") 'python-indent-shift-right)
      (define-key evil-normal-state-map (kbd "gou") 'python-indent-shift-left)
      (define-key evil-visual-state-map (kbd "god") 'python-indent-shift-right)
      (define-key evil-visual-state-map (kbd "gou") 'python-indent-shift-left))

    (add-hook 'python-mode-hook 'p-python-indent-key)))

;;; R
(prot-emacs-package ess 
  (:delay 2)
  (:install t)
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
(prot-emacs-package julia-mode
  (:install t)
  (:delay 2)
  (add-hook 'julia-mode-hook #'display-fill-column-indicator-mode)
  (add-hook 'julia-mode-hook #'electric-pair-mode))

;;; Quarto
;; (prot-emacs-package quarto-mode
;;   (:install t)
;;   (:delay 2))

;;; HTML
(prot-emacs-package htmlize
  (:install t)
  (:delay 2))

(add-hook 'mhtml-mode-hook #'turn-off-auto-fill)

(provide 'init-programming)
