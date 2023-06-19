;;;;; init-programming.el --- Programming -*- lexical-binding: t -*-

;;; Install package
;; Use my fork due to issue of current upstream
;; https://github.com/nnicandro/emacs-jupyter/issues/433
;; (straight-use-package '(jupyter :type git :host github :repo "mk0417/emacs-jupyter" :branch "patch-1"))
(straight-use-package 'jupyter)

;;; Jupyter
;; https://github.com/nnicandro/emacs-zmq
;; https://github.com/nnicandro/emacs-zmq/issues/19
;; do not download zmq module from released version that contains .so file
;; Emacs 28 needs .dylib
;; answer No when first installation and build it to have .dylib file
;; (require 'jupyter)
(setq jupyter-eval-use-overlays t)

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
  (previous-line))

;;; Python
(setq python-indent-offset 4)
(setq python-indent-guess-indent-offset-verbose nil)
(setq python-indent-guess-indent-offset t)
(add-hook 'python-mode-hook 'display-fill-column-indicator-mode)

(provide 'init-programming)
;;;;; init-programming.el ends here
