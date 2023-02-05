;;;;; init-programming.el --- Programming -*- lexical-binding: t -*-

;;; Install package
;; Use my fork due to issue of current upstream
;; https://github.com/nnicandro/emacs-jupyter/issues/433
;; (straight-use-package '(jupyter :type git :host github :repo "mk0417/emacs-jupyter" :branch "patch-1"))
(straight-use-package 'jupyter)
(straight-use-package 'ess)

;;; Jupyter
;; https://github.com/nnicandro/emacs-zmq
;; https://github.com/nnicandro/emacs-zmq/issues/19
;; do not download zmq module from released version that contains .so file
;; Emacs 28 needs .dylib
;; answer No when first installation and build it to have .dylib file
;; (require 'jupyter)

(setq jupyter-eval-use-overlays t)

(defun p-jupyter-remove-line-overlay ()
  (interactive)
  (evil-open-below 0)
  (kill-whole-line)
  (evil-escape)
  (previous-line))

;;;###autoload
(defun p-jupyter-eval-block ()
  (interactive)
  (p-select-block)
  (let (beg end)
    (setq beg (region-beginning) end (region-end))
    (jupyter-eval-region beg end)))

(autoload 'p-jupyter-eval-block "jupyter")

;;; Python
(with-eval-after-load 'python
  (setq python-indent-guess-indent-offset-verbose nil)
  (setq python-indent-guess-indent-offset nil)
  (add-hook 'python-mode-hook 'display-fill-column-indicator-mode))

;;; R
(setq-default inferior-R-args "--no-save ")

(with-eval-after-load 'ess
  ;; disable flymake
  (add-hook 'ess-r-mode-hook (lambda () (flymake-mode -1)))
  (add-hook 'ess-mode-hook 'display-fill-column-indicator-mode)
  (setq ess-ask-for-ess-directory nil)
  ;; fix: Error running timer 'ess--idle-timer-function': (wrong-type-argument stringp nil)
  ;; https://github.com/emacs-ess/ESS/issues/1102
  (setq ess-can-eval-in-background nil))

(with-eval-after-load 'ess
  (general-create-definer p-ess-leader-def
    :prefix ";"
    :states '(normal visual)
    :keymaps 'ess-mode-map)
  (p-ess-leader-def
   "r" '(:ignore t :which-key "eval")
   "rp" 'ess-request-a-process
   "ri" 'ess-interrupt
   "ra" 'ess-cycle-assign
   "rf" 'ess-eval-function
   "rl" 'ess-eval-line
   "rr" 'ess-eval-region-or-line-and-step))

;;; Keybindings
(with-eval-after-load 'evil
  (general-create-definer p-jupyter-leader-def
    :prefix ";"
    :states '(normal visual)
    :keymaps '(python-mode-map ess-r-mode-map))
  (p-jupyter-leader-def
    "j"  '(:ignore t :which-key "jupyter")
    "jj" 'jupyter-run-repl
    "jr" 'jupyter-eval-line-or-region
    "jf" 'jupyter-eval-defun
    "je" 'p-jupyter-eval-block
    "jR" 'jupyter-repl-restart-kernel
    "jK" 'jupyter-repl-clear-cells
    "jI" 'jupyter-repl-interrupt-kernel
    "ji" 'jupyter-inspect-at-point
    "jC" 'jupyter-eval-remove-overlays
    "jc" 'p-jupyter-remove-line-overlay
    "jw" 'jupyter-repl-pop-to-buffer))

(provide 'init-programming)
;;;;; init-programming.el ends here
