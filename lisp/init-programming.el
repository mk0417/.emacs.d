;;; init-programming.el --- Programming -*- lexical-binding: t -*-

;; package
(straight-use-package 'jupyter)
(straight-use-package 'ess)
(straight-use-package '(ess-stata-mode :type git :host github :repo "emacs-ess/ess-stata-mode"))
(straight-use-package 'julia-mode)
(straight-use-package 'go-mode)
(straight-use-package 'anaconda-mode)
(straight-use-package 'company-anaconda)

;; Jupyter
;; https://github.com/nnicandro/emacs-zmq
;; https://github.com/nnicandro/emacs-zmq/issues/19
;; Do not download zmq module from released version that contains .so file
;; Emacs 28 needs .dylib
;; Answer No when first installation and build it to have .dylib file
(setq jupyter-eval-use-overlays t)

(defun p-jupyter-remove-line-overlay ()
  (interactive)
  (evil-open-below 0)
  (kill-whole-line)
  (evil-escape)
  (previous-line))

(defun p-jupyter-eval-block ()
  (interactive)
  (p-select-block)
  (let (beg end)
    (setq beg (region-beginning) end (region-end))
    (jupyter-eval-region beg end)))

;; Python
(with-eval-after-load 'python
  (setq python-indent-guess-indent-offset-verbose nil)
  (setq python-indent-guess-indent-offset nil)
  (add-hook 'python-mode-hook 'display-fill-column-indicator-mode)
  (add-hook 'python-mode-hook 'anaconda-mode)
  (add-hook 'python-mode-hook 'anaconda-eldoc-mode))

(with-eval-after-load 'anaconda-mode
  (diminish 'anaconda-mode))

(eval-after-load "company"
 '(add-to-list 'company-backends 'company-anaconda))

;; R
(with-eval-after-load 'ess
  ;; disable flymake
  (add-hook 'ess-r-mode-hook (lambda () (flymake-mode -1)))
  (add-hook 'ess-mode-hook 'display-fill-column-indicator-mode)
  (setq ess-ask-for-ess-directory nil)
  ;; fix: Error running timer 'ess--idle-timer-function': (wrong-type-argument stringp nil)
  ;; https://github.com/emacs-ess/ESS/issues/1102
  (setq ess-can-eval-in-background nil))

;; Stata
;; https://github.com/hieutkt/.doom.d/blob/master/config.el
(setq inferior-STA-start-args "")
(setq inferior-STA-program (executable-find "stata-mp"))
;; make stata src work in org
(setq inferior-STA-program-name "")

(with-eval-after-load 'ess
  (general-create-definer p-ess-leader-def
    :prefix ";"
    :states '(normal visual)
    :keymaps 'ess-mode-map)
  (p-ess-leader-def
   "r" '(:ignore t :which-key "eval")
   "ra" 'ess-cycle-assign
   "rf" 'ess-eval-function
   "rl" 'ess-eval-line
   "rr" 'ess-eval-region-or-line-and-step))

;; Julia
(with-eval-after-load 'julia-mode
  (add-hook 'julia-mode-hook 'display-fill-column-indicator-mode))

;; golang
(autoload 'go-mode "go-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))

;; keybindings
(with-eval-after-load 'evil
  (define-key evil-normal-state-map (kbd "gcr") 'anaconda-mode-find-references)
  (define-key evil-normal-state-map (kbd "gca") 'anaconda-mode-find-assignments)

  (general-create-definer p-jupyter-leader-def
    :prefix ";"
    :states '(normal visual)
    :keymaps '(python-mode-map julia-mode-map))
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
    "jw" 'jupyter-repl-pop-to-buffer)

  (general-create-definer p-space-leader-def
    :prefix "SPC"
    :states '(normal))
  (p-space-leader-def
    "p"   '(:ignore t :which-key "projects and packages")
    "pp"  '(project-switch-project :which-key "switch project")
    "pb"  '(project-switch-to-buffer :which-key "switch buffer in project")
    "pf"  '(project-find-file :which-key "project find file")))

(provide 'init-programming)
;;; init-programming.el ends here
