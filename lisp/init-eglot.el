;;;;; init-eglot.el --- Eglot -*- lexical-binding: t -*-

;;; Install package
(straight-use-package 'eglot)

(setq eglot-ignored-server-capabilities '(:documentHighlightProvider))
(setq eldoc-echo-area-use-multiline-p nil)
(add-hook 'python-mode-hook 'eglot-ensure)

;; Disable flymake
;; https://github.com/joaotavora/eglot/issues/660#issuecomment-813366843
(add-hook 'python-mode-hook
          (lambda ()
            (setq-local eglot-stay-out-of '(flymake))
            (add-hook 'flymake-diagnostic-functions 'eglot-flymake-backend nil t)))

(with-eval-after-load 'eglot
  (setq completion-category-defaults nil))

;;; Keybindings
(with-eval-after-load 'evil
  (define-key evil-normal-state-map (kbd "gon") 'eglot-rename))

(provide 'init-eglot)
;;;;; init-eglot.el ends here
