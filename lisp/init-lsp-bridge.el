;;;;; init-lsp-bridge.el --- LSP-bridge -*- lexical-binding: t -*-

;;; Install packages
(straight-use-package 'posframe)
(straight-use-package 'yasnippet)
(straight-use-package '(lsp-bridge :type git :host github :repo "manateelazycat/lsp-bridge" :files ("*")))

(setq lsp-bridge-enable-diagnostics nil)
(setq acm-enable-yas nil)
(setq lsp-bridge--mode-line-format "lsp-bridge")
(setq acm-enable-tempel nil)
(setq acm-enable-tabnine nil)
(setq lsp-bridge-default-mode-hooks
      '(python-mode-hook
        ess-r-mode-hook
        emacs-lisp-mode-hook
        text-mode-hook))

(global-lsp-bridge-mode)
(diminish 'lsp-bridge-mode)

(with-eval-after-load 'evil
  (define-key evil-normal-state-map (kbd "gow") 'lsp-bridge-workspace-list-symbols)
  (define-key evil-normal-state-map (kbd "gof") 'lsp-bridge-find-def)
  (define-key evil-normal-state-map (kbd "gob") 'lsp-bridge-find-def-return))

(provide 'init-lsp-bridge)
;;;;; init-lsp-bridge.el ends here
