;;;;; init-lsp-bridge.el --- LSP-bridge -*- lexical-binding: t -*-

;;; Install packages
;; (straight-use-package 'posframe)
(straight-use-package '(lsp-bridge :type git :host github :repo "manateelazycat/lsp-bridge" :files ("*")))

(setq lsp-bridge-enable-diagnostics nil)
(setq acm-enable-yas nil)
(setq acm-enable-tempel nil)
(setq acm-enable-tabnine nil)
(setq acm-enable-telega nil)
(setq acm-backend-lsp-enable-auto-import nil)
(setq acm-candidate-match-function 'orderless-flex)
(setq lsp-bridge-python-lsp-server "pyright")
(setq lsp-bridge-symbols-enable-which-func t)
(setq acm-backend-search-file-words-candidate-min-length 3)
(setq acm-backend-elisp-candidate-min-length 2)
(setq acm-backend-lsp-candidate-min-length 2)

(setq lsp-bridge-default-mode-hooks
      '(python-mode-hook
        ess-r-mode-hook
        emacs-lisp-mode-hook
        LaTeX-mode-hook
        markdown-mode-hook))

(setq lsp-bridge-multi-lang-server-mode-list
      '(((python-mode python-ts-mode) . nil)))

(global-lsp-bridge-mode)
(diminish 'lsp-bridge-mode)

(with-eval-after-load 'evil
  (define-key evil-normal-state-map (kbd "gow") 'lsp-bridge-workspace-list-symbols)
  (define-key evil-normal-state-map (kbd "gof") 'lsp-bridge-find-def)
  (define-key evil-normal-state-map (kbd "gon") 'lsp-bridge-rename)
  (define-key evil-normal-state-map (kbd "gob") 'lsp-bridge-find-def-return))

(provide 'init-lsp-bridge)
;;;;; init-lsp-bridge.el ends here
