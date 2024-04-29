;;;;; init-lsp.el --- LSP -*- lexical-binding: t -*-

(use-package yasnippet
  :ensure t
  :hook (after-init . yas-global-mode)
  :config
  (setq yas-verbosity 0)
  ;; (require 'yasnippet)
  (setq lsp-bridge-enable-mode-line nil)
  (setq lsp-bridge-enable-diagnostics nil)
  (setq lsp-bridge-symbols-enable-which-func t)
  (setq acm-enable-yas nil)
  (setq acm-enable-tempel nil)
  (setq acm-enable-tabnine nil)
  (setq acm-enable-telega nil)
  (setq acm-candidate-match-function 'orderless-flex)
  (setq acm-backend-lsp-enable-auto-import nil))

(prot-emacs-package-install 'lsp-bridge '(lsp-bridge :url "https://github.com/manateelazycat/lsp-bridge"))

(setq lsp-bridge-default-mode-hooks
      '(python-mode-hook
        ess-mode-hook
        julia-mode-hook
        emacs-lisp-mode-hook
        LaTeX-mode-hook
        markdown-mode-hook
        html-mode-hook))
(setq lsp-bridge-multi-lang-server-mode-list
      '(((python-mode) . nil)))

(global-lsp-bridge-mode)


(provide 'init-lsp)
