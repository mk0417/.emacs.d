;;;;; init-lsp.el --- LSP -*- lexical-binding: t -*-

(use-package yasnippet
  :ensure t
  :hook (after-init . yas-global-mode)
  :config
  (setq yas-verbosity 0))

(prot-emacs-configure
  (prot-emacs-install
    lsp-bridge
    "https://github.com/manateelazycat/lsp-bridge")

  (setq lsp-bridge-enable-mode-line nil)
  (setq lsp-bridge-enable-diagnostics nil)
  (setq lsp-bridge-enable-hover-diagnostic t)
  (setq lsp-bridge-enable-completion-in-string t)
  (setq lsp-bridge-symbols-enable-which-func t)
  (setq lsp-bridge-markdown-lsp-server 'marksman)
  (setq lsp-bridge-python-lsp-server "basedpyright")
  (setq lsp-bridge-multi-lang-server-mode-list '(((python-mode python-ts-mode) . nil)))
  (setq lsp-bridge-code-action-enable-popup-menu nil)
  ;; (setq acm-enable-capf t)
  (setq acm-enable-lsp-workspace-symbol t)
  (setq acm-enable-codeium nil)
  (setq acm-enable-yas nil)
  (setq acm-enable-tempel nil)
  (setq acm-enable-tabnine nil)
  (setq acm-enable-telega nil)
  (setq acm-backend-lsp-enable-auto-import nil)
  (setq acm-candidate-match-function 'orderless-flex)
  (setq lsp-bridge-default-mode-hooks
        '(python-mode-hook
          python-ts-mode-hook
          ess-mode-hook
          julia-mode-hook
          emacs-lisp-mode-hook
          LaTeX-mode-hook
          markdown-mode-hook
          html-mode-hook))
  (setq lsp-bridge-default-mode-hooks (remove typst-ts-mode-hook lsp-bridge-default-mode-hooks))

  (global-lsp-bridge-mode))

(provide 'init-lsp)
