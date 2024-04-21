;;;;; init-lsp-bridge.el --- LSP-bridge -*- lexical-binding: t -*-

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
  (setq acm-backend-lsp-enable-auto-import nil)
  ;; (setq acm-backend-search-file-words-candidate-min-length 3)
  ;; (setq acm-backend-elisp-candidate-min-length 2)
  ;; (setq acm-backend-lsp-candidate-min-length 2)
  )

(prot-emacs-package lsp-bridge
  (:install "https://github.com/manateelazycat/lsp-bridge")
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

  (global-lsp-bridge-mode))

(with-eval-after-load 'evil
  (define-key evil-normal-state-map (kbd "gd") 'lsp-bridge-find-def)
  (define-key evil-normal-state-map (kbd "gD") 'lsp-bridge-find-def-return))

(provide 'init-lsp-bridge)
