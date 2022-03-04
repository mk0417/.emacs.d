;;;;; init-programming.el --- Programming -*- lexical-binding: t -*-

;;; package
(straight-use-package 'tree-sitter)
(straight-use-package 'tree-sitter-langs)
(straight-use-package '(grammatical-edit :type git :host github :repo "manateelazycat/grammatical-edit"))

;;; tree-sitter
(require 'tree-sitter)
(require 'tree-sitter-hl)
(require 'tree-sitter-langs)
(require 'tree-sitter-debug)
(require 'tree-sitter-query)

(global-tree-sitter-mode)
(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)

;; grammatical-edit
(dolist (hook (list
	       'c-mode-common-hook
	       'c-mode-hook
	       'c++-mode-hook
	       'emacs-lisp-mode-hook
	       'lisp-interaction-mode-hook
	       'lisp-mode-hook
	       'sh-mode-hook
	       'makefile-gmake-mode-hook
	       'python-mode-hook
	       'js-mode-hook
	       'go-mode-hook
	       'css-mode-hook
	       'ruby-mode-hook
	       'rust-mode-hook
	       'qmake-mode-hook
	       'lua-mode-hook
	       'web-mode-hook
	       'markdown-mode-hook
	       'conf-toml-mode-hook
	       'nim-mode-hook
	       'typescript-mode-hook))
  (add-hook hook #'(lambda () (grammatical-edit-mode 1))))

;;; keybindings
(with-eval-after-load 'evil
  (define-key evil-normal-state-map (kbd ";fs") 'grammatical-edit-wrap-single-quote)
  (define-key evil-normal-state-map (kbd ";fd") 'grammatical-edit-wrap-double-quote)
  (define-key evil-normal-state-map (kbd ";fb") 'grammatical-edit-backward-delete)
  (define-key evil-normal-state-map (kbd ";f,") 'grammatical-edit-backward-kill)
  (define-key evil-normal-state-map (kbd ";fi") 'grammatical-edit-backward-kill-internal)
  (define-key evil-normal-state-map (kbd ";f.") 'grammatical-edit-forward-delete)
  (define-key evil-normal-state-map (kbd ";fj") 'grammatical-edit-forward-delete-in-string)
  (define-key evil-normal-state-map (kbd ";fk") 'grammatical-edit-close-round)
  (define-key evil-normal-state-map (kbd ";ff") 'grammatical-edit-close-bracket)
  (define-key evil-normal-state-map (kbd ";fh") 'grammatical-edit-close-curly)
  (define-key evil-normal-state-map (kbd ";fu") 'grammatical-edit-unwrap-string)
  (define-key evil-normal-state-map (kbd ";f;") 'grammatical-edit-kill)
  (define-key evil-normal-state-map (kbd ";fl") 'grammatical-edit-jump-left)
  (define-key evil-normal-state-map (kbd ";fr") 'grammatical-edit-jump-right)
  (define-key evil-normal-state-map (kbd ";fp") 'grammatical-edit-jump-out-pair-and-newline)
  (define-key evil-normal-state-map (kbd ";fm") 'grammatical-edit-match-paren)
  (define-key evil-normal-state-map (kbd ";fn") 'grammatical-edit-newline)
  (define-key evil-normal-state-map (kbd ";fc") 'grammatical-edit-space)
  (define-key evil-normal-state-map (kbd ";ft") 'grammatical-edit-web-mode-element-wrap)
  (define-key evil-normal-state-map (kbd ";fw") 'grammatical-edit-web-mode-element-unwrap))

(provide 'init-tree-sitter)
;;;;; init-tree-sitter.el ends here
