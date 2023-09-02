;;;;; init-tree-sitter.el --- Tree-sitter -*- lexical-binding: t -*-

;;; Install package
(straight-use-package 'treesit-auto)

(require 'treesit-auto)
(setq treesit-auto-install 'prompt)
(global-treesit-auto-mode)

(provide 'init-tree-sitter)
;;;;; init-tree-sitter.el ends here
