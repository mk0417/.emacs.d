;;;;; init-treesit.el --- Tree-sitter -*- lexical-binding: t -*-

(setq treesit-language-source-alist
      '((bash . ("https://github.com/tree-sitter/tree-sitter-bash"))
        (css . ("https://github.com/tree-sitter/tree-sitter-css"))
        (html . ("https://github.com/tree-sitter/tree-sitter-html"))
        (json . ("https://github.com/tree-sitter/tree-sitter-json"))
        (markdown . ("https://github.com/tree-sitter-grammars/tree-sitter-markdown" nil "tree-sitter-markdown/src"))
        (markdown-inline . ("https://github.com/tree-sitter-grammars/tree-sitter-markdown" nil "tree-sitter-markdown-inline/src"))
        (org . ("https://github.com/milisims/tree-sitter-org"))
        (python . ("https://github.com/tree-sitter/tree-sitter-python"))
        (sql . ("https://github.com/m-novikov/tree-sitter-sql"))
        (toml . ("https://github.com/ikatyang/tree-sitter-toml"))
        (typst . ("https://github.com/uben0/tree-sitter-typst"))
        (yaml . ("https://github.com/ikatyang/tree-sitter-yaml"))))

(defun p-treesit-install-all-languages ()
  (interactive)
  (let ((languages (mapcar 'car treesit-language-source-alist)))
    (dolist (lang languages)
	  (treesit-install-language-grammar lang)
	  (message "`%s' parser was installed." lang)
	  (sit-for 1))))

(add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-ts-mode))
(add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))

(provide 'init-treesit)

;;; init-treesit.el ends here
