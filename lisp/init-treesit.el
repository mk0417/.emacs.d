;;;;; init-treesit.el --- Tree-sitter -*- lexical-binding: t -*-

(setq treesit-language-source-alist
      '((bash . ("https://github.com/tree-sitter/tree-sitter-bash"))
        (css . ("https://github.com/tree-sitter/tree-sitter-css"))
        (elisp . ("https://github.com/Wilfred/tree-sitter-elisp"))
        (html . ("https://github.com/tree-sitter/tree-sitter-html"))
        (json . ("https://github.com/tree-sitter/tree-sitter-json"))
        (markdown . ("https://github.com/tree-sitter-grammars/tree-sitter-markdown" nil "tree-sitter-markdown/src"))
        (markdown-inline . ("https://github.com/tree-sitter-grammars/tree-sitter-markdown" nil "tree-sitter-markdown-inline/src"))
        (python . ("https://github.com/tree-sitter/tree-sitter-python"))
        (toml . ("https://github.com/ikatyang/tree-sitter-toml"))
        (typst . ("https://github.com/uben0/tree-sitter-typst"))
        (yaml . ("https://github.com/ikatyang/tree-sitter-yaml"))))

(setq major-mode-remap-alist
      '((conf-toml-mode . toml-ts-mode)
        (css-mode . css-ts-mode)
        (json-mode . json-ts-mode)
        (markdown-mode . markdown-ts-mode)
        (python-mode . python-ts-mode)
        (sh-mode . bash-ts-mode)
        (yaml-mode . yaml-ts-mode)))

(defun p-treesit-install-all-languages ()
  (interactive)
  (let ((languages (mapcar 'car treesit-language-source-alist)))
    (dolist (lang languages)
	  (treesit-install-language-grammar lang)
	  (message "`%s' parser was installed." lang)
	  (sit-for 1)))
  (message "All tree-sitter languages are installed."))

(provide 'init-treesit)

;;; init-treesit.el ends here
