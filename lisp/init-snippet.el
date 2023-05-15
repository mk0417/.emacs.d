;;;;; init-snippet.el --- Snippet -*- lexical-binding: t -*-

;;; Install packages
(straight-use-package 'tempel)
(straight-use-package 'yasnippet)

;;; Tempel
(setq tempel-path "~/Dropbox/templates")

(with-eval-after-load 'tempel
  (define-key tempel-map (kbd "M-j") 'tempel-next)
  (define-key tempel-map (kbd "M-k") 'tempel-previous))

(global-set-key (kbd "C-c t i") 'tempel-insert)
(global-set-key (kbd "C-c t t") 'tempel-complete)
(global-set-key (kbd "M-s-;") 'tempel-complete)

;;; Yasnippet
(setq yas-snippet-dirs '("~/Dropbox/snippet"))
(setq yas-also-auto-indent-first-line t)
(setq yas-verbosity 0)
(setq yas-triggers-in-field t)

(yas-global-mode 1)
(diminish 'yas-minor-mode)

(global-set-key (kbd "M-;") 'yas-expand)

(with-eval-after-load 'evil
  (general-create-definer p-space-leader-def
    :prefix "SPC"
    :states '(normal visual))
  (p-space-leader-def
    "e" '(:ignore t :which-key "editing")
    "ey" '(yas-new-snippet :which-key "create new snippet")
    "ei" '(yas-insert-snippet :which-key "insert snippet")))

(provide 'init-snippet)
;;;;; init-snippet.el ends here
