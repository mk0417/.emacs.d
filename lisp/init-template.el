;;;;; init-template.el --- Template -*- lexical-binding: t -*-

;;; package
(straight-use-package 'tempel)

(setq tempel-path "~/Dropbox/templates")

;;; keybindings
(with-eval-after-load 'tempel
  (define-key tempel-map (kbd "M-j") 'tempel-next)
  (define-key tempel-map (kbd "M-k") 'tempel-previous))

(global-set-key (kbd "C-c t i") 'tempel-insert)
(global-set-key (kbd "C-c t t") 'tempel-complete)
(global-set-key (kbd "M-;") 'tempel-complete)

(provide 'init-template)
;;;;; init-template.el ends here
