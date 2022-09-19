;;;;; init-template.el --- Template -*- lexical-binding: t -*-

;;; Install packages
(straight-use-package 'tempel)

;;; Tempel
(setq tempel-path "~/Dropbox/templates")

;;; Keybindings
(with-eval-after-load 'tempel
  (define-key tempel-map (kbd "M-j") 'tempel-next)
  (define-key tempel-map (kbd "M-k") 'tempel-previous))

(global-set-key (kbd "C-c t i") 'tempel-insert)
(global-set-key (kbd "C-c t t") 'tempel-complete)
(global-set-key (kbd "M-;") 'tempel-complete)

(provide 'init-template)
;;;;; init-template.el ends here
