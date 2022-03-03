;;;;; init-template.el --- Template -*- lexical-binding: t -*-

;;; package
(straight-use-package 'tempel)

;;; keybindings
(with-eval-after-load 'tempel
  (define-key tempel-map (kbd "M-j") 'tempel-next)
  (define-key tempel-map (kbd "M-k") 'tempel-previous))

(global-set-key (kbd "C-c t i") 'tempel-insert)
(global-set-key (kbd "C-c t t") 'tempel-complete)

(provide 'init-template)
;;;;; init-template.el ends here
