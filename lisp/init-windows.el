;;;;; init-windows.el --- Windows config -*- lexical-binding: t -*-

;;; Install packages
(straight-use-package 'winum)

;;; winum
(setq winum-auto-setup-mode-line nil)
(add-hook 'after-init-hook 'winum-mode)

;;; Split new buffer on the right by default
(setq split-height-threshold nil)

(provide 'init-windows)
;;;;; init-windows.el ends here
