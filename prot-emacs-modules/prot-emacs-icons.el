;;;;; prot-emacs-icons.el --- Icons -*- lexical-binding: t -*-

;;; Icons
(use-package nerd-icons
  :ensure t)

(use-package nerd-icons-completion
  :ensure t
  :hook
  (marginalia-mode . nerd-icons-completion-mode))

(use-package nerd-icons-dired
  :ensure t
  :hook
  (dired-mode . nerd-icons-dired-mode))

(provide 'prot-emacs-icons)
