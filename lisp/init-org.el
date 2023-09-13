;;;;; init-org.el --- Org -*- lexical-binding: t -*-

(straight-use-package 'org)
(straight-use-package 'org-modern)

(require 'prot-emacs-org)

;; Org-modern
(with-eval-after-load 'org (global-org-modern-mode))

(provide 'init-org)
;;;;; init-org.el ends here
