;;;;; init-completion.el --- Corfu -*- lexical-binding: t -*-

;;; package
(straight-use-package 'corfu)
(straight-use-package 'cape)
(straight-use-package 'corfu-doc)

;;; corfu
(setq corfu-cycle t)
(setq corfu-auto t)
(setq corfu-auto-prefix 1)
(setq corfu-auto-delay 0)
(setq corfu-count 5)
(setq corfu-scroll-margin 20)

(add-hook 'after-init-hook 'corfu-global-mode)

(with-eval-after-load 'corfu
  (defun no-corfu-in-minibuffer ()
    (corfu-mode -1))
  (add-hook 'minibuffer-setup-hook #'no-corfu-in-minibuffer)

  (define-key corfu-map (kbd "M-j") #'corfu-doc-scroll-down)
  (define-key corfu-map (kbd "M-k") #'corfu-doc-scroll-up)
  (define-key corfu-map (kbd "M-d") #'corfu-doc-toggle)

  (custom-set-faces
   '(corfu-border
     ((t (:background "#2fafff"))))))

;;; corfu-doc
(add-hook 'corfu-mode-hook #'corfu-doc-mode)

;;; cape
(add-to-list 'completion-at-point-functions #'cape-dabbrev)
(add-to-list 'completion-at-point-functions #'cape-keyword)
(add-to-list 'completion-at-point-functions #'cape-file)
(add-to-list 'completion-at-point-functions #'cape-tex)

(provide 'init-completion)
;;;;; init-completion.el ends here
