;;; init-completion.el --- Corfu -*- lexical-binding: t -*-

;; package
(straight-use-package 'corfu)
(straight-use-package 'cape)

;; corfu
(setq corfu-cycle t)
(setq corfu-auto t)
(setq corfu-auto-prefix 1)
(setq corfu-auto-delay 0)
(setq corfu-count 5)
(setq corfu-scroll-margin 20)

(add-hook 'after-init-hook 'corfu-global-mode)

(with-eval-after-load 'corfu
  (custom-set-faces
   '(corfu-border
     ((t (:background "#69adc6"))))))

;; cape
(dolist (c '(cape-file cape-keyword cape-dabbrev cape-line cape-symbol cape-tex cape-abbrev cape-dict cape-ispell))
  (add-to-list 'completion-at-point-functions c))

(provide 'init-completion)
;;; init-completion.el ends here
