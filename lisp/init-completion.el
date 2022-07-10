;;;;; init-completion.el -*- lexical-binding: t -*-

;;; Install packages
(straight-use-package '(corfu :files ("*.el" "extensions/*.el")))
(straight-use-package 'corfu-doc)
(straight-use-package 'cape)

;;; Corfu
;; setup corfu for popup like completion
(setq corfu-cycle t)
(setq corfu-auto t)
(setq corfu-auto-prefix 2)
(setq corfu-auto-delay 0)
(setq corfu-echo-documentation 0.25)
(setq corfu-quit-no-match t)
(setq corfu-scroll-margin 20)

(global-corfu-mode 1)

(with-eval-after-load 'corfu
  (defun no-corfu-in-minibuffer ()
    (corfu-mode -1))
  (add-hook 'minibuffer-setup-hook #'no-corfu-in-minibuffer)
  (define-key corfu-map (kbd "M-p") #'corfu-doc-scroll-down)
  (define-key corfu-map (kbd "M-n") #'corfu-doc-scroll-up)
  (define-key corfu-map (kbd "M-d") #'corfu-doc-toggle)
  (define-key corfu-map (kbd "M-g") #'corfu-quit)
  (custom-set-faces
   '(corfu-border
     ((t (:background "#2fafff"))))))
(add-hook 'corfu-mode-hook #'corfu-doc-mode)

;;; Cape
;; setup Cape for better completion-at-point support and more
(require 'cape)
;; add useful defaults completion sources from cape
(add-to-list 'completion-at-point-functions #'cape-file)
(add-to-list 'completion-at-point-functions #'cape-dabbrev)
;; silence the pcomplete capf, no errors or messages!
(advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)
;; ensure that pcomplete does not write to the buffer and behaves as a pure `completion-at-point-function'
(advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify)

(provide 'init-completion)
;;;;; init-completion.el ends here
