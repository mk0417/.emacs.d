;;; Mode line
(prot-emacs-package prot-modeline
  (setq mode-line-compact nil) ; Emacs 28

  ;; NOTE 2023-10-05: my hack to display Jupyter indicator
  (defun get-jupyter-repl-interaction-indicator ()
    "Get the indicator for Jupyter-Repl-Interaction mode."
    (cdr (assq 'jupyter-repl-interaction-mode minor-mode-alist)))

  (setq-default prot-modeline-major-mode
                (append (list
                         (propertize "%[" 'face 'prot-modeline-indicator-red)
                         '(:eval
                           (concat
                            (prot-modeline-major-mode-indicator)
                            " "
                            (propertize
                             (prot-modeline-string-truncate
                              (prot-modeline-major-mode-name))
                             'mouse-face 'mode-line-highlight
                             'help-echo (prot-modeline-major-mode-help-echo))))
                         (propertize "%]" 'face 'prot-modeline-indicator-red)
                         '(" " (:eval (get-jupyter-repl-interaction-indicator))))))

  (setq-default mode-line-format
                '("%e"
                  prot-modeline-kbd-macro
                  prot-modeline-narrow
                  prot-modeline-input-method
                  prot-modeline-buffer-status
                  " "
                  prot-modeline-evil
                  prot-modeline-buffer-identification
                  "  "
                  prot-modeline-major-mode
                  prot-modeline-process
                  "  "
                  prot-modeline-vc-branch
                  "  "
                  prot-modeline-breadcrumb
                  "  "
                  prot-modeline-eglot
                  "  "
                  prot-modeline-flymake
                  "  "
                  prot-modeline-align-right
                  prot-modeline-misc-info))

  (prot-modeline-subtle-mode 1)

  ;; Overrides the "two-column" gimmick that I will never use.
  (define-key global-map (kbd "<f2>") #'prot-modeline-subtle-mode))

;;; Context of current item (breadcrumb)
(prot-emacs-package breadcrumb
  (:install t)
  (:delay 10)
  (setq breadcrumb-imenu-max-length 0.5)
  (setq breadcrumb-project-max-length 0.5))

;;; Keycast mode
(prot-emacs-package keycast
  (:install t)
  (:delay 60)
  (setq keycast-mode-line-format "%2s%k%c%R")
  (setq keycast-mode-line-insert-after 'prot-modeline-vc-branch)
  (setq keycast-mode-line-window-predicate 'mode-line-window-selected-p)
  (setq keycast-mode-line-remove-tail-elements nil)

  (dolist (input '(self-insert-command org-self-insert-command))
    (add-to-list 'keycast-substitute-alist `(,input "." "Typing…")))

  (dolist (event '(mouse-event-p mouse-movement-p mwheel-scroll))
    (add-to-list 'keycast-substitute-alist `(,event nil))))

(provide 'prot-emacs-modeline)
