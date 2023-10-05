;;;;; init-modeline.el --- Modeline -*- lexical-binding: t -*-

(require 'prot-modeline)

(setq mode-line-percent-position '(-3 "%p"))
(setq mode-line-position-column-line-format '("%l,%c"))
(setq mode-line-compact nil)

;; Full path in mode-line
;; (setq-default prot-modeline-buffer-identification
;;               (list 'buffer-file-name
;;                     '(:eval (propertize (format "  %s" buffer-file-truename)))))

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
                mode-line-modified
                prot-modeline-narrow
                prot-modeline-input-method
                prot-modeline-buffer-status
                " "
                prot-modeline-buffer-identification
                "  "
                mode-line-position
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
                prot-modeline-align-right
                prot-modeline-misc-info))

(prot-modeline-subtle-mode 1)

;;; Context of current item (breadcrumb)
(straight-use-package 'breadcrumb)

(setq breadcrumb-imenu-max-length 0.5)
(setq breadcrumb-project-max-length 0.5)

(provide 'init-modeline)
;;;;; init-modeline.el ends here
