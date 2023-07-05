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
                       (propertize "%[" 'face 'error)
                       '(:eval
                         (concat
                          (prot-modeline-major-mode-indicator)
                          " "
                          (propertize
                           (prot-modeline-string-truncate
                            (prot-modeline-major-mode-name))
                           'mouse-face 'mode-line-highlight
                           'help-echo (prot-modeline-major-mode-help-echo))))
                       '(:eval
                         (when mode-line-process
                           (concat " " mode-line-process)))
                       (propertize "%]" 'face 'error)
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
                "  "
                prot-modeline-vc-branch
                "  "
                prot-modeline-align-right
                prot-modeline-misc-info))

(provide 'init-modeline)
;;;;; init-modeline.el ends here
