;;;;; init-modeline.el --- Modeline -*- lexical-binding: t -*-

(require 'prot-modeline)

(setq mode-line-percent-position '(-3 "%p"))
(setq mode-line-position-column-line-format '("%l,%c"))
(setq mode-line-compact nil)

;; Full path in mode-line
(setq prot-modeline-buffer-identification
      (list 'buffer-file-name
            '(:eval (propertize (format "  %s" buffer-file-truename)))))

(defun get-jupyter-repl-interaction-indicator ()
  "Get the indicator for Jupyter-Repl-Interaction mode."
  (cdr (assq 'jupyter-repl-interaction-mode minor-mode-alist)))

(setq prot-modeline-major-mode
      (append (list (propertize "%[" 'face 'error)
                    '(:eval
                      (concat
                       ;; P-NOTE 2023-07-01: I change the unicode symbol
                       (propertize (char-to-string #x1F11C) 'face 'shadow)
                       " "
                       (propertize
                        (capitalize
                         (string-replace
                          "-mode"
                          ""
                          (symbol-name major-mode)))
                        'mouse-face 'mode-line-highlight))
                      '("" mode-line-process)
                      (propertize "%]" 'face 'error))
                    '(" " (:eval (get-jupyter-repl-interaction-indicator)))
                    " ")))

(setq-default mode-line-format
              '("%e"
                prot-modeline-kbd-macro
                " "
                mode-line-mule-info
                mode-line-modified
                mode-line-remote
                " "
                prot-modeline-buffer-identification
                "  "
                prot-modeline-position
                "  "
                prot-modeline-major-mode
                "  "
                prot-modeline-vc-branch
                "  "
                prot-modeline-align-right
                prot-modeline-misc-info))

(add-hook 'after-init-hook #'column-number-mode)

(provide 'init-modeline)
;;;;; init-modeline.el ends here
