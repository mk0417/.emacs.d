;;;;; prot-emacs-modeline.el --- Modeline -*- lexical-binding: t -*-

;;; Mode line
(use-package prot-modeline
  :ensure nil
  :config
  (setq mode-line-compact nil) ; Emacs 28
  (setq mode-line-right-align-edge 'right-margin) ; Emacs 30

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
                             (prot-modeline-major-mode-name)
                             'mouse-face 'mode-line-highlight
                             'help-echo (prot-modeline-major-mode-help-echo))))
                         (propertize "%]" 'face 'prot-modeline-indicator-red)
                         '(" " (:eval (get-jupyter-repl-interaction-indicator))))))

  (setq-default mode-line-format
                '("%e"
                  prot-modeline-kbd-macro
                  prot-modeline-narrow
                  prot-modeline-buffer-status
                  prot-modeline-window-dedicated-status
                  prot-modeline-input-method
                  "  "
                  prot-modeline-buffer-identification
                  "  "
                  prot-modeline-major-mode
                  prot-modeline-process
                  "  "
                  prot-modeline-vc-branch
                  "  "
                  prot-modeline-eglot
                  "  "
                  mode-line-format-right-align ; Emacs 30
                  "  "
                  prot-modeline-misc-info))

  (with-eval-after-load 'spacious-padding
    (defun prot/modeline-spacious-indicators ()
      "Set box attribute to `'prot-modeline-indicator-button' if spacious-padding is enabled."
      (if (bound-and-true-p spacious-padding-mode)
          (set-face-attribute 'prot-modeline-indicator-button nil :box t)
        (set-face-attribute 'prot-modeline-indicator-button nil :box 'unspecified)))

    ;; Run it at startup and then afterwards whenever
    ;; `spacious-padding-mode' is toggled on/off.
    (prot/modeline-spacious-indicators)

    (add-hook 'spacious-padding-mode-hook #'prot/modeline-spacious-indicators)))

;;; Keycast mode
(use-package keycast
  :ensure t
  :after prot-modeline
  :commands (keycast-mode-line-mode keycast-header-line-mode keycast-tab-bar-mode keycast-log-mode)
  :init
  (setq keycast-mode-line-format "%2s%k%c%R")
  (setq keycast-mode-line-insert-after 'prot-modeline-vc-branch)
  (setq keycast-mode-line-window-predicate 'mode-line-window-selected-p)
  (setq keycast-mode-line-remove-tail-elements nil)
  :config
  (dolist (input '(self-insert-command org-self-insert-command))
    (add-to-list 'keycast-substitute-alist `(,input "." "Typing…")))

  (dolist (event '( mouse-event-p mouse-movement-p mwheel-scroll handle-select-window
                    mouse-set-point mouse-drag-region))
    (add-to-list 'keycast-substitute-alist `(,event nil))))

(provide 'prot-emacs-modeline)
