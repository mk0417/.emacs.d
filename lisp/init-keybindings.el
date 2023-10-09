;;; init-keybindings.el --- Personal keybindings -*- lexical-binding: t -*-

(prot-emacs-package general
  (:install t)
  (general-evil-setup)
  ;; Fix leader key in Message buffer
  ;; https://github.com/noctuid/general.el/issues/493
  ;; https://github.com/noctuid/general.el/issues/493#issuecomment-913168833
  (general-add-hook 'after-init-hook
                    (lambda (&rest _)
                      (when-let ((messages-buffer (get-buffer "*Messages*")))
                        (with-current-buffer messages-buffer
                          (evil-normalize-keymaps)))) nil nil t)

  (general-create-definer p-space-leader-def
    :states '(normal visual motion emacs)
    :keymaps 'override
    :prefix "SPC")
  (p-space-leader-def
    "SPC" 'execute-extended-command
    "`" 'p-switch-to-previous-buffer
    ;; File
    "ff" 'find-file
    "fs" 'save-buffer
    "fr" 'consult-recent-file
    "fR" 'prot-simple-rename-file-and-buffer
    "fp" 'p-find-file-in-config
    "fn" 'p-find-file-in-notes
    "fj" 'p-create-scratch-file
    ;; Buffer
    "bb" 'consult-buffer
    "bi" 'ibuffer
    "bd" 'prot-simple-kill-buffer-current
    "bD" 'kill-buffer-and-window
    "br" 'revert-buffer
    "bh" 'mark-whole-buffer
    "bn" 'prot-scratch-buffer
    "bm" 'p-switch-to-messages
    ;; Windows
    "wd" 'delete-window
    "wo" 'delete-other-windows
    "wv" 'split-window-right
    "ws" 'split-window-below
    ;; Search
    "ss" 'consult-line
    "sk" 'consult-yank-pop
    "sp" 'consult-ripgrep
    "si" 'consult-imenu
    "sl" 'consult-outline
    ;; Git and project
    "gg" 'project-vc-dir
    "pp" 'p-project-switch-project
    "gd" 'color-rg-search-symbol
    "gp" 'color-rg-search-symbol-in-project
    ;; Eval
    "rb" 'eval-buffer
    ;; Toogle
    "tw" 'count-words
    "tc" 'count-lines-page
    "tm" 'toggle-frame-maximized
    ;; Quit
    "qq" 'kill-emacs
    "qr" 'restart-emacs)

  (general-create-definer p-jupyter-leader-def
    :prefix ";"
    :states '(normal visual)
    :keymaps '(python-mode-map))
  (p-jupyter-leader-def
    "jj" 'jupyter-run-repl
    "jr" 'jupyter-eval-line-or-region
    "jf" 'jupyter-eval-defun
    "je" 'p-jupyter-eval-region-dwim
    "jK" 'jupyter-repl-clear-cells
    "jI" 'jupyter-repl-interrupt-kernel
    "ji" 'jupyter-inspect-at-point
    "jC" 'jupyter-eval-remove-overlays
    "jc" 'p-jupyter-remove-line-overlay
    "jw" 'jupyter-repl-pop-to-buffer)

  (general-create-definer p-latex-leader-def
    :prefix ";"
    :states '(normal visual)
    :keymaps '(TeX-mode-map))
  (p-latex-leader-def
    "jm" 'TeX-insert-macro
    "je" 'LaTeX-environment
    "jf" 'LaTeX-fill-buffer
    "jr" 'p-run-latex
    "ja" 'TeX-command-run-all
    "jp" 'p-select-beamer-frame
    "jc" 'p-clear-latex-temp-files
    "jv" 'TeX-view)

  (general-imap "f"
    (general-key-dispatch 'self-insert-command
      :timeout 0.1
      "d" 'evil-normal-state))

  (defun p-insert-pound () (interactive) (insert "£"))
  (general-imap "y"
    (general-key-dispatch 'self-insert-command
      :timeout 0.2
      "b" 'p-insert-pound)))

(provide 'init-keybindings)
