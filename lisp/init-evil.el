;;; init-evil.el --- Evil -*- lexical-binding: t -*-
(prot-emacs-configure
  (:delay 1)

  (prot-emacs-package evil-surround
    (:install t)
    (:delay 2)
    (global-evil-surround-mode 1))

  (prot-emacs-package general
    (:install t)
    (general-evil-setup)

    (general-imap "f"
      (general-key-dispatch 'self-insert-command
        :timeout 0.1
        "d" 'evil-normal-state))

    (defun p-insert-pound () (interactive) (insert "£"))
    (general-imap "y"
      (general-key-dispatch 'self-insert-command
        :timeout 0.2
        "b" 'p-insert-pound)))

  (with-eval-after-load 'evil
    ;; Vim style replace
    (defun p-ex-evil-buffer-replace ()
      (interactive)
      (evil-ex (concat "%s/")))

    (defun p-ex-evil-selection-replace ()
      (interactive)
      (evil-ex (concat "'<,'>s/")))

    (defun p-ex-evil-selection-replace-yank ()
      (interactive)
      (evil-ex (concat "'<,'>s/" (substring-no-properties (car kill-ring)) "/")))

    (defun p-ex-evil-replace-yank ()
      (interactive)
      (kill-new (thing-at-point 'symbol))
      (p-mark-paragraph)
      ;; make sure to run evil ex after above functions
      ;; https://emacs.stackexchange.com/questions/11003/run-a-function-after-control-returns-to-the-command-loop
      (run-with-timer 0 nil 'p-ex-evil-selection-replace-yank))

    (defun p-ex-evil-replace-yank-def ()
      (interactive)
      (kill-new (thing-at-point 'symbol))
      (p-mark-defun)
      (run-with-timer 0 nil 'p-ex-evil-selection-replace-yank))

    (define-key evil-normal-state-map (kbd ".") nil)
    (define-key evil-normal-state-map (kbd "f") nil)
    (define-key evil-normal-state-map (kbd "m") nil)
    (define-key evil-normal-state-map (kbd "C-.") nil)
    (define-key evil-normal-state-map (kbd "C-r") nil)
    (define-key evil-normal-state-map (kbd "C-n") nil)
    (define-key evil-normal-state-map (kbd "C-p") nil)
    (define-key evil-insert-state-map (kbd "C-o") nil)
    (define-key evil-insert-state-map (kbd "C-n") nil)
    (define-key evil-insert-state-map (kbd "C-p") nil)
    (define-key evil-motion-state-map (kbd "C-e") nil)
    (define-key evil-motion-state-map (kbd "C-o") nil)

    (define-key evil-inner-text-objects-map "d" 'evil-inner-double-quote)
    (define-key evil-inner-text-objects-map "s" 'evil-inner-single-quote)
    (define-key evil-inner-text-objects-map "f" 'evil-inner-bracket)
    (define-key evil-inner-text-objects-map "h" 'evil-inner-curly)
    (define-key evil-inner-text-objects-map "a" 'evil-inner-angle)

    (define-key evil-outer-text-objects-map "d" 'evil-a-double-quote)
    (define-key evil-outer-text-objects-map "s" 'evil-a-single-quote)
    (define-key evil-outer-text-objects-map "f" 'evil-a-bracket)
    (define-key evil-outer-text-objects-map "h" 'evil-a-curly)
    (define-key evil-outer-text-objects-map "a" 'evil-an-angle)

    (define-key evil-normal-state-map (kbd ";a") 'evil-first-non-blank)
    (define-key evil-normal-state-map (kbd ";e") 'evil-last-non-blank)
    (define-key evil-normal-state-map (kbd ",,") 'p-mark-paragraph)
    (define-key evil-normal-state-map (kbd ",.") 'p-mark-defun)
    (define-key evil-normal-state-map (kbd ",b") 'p-mark-paragraph-below)
    (define-key evil-normal-state-map (kbd ",a") 'beginning-of-defun)
    (define-key evil-normal-state-map (kbd ",e") 'end-of-defun)
    (define-key evil-normal-state-map (kbd ",k") 'p-insert-surround-parentheses)
    (define-key evil-normal-state-map (kbd ",f") 'p-insert-surround-bracket)
    (define-key evil-normal-state-map (kbd ",h") 'p-insert-surround-curly-bracket)
    (define-key evil-normal-state-map (kbd ",s") 'p-insert-surround-single-quote)
    (define-key evil-normal-state-map (kbd ",d") 'p-insert-surround-double-quotes)
    (define-key evil-normal-state-map (kbd "gom") 'execute-extended-command)
    (define-key evil-normal-state-map (kbd "goo") 'indent-region)
    (define-key evil-normal-state-map (kbd "goi") 'p-format-indent-in-buffer)
    (define-key evil-normal-state-map (kbd "gor") 'p-ex-evil-buffer-replace)
    (define-key evil-normal-state-map (kbd "goa") 'p-ex-evil-replace-yank)
    (define-key evil-normal-state-map (kbd "gof") 'p-ex-evil-replace-yank-def)
    (define-key evil-normal-state-map (kbd "gol") 'p-mark-line-non-blank)
    (define-key evil-normal-state-map (kbd "gcc") 'prot-comment)
    (define-key evil-normal-state-map (kbd "gcl") 'p-clear-line)
    (define-key evil-normal-state-map (kbd "gcs") 'p-remove-extra-spaces)
    (define-key evil-normal-state-map (kbd "gc.") 'p-delete-between-commas)
    (define-key evil-normal-state-map (kbd "gc,") 'p-select-between-commas)

    (define-key evil-visual-state-map (kbd ";a") 'evil-first-non-blank)
    (define-key evil-visual-state-map (kbd ";e") 'evil-last-non-blank)
    (define-key evil-visual-state-map (kbd ",,") 'p-mark-paragraph)
    (define-key evil-visual-state-map (kbd ",b") 'p-mark-paragraph-below)
    (define-key evil-visual-state-map (kbd ",k") 'p-insert-surround-parentheses)
    (define-key evil-visual-state-map (kbd ",a") 'beginning-of-defun)
    (define-key evil-visual-state-map (kbd ",e") 'end-of-defun)
    (define-key evil-visual-state-map (kbd ",f") 'p-insert-surround-bracket)
    (define-key evil-visual-state-map (kbd ",h") 'p-insert-surround-curly-bracket)
    (define-key evil-visual-state-map (kbd ",s") 'p-insert-surround-single-quote)
    (define-key evil-visual-state-map (kbd ",d") 'p-insert-surround-double-quotes)
    (define-key evil-visual-state-map (kbd "goo") 'indent-region)
    (define-key evil-visual-state-map (kbd "gor") 'p-ex-evil-selection-replace)
    (define-key evil-visual-state-map (kbd "goa") 'p-ex-evil-selection-replace-yank)
    (define-key evil-visual-state-map (kbd "gcc") 'prot-comment)
    (define-key evil-visual-state-map (kbd "gcs") 'p-remove-extra-spaces)

    (define-key evil-insert-state-map (kbd "C-k") 'delete-backward-char)
    (define-key evil-insert-state-map (kbd "C-w") 'backward-kill-word)
    (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)

    (define-key evil-emacs-state-map (kbd "C-k") 'delete-backward-char)
    (define-key evil-emacs-state-map (kbd "C-w") 'backward-kill-word)
    (define-key evil-emacs-state-map (kbd "C-g") 'evil-normal-state)

    (define-key evil-ex-completion-map (kbd "C-k") 'delete-backward-char)
    (define-key evil-ex-completion-map (kbd "C-w") 'backward-kill-word)
    (define-key evil-ex-completion-map (kbd "C-f") 'forward-char)
    (define-key evil-ex-completion-map (kbd "C-b") 'backward-char)))

(provide 'init-evil)
