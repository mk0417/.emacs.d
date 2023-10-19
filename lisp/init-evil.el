;;; init-evil.el --- Evil -*- lexical-binding: t -*-

;;; This is modified based on `prot-emacs-evil.el' that has been removed by Prot

(prot-emacs-configure
  ;; Evaluate these before loading `evil'
  (setq evil-want-C-i-jump t)
  (setq evil-want-C-u-scroll t) ; Vim style
  (setq evil-want-C-d-scroll t)
  (setq evil-want-C-u-delete nil) ; I can use Emacs keys in insert mode
  (setq evil-want-C-w-delete t)
  (setq evil-want-C-h-delete nil) ; I can use Emacs keys in insert mode
  (setq evil-disable-insert-state-bindings t)
  (setq evil-want-C-g-bindings nil)
  (setq evil-want-C-w-in-emacs-state nil)
  (setq evil-want-change-word-to-end t)
  (setq evil-want-Y-yank-to-eol t) ; consistent with D
  (setq evil-want-abbrev-expand-on-insert-exit nil) ; expand abbrevs outright
  (setq evil-want-empty-ex-last-command t)
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-minibuffer nil)
  (setq evil-toggle-key "C-z") ; TODO
  (setq evil-respect-visual-line-mode nil)

  (prot-emacs-package evil
    (:install t)
    (setq evil-search-module 'isearch)
    (setq evil-symbol-word-search t)
    (setq evil-magic nil) ; not applicable for `evil-search-module' isearch module
    (setq evil-ex-search-vim-style-regexp nil) ; not for isearch
    (setq evil-shift-width tab-width)
    (setq evil-default-cursor t)
    (setq evil-start-of-line nil)
    (setq evil-repeat-move-cursor t)
    (setq evil-cross-lines nil)
    (setq evil-backspace-join-lines t)
    (setq evil-move-cursor-back t)
    (setq evil-move-beyond-eol t) ; Emacs style, not Vim
    (setq evil-repeat-find-to-skip-next t)
    (setq evil-kbd-macro-suppress-motion-error nil) ; never suppress errors in kmacros, Emacs-style
    (setq evil-track-eol nil)
    (setq evil-mode-line-format '(before . prot-modeline-buffer-identification))
    (setq evil-mouse-word 'evil-word)
    (setq evil-bigword "^ \t\r\n")
    (setq evil-want-fine-undo nil)
    (setq evil-regexp-search nil)
    (setq evil-search-wrap t)
    (setq evil-search-wrap-ring-bell t) ; TODO does this quit keyboard macros, specifically with C-u 0?
    (setq evil-flash-delay 0.5)
    (setq evil-auto-balance-windows window-combination-resize)
    (setq evil-split-window-below nil)
    (setq evil-vsplit-window-right nil)
    (setq evil-esc-delay 0.01)
    (setq evil-intercept-esc 'always)
    (setq evil-show-paren-range 1)
    (setq evil-ex-hl-update-delay lazy-highlight-initial-delay)
    (setq evil-highlight-closing-paren-at-point-states '(not emacs insert replace))
    (setq evil-kill-on-visual-paste nil) ; Emacs style, not Vim
    (setq evil-echo-state nil) ; be silent
    (setq evil-complete-all-buffers t) ; TODO C-n and C-p in insert mode
    (setq evil-lookup-func #'man) ; TODO K in normal mode
    (setq evil-default-state 'normal) ; check `evil-set-initial-state'
    ;; evil-buffer-regexps
    (setq evil-motion-state-modes nil)
    (setq evil-insert-state-modes nil)
    (setq evil-overriding-maps nil)

    ;; evil-intercept-maps
    ;; evil-motions
    ;; evil-visual-newline-commands
    (setq evil-v$-excludes-newline nil)
    (setq evil-text-object-change-visual-type t)
    ;; evil-ex-complete-emacs-commands 'in-turn
    ;; evil-ex-visual-char-range nil
    (setq evil-ex-interactive-search-highlight 'selected-window)
    (setq evil-ex-search-persistent-highlight t)
    (setq evil-ex-search-case 'smart)
    (setq evil-ex-substitute-case nil) ; use `evil-ex-search-case'
    (setq evil-ex-search-interactive t)
    (setq evil-ex-search-incremental t)
    (setq evil-ex-search-highlight-all t)
    (setq evil-ex-substitute-highlight-all t)
    (setq evil-ex-substitute-interactive-replace t)
    (setq evil-ex-substitute-global nil) ; global substitute is with s/a/b/g as it should
    (setq evil-command-window-height 7)  ; TODO
    (setq evil-display-shell-error-in-message nil) ; TODO
    (setq evil-undo-system 'undo-redo) ; Emacs 28
    (setq evil-visual-update-x-selection-p t)

    (evil-mode 1))

  (prot-emacs-package evil-collection
    (:install t)
    (evil-collection-init))

  (prot-emacs-package evil-surround
    (:install t)
    (global-evil-surround-mode 1))

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

  (define-key evil-normal-state-map (kbd "f") nil)
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

  (define-key evil-ex-completion-map (kbd "C-k") 'delete-backward-char)
  (define-key evil-ex-completion-map (kbd "C-w") 'backward-kill-word)
  (define-key evil-ex-completion-map (kbd "C-f") 'forward-char)
  (define-key evil-ex-completion-map (kbd "C-b") 'backward-char))

(provide 'init-evil)
