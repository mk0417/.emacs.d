;; WORK IN PROGRESS

(straight-use-package 'evil)

;; Evaluate these before loading `evil'
(setq evil-want-C-i-jump t)
(setq evil-want-C-u-scroll t) ; Vim style
(setq evil-want-C-d-scroll t)
(setq evil-want-C-u-delete nil) ; I can use Emacs keys in insert mode
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
(setq evil-respect-visual-line-mode nil)

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
(setq evil-show-paren-range 0)
(setq evil-ex-hl-update-delay lazy-highlight-initial-delay)
(setq evil-highlight-closing-paren-at-point-states '(not emacs insert replace))
(setq evil-kill-on-visual-paste nil) ; Emacs style, not Vim
(setq evil-echo-state nil) ; be silent
(setq evil-complete-all-buffers t) ; TODO C-n and C-p in insert mode
(setq evil-lookup-func #'man) ; TODO K in normal mode
(setq evil-toggle-key "C-]") ; TODO
(setq evil-default-state 'normal) ; check `evil-set-initial-state'
;; evil-buffer-regexps
;; evil-emacs-state-modes
(setq evil-insert-state-modes '(comint-mode
                                erc-mode
                                rcirc-mode
                                eshell-mode
                                inferior-emacs-lisp-mode
                                reb-mode
                                shell-mode
                                term-mode
                                wdired-mode))
;; (setq evil-motion-state-modes '(apropos-mode ; FIXME 2023-09-29: What is the motion state?
;;                                  Buffer-menu-mode
;;                                  calendar-mode
;;                                  org-agenda-mode
;;                                  command-history-mode
;;                                  compilation-mode
;;                                  dictionary-mode
;;                                  ert-results-mode
;;                                  help-mode
;;                                  Info-mode
;;                                  Man-mode
;;                                  vundo-mode
;;                                  woman-mode))
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

(evil-mode 1)

(straight-use-package 'evil-collection)

(evil-collection-init)

(provide 'prot-emacs-evil)
