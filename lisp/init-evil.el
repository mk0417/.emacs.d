;;;;; init-evil.el --- Evil mode configuration -*- lexical-binding: t -*-

;;; Install packages
(straight-use-package 'evil)
(straight-use-package 'general)
(straight-use-package 'evil-nerd-commenter)
(straight-use-package 'evil-surround)
(straight-use-package 'evil-escape)
(straight-use-package 'evil-matchit)
(straight-use-package 'evil-goggles)
(straight-use-package 'evil-exchange)

;;; Set some variables that must be configured before loading the package
(setq evil-want-C-i-jump nil)
(setq evil-want-C-u-scroll t)
(setq evil-respect-visual-line-mode t)
(setq evil-undo-system 'undo-redo)

;;; Load Evil and enable it globally
(require 'evil)
(evil-mode 1)

;;; Change cursor type and color
;; https://github.com/hlissner/doom-emacs/issues/1848
(setq evil-normal-state-cursor '(box "#cf5a65"))
(setq evil-insert-state-cursor '(hbar "#cf5a65"))
(setq evil-visual-state-cursor '(hollow "#cf5a65"))
;; curosr in minibuffer
(add-hook 'minibuffer-setup-hook
          (lambda ()
            (setq-local cursor-type 'hbar)
            (set-face-attribute 'cursor nil :background "#cf5a65")))

;;; Make evil search more like vim
(evil-select-search-module 'evil-search-module 'evil-search)

;;; Evil surround
(global-evil-surround-mode 1)

;;; Evil escape
(setq-default evil-escape-key-sequence "fd")
(evil-escape-mode 1)
(diminish 'evil-escape-mode)

;;; Evil-matchit
(setq evilmi-shortcut "m")
(global-evil-matchit-mode 1)

;;; Evil goggles
(setq evil-goggles-pulse t)
(setq evil-goggles-duration 0.35)
(evil-goggles-mode 1)
(custom-set-faces
 '(evil-goggles-paste-face ((t (:background "#cf5a65"))))
 '(evil-goggles-delete-face ((t (:background "#cf5a65"))))
 '(evil-goggles-change-face ((t (:background "#cf5a65"))))
 '(evil-goggles-yank-face ((t (:background "#cf5a65")))))
(diminish 'evil-goggles-mode)

;;; Turn on evil nerd commenter
(evilnc-default-hotkeys t)

;;; Evil-exchange
(setq evil-exchange-key (kbd "gs"))
(evil-exchange-install)

;;; Make C-g revert to normal state
(define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)

;;; Rebind `universal-argument' to `M-s-u' since `C-u' now scrolls the buffer
(global-set-key (kbd "M-s-u") 'universal-argument)

;;; Use visual line motions even outside of visual-line-mode buffers
(evil-global-set-key 'motion "j" 'evil-next-visual-line)
(evil-global-set-key 'motion "k" 'evil-previous-visual-line)

;;; Make sure some modes start in Emacs state
(dolist (mode '(custom-mode eshell-mode term-mode xref--xref-buffer-mode lsp-bridge-ref-mode))
  (add-to-list 'evil-emacs-state-modes mode))

;;; Set evil normal state for grep mode
(dolist (mode '(grep-mode occur-mode occur-edit-mode dired-mode-map))
  (evil-set-initial-state mode 'normal))

;;; Keybindings
(with-eval-after-load 'evil
  ;; I prefer to use C-n and C-p in many other places
  (define-key evil-normal-state-map (kbd "C-n") nil)
  (define-key evil-normal-state-map (kbd "C-p") nil)
  (define-key evil-insert-state-map (kbd "C-n") nil)
  (define-key evil-insert-state-map (kbd "C-p") nil)

  (define-key evil-inner-text-objects-map "k" 'evil-inner-paren)
  (define-key evil-inner-text-objects-map "f" 'evil-inner-bracket)
  (define-key evil-inner-text-objects-map "h" 'evil-inner-curly)
  (define-key evil-inner-text-objects-map "d" 'evil-inner-double-quote)
  (define-key evil-inner-text-objects-map "s" 'evil-inner-single-quote)
  (define-key evil-outer-text-objects-map "k" 'evil-a-paren)
  (define-key evil-outer-text-objects-map "f" 'evil-a-bracket)
  (define-key evil-outer-text-objects-map "h" 'evil-a-curly)
  (define-key evil-outer-text-objects-map "d" 'evil-a-double-quote)
  (define-key evil-outer-text-objects-map "s" 'evil-a-single-quote)

  (define-key evil-normal-state-map (kbd "gcd") 'kill-sexp)
  (define-key evil-normal-state-map (kbd "gos") 'transpose-sexps)
  (define-key evil-normal-state-map (kbd ",a") 'beginning-of-defun)
  (define-key evil-normal-state-map (kbd ",e") 'end-of-defun)
  (define-key evil-normal-state-map (kbd "goo") 'evil-indent-line)
  (define-key evil-normal-state-map (kbd "gcc") 'evilnc-comment-or-uncomment-lines)
  (define-key evil-normal-state-map (kbd "gog") 'p-cycle-line-beginning-end)

  (define-key evil-visual-state-map (kbd ",a") 'beginning-of-defun)
  (define-key evil-visual-state-map (kbd ",e") 'end-of-defun)
  (define-key evil-visual-state-map (kbd "goo") 'evil-indent)
  (define-key evil-visual-state-map (kbd "gcc") 'evilnc-comment-or-uncomment-lines)

  (define-key evil-insert-state-map (kbd "C-a") 'evil-beginning-of-line)
  (define-key evil-insert-state-map (kbd "C-e") 'end-of-line)
  (define-key evil-insert-state-map (kbd "C-k") 'delete-backward-char)

  (define-key evil-ex-completion-map (kbd "C-f") 'forward-char)
  (define-key evil-ex-completion-map (kbd "C-b") 'backward-char)
  (define-key evil-ex-completion-map (kbd "C-k") 'delete-backward-char)

  (general-create-definer p-space-leader-def
    :prefix "SPC"
    :states '(normal visual))
  (p-space-leader-def
    "SPC" 'execute-extended-command
    "f" '(:ignore t :which-key "file")
    "ff" '(find-file :which-key "find file")
    "fs" '(save-buffer :which-key "save buffer")
    "fo" '(find-file-other-window :which-key "open file in another window")
    "fc" '(copy-file :which-key "copy file")
    "fw" '(write-file :which-key "save file as")
    "b" '(:ignore t :which-key "buffer")
    "bw" '(mark-whole-buffer :which-key "Select whole buffer")
    "bd" '(kill-this-buffer :which-key "kill buffer")
    "bi" '(ibuffer :which-key "ibuffer")
    "bD" '(kill-buffer-and-window :which-key "kill buffer and window")
    "br" '(revert-buffer :which-key "revert buffer")
    "bs" '(scratch-buffer :which-key "switch to scratch")
    "d" '(:ignore t :which-key "dired")
    "dd" '(dired :which-key "dired directory")
    "dj" '(dired-jump :which-key "dired jump")
    "r" '(:ignore t :which-key "eval")
    "rb" '(eval-buffer :which-key "eval buffer")
    "rr" '(eval-region :which-key "eval region")
    "rf" '(eval-defun :which-key "eval function")
    "re" '(eval-expression :which-key "eval expression")
    "rl" '(eval-last-sexp :which-key "eval last sexp")
    "rs" '(shell-command :which-key "shell command")
    "t" '(:ignore t :which-key "toggle")
    "tf" '(fontaine-set-preset :which-key "set font preset")
    "tF" '(fontaine-set-face-font :which-key "set font face")
    "tp" '(variable-pitch-mode :which-key "pitch font mode")
    "tw" '(count-words :which-key "count words")
    "tl" '(count-lines-page :which-key "count lines")
    "tm" '(toggle-frame-maximized :which-key "maximize window")
    "tt" '(modus-themes-toggle :which-key "toggle modus theme")
    "q" '(:ignore t :which-key "quit")
    "qq" '(kill-emacs :which-key "quit emacs")
    "qr" '(restart-emacs :which-key "restart emacs")))

(provide 'init-evil)
;;;;; init-evil.el ends here
