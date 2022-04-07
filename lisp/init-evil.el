;;;;; init-evil.el --- Evil-mode/Vim -*- lexical-binding: t -*-

;;; package
(straight-use-package 'evil)
(straight-use-package 'general)
(straight-use-package 'evil-surround)
(straight-use-package 'evil-escape)
(straight-use-package 'evil-matchit)
(straight-use-package 'evil-goggles)
(straight-use-package 'winum)

;;; evil
(setq evil-undo-system 'undo-redo)
(setq evil-symbol-word-search t)
(setq evil-respect-visual-line-mode t)
(setq evil-want-C-u-scroll t)
(add-hook 'after-init-hook 'evil-mode)

;;; change cursor type and color
;; https://github.com/hlissner/doom-emacs/issues/1848
(setq evil-normal-state-cursor '(box "#cf5a65"))
(setq evil-insert-state-cursor '(hbar "#00ff00"))
(setq evil-visual-state-cursor '(hollow "#cf5a65"))

;;; evil surround
(global-evil-surround-mode 1)

;;; evil escape
(setq-default evil-escape-key-sequence "fd")
(evil-escape-mode 1)
(diminish 'evil-escape-mode)

;;; evil-matchit
(setq evilmi-shortcut "m")
(global-evil-matchit-mode 1)

;;; evil goggles
(setq evil-goggles-pulse t)
(setq evil-goggles-duration 0.35)
(evil-goggles-mode 1)
(custom-set-faces
 '(evil-goggles-paste-face ((t (:background "#cf5a65"))))
 '(evil-goggles-delete-face ((t (:background "#cf5a65"))))
 '(evil-goggles-change-face ((t (:background "#cf5a65"))))
 '(evil-goggles-yank-face ((t (:background "#cf5a65")))))
(diminish 'evil-goggles-mode)

;;; winum
(add-hook 'after-init-hook 'winum-mode)

;;; set evil normal state for grep mode
(dolist (mode '(grep-mode occur-mode occur-edit-mode))
  (evil-set-initial-state mode 'normal))

(evil-set-initial-state 'dired-mode 'emacs)

;;; keybindings
(with-eval-after-load 'evil
  ;; ex-evil replace buffer
  (defun p-ex-evil-buffer-replace ()
    (interactive)
    (evil-ex (concat "%s/")))

  ;; ex-evil replace in selected region
  (defun p-ex-evil-selection-replace ()
    (interactive)
    (evil-ex (concat "'<,'>s/")))

  ;; select functions
  (defun p-select-function ()
    (interactive)
	(forward-char)
    (beginning-of-defun)
    (evilmi-select-items))

  (defun p-switch-to-scratch ()
    (interactive)
    (switch-to-buffer "*scratch*"))

  (defun p-switch-to-messages ()
    (interactive)
    (switch-to-buffer "*Messages*"))

  (defun p-switch-to-previous-buffer ()
    (interactive)
    (switch-to-buffer nil))

  ;; I prefer to use C-n and C-p in many other places
  (define-key evil-normal-state-map (kbd "C-n") nil)
  (define-key evil-normal-state-map (kbd "C-p") nil)
  (define-key evil-insert-state-map (kbd "C-n") nil)
  (define-key evil-insert-state-map (kbd "C-p") nil)

  (define-key evil-normal-state-map (kbd ",.") 'p-select-function)
  (define-key evil-normal-state-map (kbd "gl") 'evil-shift-right)
  (define-key evil-normal-state-map (kbd "gh") 'evil-shift-left)
  (define-key evil-normal-state-map (kbd "gcc") 'comment-line)
  (define-key evil-normal-state-map (kbd "gor") 'p-ex-evil-buffer-replace)
  (define-key evil-normal-state-map (kbd "gos") 'transpose-sexps)
  (define-key evil-normal-state-map (kbd ",a") 'beginning-of-defun)
  (define-key evil-normal-state-map (kbd ",e") 'end-of-defun)

  (define-key evil-visual-state-map (kbd "gcc") 'comment-line)
  (define-key evil-visual-state-map (kbd "gor") 'p-ex-evil-selection-replace)
  (define-key evil-visual-state-map (kbd ",a") 'beginning-of-defun)
  (define-key evil-visual-state-map (kbd ",e") 'end-of-defun)

  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-a") 'evil-beginning-of-line)
  (define-key evil-insert-state-map (kbd "C-e") 'end-of-line)
  (define-key evil-insert-state-map (kbd "C-k") 'delete-backward-char)

  (define-key evil-ex-completion-map (kbd "C-f") 'forward-char)
  (define-key evil-ex-completion-map (kbd "C-b") 'backward-char)
  (define-key evil-ex-completion-map (kbd "C-k") 'delete-backward-char)

  (define-key evil-inner-text-objects-map "f" 'evil-inner-bracket)
  (define-key evil-inner-text-objects-map "h" 'evil-inner-curly)
  (define-key evil-inner-text-objects-map "d" 'evil-inner-double-quote)
  (define-key evil-outer-text-objects-map "f" 'evil-a-bracket)
  (define-key evil-outer-text-objects-map "h" 'evil-a-curly)
  (define-key evil-outer-text-objects-map "d" 'evil-a-double-quote)

  (general-create-definer p-space-leader-def
    :prefix "SPC"
    :states '(normal visual))
  (p-space-leader-def
    "SPC" 'execute-extended-command
    "1" '(winum-select-window-1 :which-key "select window 1")
    "2" '(winum-select-window-2 :which-key "select window 2")
    "3" '(winum-select-window-3 :which-key "select window 3")
    "f" '(:ignore t :which-key "file")
    "ff" '(find-file :which-key "find file")
    "fs" '(save-buffer :which-key "save buffer")
    "fo" '(find-file-other-window :which-key "open file in another window")
    "fc" '(copy-file :which-key "copy file")
    "b" '(:ignore t :which-key "buffer")
    "bd" '(kill-this-buffer :which-key "kill buffer")
    "bi" '(ibuffer :which-key "ibuffer")
    "bD" '(kill-buffer-and-window :which-key "kill buffer and window")
    "br" '(revert-buffer :which-key "revert buffer")
    "bs" '(p-switch-to-scratch :which-key "switch to scratch")
    "bm" '(p-switch-to-messages :which-key "switch to messages")
    "§" '(p-switch-to-previous-buffer :which-key "switch to previous buffer")
    "`" '(p-switch-to-previous-buffer :which-key "switch to previous buffer")
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
    "e" '(:ignore t :which-key "editing")
    "ec" '(whitespace-cleanup :which-key "clear whitespace")
    "w" '(:ignore t :which-key "window")
    "wd" '(delete-window :which-key "delete window")
    "wv" '(evil-window-vsplit :which-key "split window right")
    "ws" '(evil-window-split :which-key "split window below")
    "wo" '(delete-other-windows :which-key "delete other windows")
    "p" '(:ignore t :which-key "projects and packages")
    "ps" '(straight-pull-package-and-deps :which-key "straight-pull-package-and-deps")
    "pr" '(straight-remove-unused-repos :which-key "straight-remove-unused-repos")
    "pU" '(straight-pull-recipe-repositories :which-key "straight-pull-recipe-repositories")
    "pu" '(straight-pull-all  :which-key "straight update all packages")
    "t" '(:ignore t :which-key "toggle")
    "tf" '(p-set-regular-font :which-key "set regular font")
    "tF" '(p-set-large-font :which-key "set large font")
    "te" '(p-set-extra-large-font :which-key "set extra large font")
    "tM" '(toggle-frame-fullscreen :which-key "fullscreen")
    "tp" '(variable-pitch-mode :which-key "pitch font mode")
    "tw" '(count-words :which-key "count words")
    "tl" '(count-lines-page :which-key "count lines")
    "tm" '(toggle-frame-maximized :which-key "maximize window")
    "tt" '(my-modus-themes-toggle :which-key "toggle modus theme")
    "q" '(:ignore t :which-key "quit")
    "qq" '(kill-emacs :which-key "quit emacs")))

(provide 'init-evil)
;;;;; init-evil.el ends here
