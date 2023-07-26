;;; init-keybindings.el --- Keybindings -*- lexical-binding: t -*-

(straight-use-package 'which-key)
(straight-use-package 'general)

(require 'prot-prefix)
(require 'prot-scratch)

;;; Which-key
(setq which-key-sort-order #'which-key-key-order-alpha
      which-key-sort-uppercase-first nil
      which-key-add-column-padding 1
      which-key-max-display-columns nil
      which-key-min-display-lines 6
      which-key-side-window-slot -10)
(which-key-mode)

;;; Emacs keybindings
(global-set-key (kbd "C-g") #'prot-simple-keyboard-quit-dwim)
(global-set-key (kbd "C-o") #'prot-prefix)
(global-set-key (kbd "C-x k") 'prot-simple-kill-buffer-current)
(global-set-key (kbd "C-x K") 'kill-buffer-and-window)
(global-set-key (kbd "M-i") 'forward-paragraph)
(global-set-key (kbd "M-o") 'backward-paragraph)
(global-set-key (kbd "C-h K") #'describe-keymap)
(global-set-key (kbd "M-s s") #'isearch-forward)
(global-set-key (kbd "M-s r") #'isearch-backward)
(global-set-key (kbd "M-s M-s") #'isearch-forward-regexp)
(global-set-key (kbd "M-s M-r") #'isearch-backward-regexp)
(global-set-key (kbd "M-s a") #'query-replace)
(global-set-key (kbd "M-s M-a") #'query-replace-regexp)
(global-set-key (kbd "M-s e") #'occur-edit-mode)

;;; General
(general-evil-setup)

;; Fix leader key in Message buffer
;; https://github.com/noctuid/general.el/issues/493
;; https://github.com/noctuid/general.el/issues/493#issuecomment-913168833
(general-add-hook
 'after-init-hook
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
  "1" '(winum-select-window-1 :which-key "select window 1")
  "2" '(winum-select-window-2 :which-key "select window 2")
  "3" '(winum-select-window-3 :which-key "select window 3")
  ;; File
  "f" '(:ignore t :wk "file")
  "ff" '(find-file :wk "find file")
  "fs" '(save-buffer :wk "save buffer")
  "fr" '(consult-recent-file :wk "recent file")
  "fp" '(p-find-file-in-config :wk "find config file")
  "fn" '(p-find-file-in-notes :wk "find note file")
  "fo" '(p-find-file-in-org :wk "find org file")
  "fR" '(prot-simple-rename-file-and-buffer :wk "rename file")
  "fj" '(p-create-scratch-file :wk "scratch file")
  ;; Buffer
  "b" '(:ignore t :wk "buffer")
  "bb" '(consult-buffer :wk "switch buffer")
  "bi" '(ibuffer :wk "ibuffer")
  "bm" '(p-switch-to-messages :wk "switch to message buffer")
  "bd" '(prot-simple-kill-buffer-current :wk "kill buffer")
  "bD" '(kill-buffer-and-window :wk "kill buffer and delete window")
  "br" '(revert-buffer :wk "reload buffer")
  "bh" '(mark-whole-buffer :wk "select whole buffer")
  "bn" '(prot-scratch-buffer :wk "create new scratch")
  ;; Windows
  "wd" '(delete-window :wk "delete window")
  "wo" '(delete-other-windows :wk "delete other windows")
  "wv" '(split-window-right :wk "split window right")
  "ws" '(split-window-below :wk "split window below")
  ;; Search
  "s"  '(:ignore t :wk "search")
  "ss" '(consult-line :wk "consult line")
  "sk" '(consult-yank-pop :wk "consult yank")
  "sp" '(consult-ripgrep :wk "consult-rg project")
  "sb" '(p-consult-line-at-point :wk "consult line at point")
  "sd" '(p-consult-ripgrep-at-point :wk "consult-rg at point")
  "sr" '(p-consult-ripgrep-dir :wk "consult-rg dir")
  "si" '(consult-imenu :wk "consult imenu")
  "sl" '(consult-outline :wk "consult outline")
  "sy" '(consult-yasnippet :wk "consult yasnippet")
  ;; Git and projects
  "g"  '(:ignore t :wk "git")
  "gr" '(vc-dir :wk "vc-dir")
  "gg" '(project-vc-dir :wk "project-vc-dir")
  "gm" '(git-messenger:popup-message :wk "git message")
  "gd" '(color-rg-search-input :wk "color-rg-search-input")
  "gD" '(color-rg-search-symbol :wk "color-rg-search-symbol")
  "gp" '(color-rg-search-input-in-project :wk "color-rg-search-input-in-project")
  "gP" '(color-rg-search-symbol-in-project :wk "color-rg-search-symbol-in-project")
  "gt" '(color-rg-search-project-with-type :wk "color-rg-search-project-with-type")
  "gT" '(color-rg-search-symbol-with-type :wk "color-rg-search-symbol-with-type")
  "p" '(:ignore t :wk "projects")
  "pP" '(project-switch-project :wk "switch project")
  "pp" '(p-project-switch-project :wk "dwim switch project")
  "pb" '(project-switch-to-buffer :wk "switch buffer in project")
  "pf" '(project-find-file :wk "project find file")
  "pr" '(straight-remove-unused-repos :wk "straight-remove-unused-repos")
  ;; Eval
  "r" '(:ignore t :wk "eval")
  "rb" '(eval-buffer :wk "eval buffer")
  ;; Toogle
  "t"  '(:ignore t :wk "toggle")
  "tr" '(p-reveal-file-in-finder :wk "reveal file in finder")
  "tw" '(count-words :wk "count words")
  "tl" '(count-lines-page :wk "count lines")
  "tm" '(toggle-frame-maximized :wk "maximize window")
  "tt" '(modus-themes-toggle :wk "toggle modus theme")
  "ta" '(app-launcher-run-app :wk "app-launcher-run-app")
  "td" '(dir-launcher-run-dir :wk "dir-launcher-run-dir")
  "tu" '(url-launcher-run-url :wk "url-launcher-run-url")
  "ty" '(yas-new-snippet :wk "create new snippet")
  "ti" '(yas-insert-snippet :wk "insert snippet")
  ;; Xah Lee
  "x"  '(:ignore t :wk "xah editing")
  "xd" '(xah-choose-and-insert-date :wk "choose and insert date")
  "xl" '(xah-delete-blank-lines :wk "deletle blank lies")
  "xn" '(xah-space-to-newline :wk "from space to newline")
  "xc" '(xah-cycle-hyphen-lowline-space :wk "cycle hyphen lowline")
  "xb" '(xah-new-empty-buffer :wk "new empty buffer")
  "xa" '(xah-add-space-after-comma :wk "add space after comma")
  ;; Quit
  "q" '(:ignore t :wk "quit")
  "qq" '(kill-emacs :wk "quit emacs")
  "qr" '(restart-emacs :wk "restart emacs"))

(general-create-definer p-jupyter-leader-def
  :prefix ";"
  :states '(normal visual)
  :keymaps '(python-mode-map))
(p-jupyter-leader-def
  "j"  '(:ignore t :wk "jupyter")
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
 "j"  '(:ignore t :wk "latex")
 "jm" '(TeX-insert-macro :wk "insert latex macro")
 "je" '(LaTeX-environment :wk "insert latex environment")
 "jF" '(LaTeX-fill-buffer :wk "format latex file")
 "jr" '(p-run-latex :wk "run tex")
 "ja" '(TeX-command-run-all :wk "run all")
 "jf" '(p-select-beamer-frame :wk "select beamer frame block")
 "jc" '(p-clear-latex-temp-files :wk "clear temp files")
 "jv" '(TeX-view :wk "view pdf"))

(general-imap "f"
  (general-key-dispatch 'self-insert-command
    :timeout 0.1
    "d" 'evil-normal-state))

(defun p-insert-pound () (interactive) (insert "£"))
(general-imap "y"
  (general-key-dispatch 'self-insert-command
    :timeout 0.2
    "b" 'p-insert-pound))

(define-key evil-normal-state-map (kbd "C-'") 'prot-simple-kill-line-backward)
(define-key evil-normal-state-map (kbd "C-i") 'p-delete-backward-to-tab)
(define-key evil-normal-state-map (kbd "C-f") 'prot-simple-multi-line-below)
(define-key evil-normal-state-map (kbd "C-b") 'prot-simple-multi-line-above)
(define-key evil-normal-state-map (kbd "m") 'evil-jump-item)
(define-key evil-normal-state-map (kbd "U") 'undo-redo)
(define-key evil-normal-state-map (kbd ";a") 'evil-first-non-blank)
(define-key evil-normal-state-map (kbd ";e") 'evil-last-non-blank)
(define-key evil-normal-state-map (kbd ";r") 'prot-simple-yank-replace-line-or-region)
(define-key evil-normal-state-map (kbd ";m") 'prot-simple-mark-construct-dwim)
(define-key evil-normal-state-map (kbd ";f") 'p-move-sexp-forward)
(define-key evil-normal-state-map (kbd ";b") 'p-move-sexp-backward)
(define-key evil-normal-state-map (kbd ",,") 'p-mark-paragraph)
(define-key evil-normal-state-map (kbd ",.") 'p-mark-defun)
(define-key evil-normal-state-map (kbd ",b") 'p-mark-paragraph-below)
(define-key evil-normal-state-map (kbd ",k") 'p-insert-surround-parentheses)
(define-key evil-normal-state-map (kbd ",a") 'beginning-of-defun)
(define-key evil-normal-state-map (kbd ",e") 'end-of-defun)
(define-key evil-normal-state-map (kbd ",f") 'p-insert-surround-bracket)
(define-key evil-normal-state-map (kbd ",h") 'p-insert-surround-curly-bracket)
(define-key evil-normal-state-map (kbd ",s") 'p-insert-surround-single-quote)
(define-key evil-normal-state-map (kbd ",d") 'p-insert-surround-double-quotes)
(define-key evil-normal-state-map (kbd "gcc") 'prot-comment-comment-dwim)
(define-key evil-normal-state-map (kbd "goo") 'indent-region)
(define-key evil-normal-state-map (kbd "goi") 'p-format-indent-in-buffer)
(define-key evil-normal-state-map (kbd "gor") 'p-ex-evil-buffer-replace)
(define-key evil-normal-state-map (kbd "goa") 'p-ex-evil-replace-yank)
(define-key evil-normal-state-map (kbd "gof") 'p-ex-evil-replace-yank-def)
(define-key evil-normal-state-map (kbd "gol") 'p-mark-line-non-blank)
(define-key evil-normal-state-map (kbd "gcd") 'kill-sexp)
(define-key evil-normal-state-map (kbd "gcl") 'p-clear-line)
(define-key evil-normal-state-map (kbd "gci") 'p-kill-sexp-and-insert)
(define-key evil-normal-state-map (kbd "gcs") 'p-remove-extra-spaces)
(define-key evil-normal-state-map (kbd "gc.") 'p-delete-between-commas)
(define-key evil-normal-state-map (kbd "gc,") 'p-select-between-commas)

(define-key evil-visual-state-map (kbd "C-g") 'evil-normal-state)
(define-key evil-visual-state-map (kbd ";a") 'evil-first-non-blank)
(define-key evil-visual-state-map (kbd ";e") 'evil-last-non-blank)
(define-key evil-visual-state-map (kbd ";r") 'prot-simple-yank-replace-line-or-region)
(define-key evil-visual-state-map (kbd ",,") 'p-mark-paragraph)
(define-key evil-visual-state-map (kbd ",b") 'p-mark-paragraph-below)
(define-key evil-visual-state-map (kbd ",k") 'p-insert-surround-parentheses)
(define-key evil-visual-state-map (kbd ",a") 'beginning-of-defun)
(define-key evil-visual-state-map (kbd ",e") 'end-of-defun)
(define-key evil-visual-state-map (kbd ",f") 'p-insert-surround-bracket)
(define-key evil-visual-state-map (kbd ",h") 'p-insert-surround-curly-bracket)
(define-key evil-visual-state-map (kbd ",s") 'p-insert-surround-single-quote)
(define-key evil-visual-state-map (kbd ",d") 'p-insert-surround-double-quotes)
(define-key evil-visual-state-map (kbd "gcc") 'prot-comment-comment-dwim)
(define-key evil-visual-state-map (kbd "goo") 'indent-region)
(define-key evil-visual-state-map (kbd "gor") 'p-ex-evil-selection-replace)
(define-key evil-visual-state-map (kbd "goa") 'p-ex-evil-selection-replace-yank)
(define-key evil-visual-state-map (kbd "gcs") 'p-remove-extra-spaces)

(define-key evil-insert-state-map (kbd "C-a") 'evil-first-non-blank)
(define-key evil-insert-state-map (kbd "C-e") 'evil-last-non-blank)
(define-key evil-insert-state-map (kbd "C-i") 'p-delete-backward-to-tab)
(define-key evil-insert-state-map (kbd "C-k") 'delete-backward-char)
(define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
(define-key evil-insert-state-map (kbd "C-'") 'prot-simple-kill-line-backward)

(define-key evil-ex-completion-map (kbd "C-k") 'delete-backward-char)
(define-key evil-ex-completion-map (kbd "C-f") 'forward-char)
(define-key evil-ex-completion-map (kbd "C-b") 'backward-char)

(provide 'init-keybindings)
;;; init-keybindings.el ends here
