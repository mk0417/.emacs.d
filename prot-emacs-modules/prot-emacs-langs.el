;;; General language/editing settings

(straight-use-package 'markdown-mode)

(require 'flyspell)
(require 'prot-spell)

;;;; Tabs, indentation, and the TAB key
(setq-default tab-always-indent 'complete
              tab-first-completion 'word-or-paren-or-punct ; Emacs 27
              tab-width 4
              indent-tabs-mode nil)

;;;; Disable "electric" behaviour
(electric-pair-mode -1)
(electric-quote-mode -1)
;; I don't like auto indents in Org and related.  They are okay for
;; programming.
(electric-indent-mode -1)
(add-hook 'prog-mode-hook #'electric-indent-local-mode)

;;;; Parentheses (show-paren-mode)
(setq show-paren-style 'parenthesis)
(setq show-paren-when-point-in-periphery nil)
(setq show-paren-when-point-inside-paren nil)
(setq show-paren-context-when-offscreen 'overlay) ; Emacs 29
(add-hook 'after-init-hook #'show-paren-mode)

;;;; Emacs Lisp (emacs-lisp-mode)
(prot-emacs-keybind emacs-lisp-mode-map
  "C-x e" edebug-defun ; override `kmacro-end-and-call-macro'
  "C-x E" edebug-remove-instrumentation)

;;;; Plain text (text-mode)
(setq sentence-end-double-space t)
(setq sentence-end-without-period nil)
(setq colon-double-space nil)
(setq use-hard-newlines nil)
(setq adaptive-fill-mode t)

(add-hook 'text-mode-hook #'turn-on-auto-fill)

(add-to-list 'auto-mode-alist '("\\(README\\|CHANGELOG\\|COPYING\\|LICENSE\\)\\'" . text-mode))

;;;; Arch Linux and AUR package scripts (sh-mode)
(add-to-list 'auto-mode-alist '("PKGBUILD" . sh-mode))

;;;; SystemD and other configuration files (conf-mode)
(add-to-list 'auto-mode-alist '("\\.\\(service\\|timer\\)\\'" . conf-mode))
(add-to-list 'auto-mode-alist '("dircolors" . conf-mode))

;;;; Eldoc (elisp live documentation feedback)
(setq eldoc-message-function #'message) ; don't use mode line for M-x eval-expression, etc.
(global-eldoc-mode 1)

;;;; Eglot (built-in client for the language server protocol)
(setq eglot-sync-connect nil)
(setq eglot-autoshutdown t)

;;;; Handle performance for very long lines (so-long.el)
(global-so-long-mode 1)

;;; Markdown (markdown-mode)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(setq markdown-fontify-code-blocks-natively t)

;;; Flyspell and prot-spell.el (spell check)
;; See FIXME for `jinx'.
(setq flyspell-issue-message-flag nil)
(setq flyspell-issue-welcome-flag nil)
(setq ispell-program-name "aspell")
(setq ispell-dictionary "en_GB")
(define-key flyspell-mode-map (kbd "C-;") nil)
(define-key ctl-x-x-map "s" flyspell-mode) ; C-x x s

(setq prot-spell-dictionaries
      '(("EN English" . "en")
        ("EL Ελληνικά" . "el")
        ("FR Français" . "fr")
        ("ES Espanõl" . "es")))

(setq ispell-choices-buffer "*ispell-top-choices*") ; see my `display-buffer-alist'

;; Also check prot-spell.el for what I am doing with
;; `prot-spell-ispell-display-buffer'.  Then refer to the
;; `display-buffer-alist' for the relevant entry.

(prot-emacs-keybind global-map
  "M-$" prot-spell-spell-dwim
  "C-M-$" prot-spell-change-dictionary)

(provide 'prot-emacs-langs)
