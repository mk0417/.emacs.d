;;; General language/editing settings

;;; Install package
(straight-use-package 'markdown-mode)

(require 'prot-comment)

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

;;;; SystemD files (conf-mode)
(add-to-list 'auto-mode-alist '("\\.\\(service\\|timer\\)\\'" . conf-mode))

;;;; Eldoc (elisp live documentation feedback)
(setq eldoc-message-function #'message) ; don't use mode line for M-x eval-expression, etc.
(global-eldoc-mode 1)

;;; Markdown (markdown-mode)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(setq markdown-fontify-code-blocks-natively t)

;;; Comments (newcomment.el and prot-comment.el)
(setq comment-empty-lines t)
(setq comment-fill-column nil)
(setq comment-multi-line t)
(setq comment-style 'multi-line)
(prot-emacs-keybind global-map
  "C-:" #'comment-kill ; C-S-;
  "M-;" #'comment-indent)

(setq prot-comment-comment-keywords
      '("TODO" "NOTE" "XXX" "REVIEW" "FIXME"))
(setq prot-comment-timestamp-format-concise "%F")
(setq prot-comment-timestamp-format-verbose "%F %T %z")
(prot-emacs-keybind global-map
  "C-;" #'prot-comment-comment-dwim
  "C-x C-;" #'prot-comment-timestamp-keyword)

(provide 'prot-emacs-langs)
