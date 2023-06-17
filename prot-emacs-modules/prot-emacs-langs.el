;;; General language/editing settings

;;; Install package
(straight-use-package 'markdown-mode)

(require 'prot-comment)

;;;; Tabs, indentation, and the TAB key
(setq-default tab-always-indent 'complete
              tab-first-completion 'word-or-paren-or-punct ; Emacs 27
              tab-width 4
              indent-tabs-mode nil)

;;;; Configure 'electric' behaviour
(setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)
(setq electric-pair-preserve-balance t)
(setq electric-pair-pairs
      '((8216 . 8217)
        (8220 . 8221)
        (171 . 187)))
(setq electric-pair-skip-self 'electric-pair-default-skip-self)
(setq electric-pair-skip-whitespace nil)
(setq electric-pair-skip-whitespace-chars '(9 10 32))
(setq electric-quote-context-sensitive t)
(setq electric-quote-paragraph t)
(setq electric-quote-string nil)
(setq electric-quote-replace-double t)
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

;;;; Shell scripts (sh-mode)
(add-to-list 'auto-mode-alist '("PKGBUILD" . sh-mode))

;;;; Eldoc (elisp live documentation feedback)
(global-eldoc-mode 1)

;;; Markdown (markdown-mode)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(setq markdown-enable-math t)
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
