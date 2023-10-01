;;; Isearch, occur, grep, and extras (prot-search.el)
(setq search-highlight t)
(setq search-whitespace-regexp ".*?" ; one `setq' here to make it obvious they are a bundle
      isearch-lax-whitespace t
      isearch-regexp-lax-whitespace nil)
(setq isearch-lazy-highlight t)
(setq list-matching-lines-jump-to-current-line nil) ; for `occur'
;; All of the following variables were introduced in Emacs 27.1.
(setq isearch-lazy-count t)
(setq lazy-count-prefix-format "(%s/%s) ")
(setq lazy-count-suffix-format nil)
(setq isearch-yank-on-move 'shift)
(setq isearch-allow-scroll 'unlimited)
;; These variables are from Emacs 28
(setq isearch-repeat-on-direction-change t)
(setq lazy-highlight-initial-delay 0.5)
(setq lazy-highlight-no-delay-length 3)
(setq isearch-wrap-pause t) ; `no-ding' makes keyboard macros never quit

(add-hook 'occur-mode-hook #'hl-line-mode)
(add-hook 'occur-mode-hook #'prot-common-truncate-lines-silently) ; from `prot-common.el'

(define-key minibuffer-local-isearch-map (kbd "M-/") #'isearch-complete-edit)
(define-key occur-mode-map (kbd "t") #'toggle-truncate-lines)
(prot-emacs-keybind isearch-mode-map
  "C-g" isearch-cancel ; instead of `isearch-abort'
  "M-/" isearch-complete)

(require 'prot-search)

(setq prot-search-outline-regexp-alist
      '((emacs-lisp-mode . "^\\((\\|;;;+ \\)")
        (org-mode . "^\\(\\*+ +\\|#\\+[Tt][Ii][Tt][Ll][Ee]:\\)")
        (outline-mode . "^\\*+ +")
        (emacs-news-view-mode . "^\\*+ +")
        (conf-toml-mode . "^\\[")
        (markdown-mode . "^#+ +")))
(setq prot-search-todo-keywords
      (concat "TODO\\|FIXME\\|NOTE\\|REVIEW\\|XXX\\|KLUDGE"
              "\\|HACK\\|WARN\\|WARNING\\|DEPRECATED\\|BUG"))

(prot-emacs-keybind global-map
  "M-s M-%" prot-search-replace-markup ; see `prot-search-markup-replacements'
  "M-s M-<" prot-search-isearch-beginning-of-buffer
  "M-s M->" prot-search-isearch-end-of-buffer
  "M-s g" prot-search-grep
  "M-s u" prot-search-occur-urls
  "M-s t" prot-search-occur-todo-keywords
  "M-s M-t" prot-search-grep-todo-keywords ; With C-u it runs `prot-search-git-grep-todo-keywords'
  "M-s M-o" prot-search-occur-outline
  "M-s M-u" prot-search-occur-browse-url)
(prot-emacs-keybind isearch-mode-map
  "<up>" prot-search-isearch-repeat-backward
  "<down>" prot-search-isearch-repeat-forward
  "<backspace>" prot-search-isearch-abort-dwim
  "<C-return>" prot-search-isearch-other-end)

;;; `grep' package
(setq grep-program (or (executable-find "rg") "grep"))
(setq grep-save-buffers nil)
(setq grep-use-headings t) ; Emacs 30
;;; `re-builder' package
(setq reb-re-syntax 'read)
;;; `xref' package
;; All those have been changed for Emacs 28
(setq xref-show-definitions-function #'xref-show-definitions-completing-read) ; for M-.
(setq xref-show-xrefs-function #'xref-show-definitions-buffer) ; for grep and the like
(setq xref-file-name-display 'project-relative)
(setq xref-search-program (if (string-match-p "rg" grep-program) 'ripgrep 'grep))

;;; wgrep (writable grep)
(straight-use-package 'wgrep)

(setq wgrep-auto-save-buffer t)
(setq wgrep-change-readonly-file t)
(prot-emacs-keybind grep-mode-map
  "e" wgrep-change-to-wgrep-mode
  "C-x C-q" wgrep-change-to-wgrep-mode
  "C-c C-c" wgrep-finish-edit)

(provide 'prot-emacs-search)
