;;;;; init-note.el --- Note -*- lexical-binding: t -*-

;;; package
(straight-use-package 'markdown-mode)
(straight-use-package 'auctex)
(straight-use-package 'latex-preview-pane)
(straight-use-package 'evil-tex)
(straight-use-package '(usls :type git :host gitlab :repo "protesilaos/usls"))
(straight-use-package 'logos)

;;; markdown
(add-to-list 'auto-mode-alist '("\\.md\\.html\\'"))
(setq markdown-italic-underscore t)
(setq markdown-asymmetric-header t)
(setq markdown-fontify-code-blocks-natively t)

;;; latex
(add-to-list 'auto-mode-alist '("\\.tex\\'" . LaTeX-mode))
(add-hook 'LaTeX-mode-hook #'evil-tex-mode)

;;; usls
(setq usls-directory (expand-file-name "~/Dropbox/notes/"))
(setq usls-known-categories '("research" "work" "misc"))
(setq usls-file-type-extension ".txt")
(setq usls-subdir-support nil)
(setq usls-file-region-separator 'line)
(setq usls-file-region-separator-heading-level 1)
(setq usls-custom-header-function nil)

;;; logos
;; need outline otherwise outline-regexp is void variable
(require 'outline)
(autoload 'logos-focus-mode "logos")

;; https://gitlab.com/protesilaos/logos
(setq logos-outlines-are-pages t)

(setq logos-outline-regexp-alist
      '((emacs-lisp-mode . "^;;;+ ")
	(org-mode . "^\\*+ +")
	(text-mode . "^\\* [0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\},")
	(t . ,(or outline-regexp logos--page-delimiter))))

(setq-default logos-hide-mode-line nil
	      logos-scroll-lock nil
	      logos-variable-pitch nil
	      logos-indicate-buffer-boundaries nil
	      logos-buffer-read-only nil
	      logos-olivetti t)

(defun logos--reveal-entry ()
  (cond
   ((and (eq major-mode 'org-mode)
	 (org-at-heading-p))
    (org-show-subtree))
   ((or (eq major-mode 'outline-mode)
	(bound-and-true-p outline-minor-mode))
    (outline-show-subtree))))

(add-hook 'logos-page-motion-hook #'logos--reveal-entry)

;; place point at the top when changing pages
(defun my-logos--recenter-top ()
  (recenter 0))

(add-hook 'logos-page-motion-hook #'my-logos--recenter-top)

;;; auto fill
(dolist (hook '(markdown-mode-hook text-mode-hook))
  (add-hook hook 'p-text-mode-auto-fill))

;;; keybindings
(with-eval-after-load 'evil
  (general-create-definer p-space-leader-def
    :prefix "SPC"
    :states 'normal)
  (p-space-leader-def
    "n"  '(:ignore t :which-key "note")
    "nn" '(usls-new-note :which-key "usls new note")
    "nf" '(usls-find-file :which-key "usls find file")
    "nN" '(usls-append-region-buffer-or-file :which-key "usls append")
    "nd" '(usls-dired :which-key "usls dired")
    "ni" '(usls-id-insert :which-key "usls insert id")
    "nk" '(usls-follow-link :which-key "usls follow link")
    "nl"  '(:ignore t :which-key "logos")
    "nln" '(logos-narrow-dwim :which-key "narrow")
    "nlf" '(logos-forward-page-dwim :which-key "forward page")
    "nlb" '(logos-backward-page-dwim :which-key "backward page")
    "nll" '(logos-focus-mode :which-key "focus mode"))

  (general-create-definer p-latex-leader-def
    :prefix ";"
    :states '(normal visual)
    :keymaps 'LaTeX-mode-map)
  (p-latex-leader-def
   "p"  '(:ignore t :which-key "latex preview")
   "pp" '(latex-preview-pane-mode :which-key "toggle latex preview pane")))

(provide 'init-note)
;;;;; init-note.el ends here
