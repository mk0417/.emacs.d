;;; init-note.el --- Note -*- lexical-binding: t -*-

;; package
(straight-use-package 'markdown-mode)
(straight-use-package 'auctex)
(straight-use-package 'latex-preview-pane)
(straight-use-package 'evil-tex)
(straight-use-package '(usls :type git :host gitlab :repo "protesilaos/usls"))

;; markdown
(add-to-list 'auto-mode-alist '("\\.md\\.html\\'"))
(setq markdown-italic-underscore t)
(setq markdown-asymmetric-header t)
(setq markdown-fontify-code-blocks-natively t)

;; latex
(add-hook 'LaTeX-mode-hook #'evil-tex-mode)

;; usls
(setq usls-directory (expand-file-name "~/Dropbox/notes/"))
(setq usls-known-categories '("research" "work" "misc"))
(setq usls-file-type-extension ".txt")
(setq usls-subdir-support nil)
(setq usls-file-region-separator 'line)
(setq usls-file-region-separator-heading-level 1)
(setq usls-custom-header-function nil)

;; keybindings
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
    "nl" '(usls-follow-link :which-key "usls follow link"))

  (general-create-definer p-latex-leader-def
    :prefix ";"
    :states '(normal visual)
    :keymaps 'LaTeX-mode-map)
  (p-latex-leader-def
   "p"  '(:ignore t :which-key "latex preview")
   "pp" '(latex-preview-pane-mode :which-key "toggle latex preview pane")))

(provide 'init-note)
;;; init-note.el ends here
