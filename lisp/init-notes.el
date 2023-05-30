;;;;; init-notes.el --- Note -*- lexical-binding: t -*-

;;; Install packages
(straight-use-package 'denote)
(straight-use-package 'markdown-mode)
(straight-use-package 'adaptive-wrap)

;;; Markdown
(autoload 'markdown-mode "markdown-mode")
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(setq markdown-enable-math t)

;;; Denote
(require 'denote)

(setq denote-directory (expand-file-name "~/Dropbox/peng_notes/"))
(setq denote-infer-keywords t)
(setq denote-sort-keywords t)
(setq denote-file-type nil)
(setq denote-prompts '(title keywords subdirectory))
(setq denote-allow-multi-word-keywords t)
(setq denote-date-format nil)
(setq denote-link-fontify-backlinks t)
(setq denote-org-front-matter
      "#+title: %s
#+date: %s
#+filetags: %s
#+identifier: %s
\n")

(add-hook 'dired-mode-hook #'denote-dired-mode)

;; If you use Markdown or plain text files (Org renders links as buttons right away)
(add-hook 'find-file-hook #'denote-link-buttonize-buffer)

;; Find file in my notes
(defun p-find-file-in-notes ()
  (interactive)
  (let ((default-directory (file-truename (file-name-directory (expand-file-name "~/Dropbox/peng_notes/")))))
    (call-interactively 'find-file)))

(defun p-denote-journal ()
  (interactive)
  (denote
   (denote--title-prompt)
   '("journal")))

;;; Adaptive-wrap
(setq-default adaptive-wrap-extra-indent 0)

;;; Keybindings
(with-eval-after-load 'evil
  (general-create-definer p-space-leader-def
    :prefix "SPC"
    :states '(normal visual))
  (p-space-leader-def
    "n" '(:ignore t :which-key "notes")
    "nn" '(denote :which-key "denote")
    "nj" '(denote-subdirectory :which-key "denote subdirectory")
    "nl" '(denote-link :which-key "denote link")
    "nL" '(denote-link-add-links :which-key "denote links")
    "nv" '(denote-link-find-file :which-key "denote link find file")
    "ns" '(denote-link-backlinks :which-key "denote backlinks")
    "no" '(p-denote-journal :which-key "denote journal")
    "f" '(:ignore t :which-key "file")
    "fn" '(p-find-file-in-notes :which-key "find notes"))

  (general-create-definer p-quarto-leader-def
    :prefix ";"
    :states '(normal visual)
    :keymaps '(markdown-mode-map))
  (p-quarto-leader-def
    "j"  '(:ignore t :which-key "markdown")
    "jc" 'markdown-cycle))

(provide 'init-notes)
;;;;; init-notes.el ends here
