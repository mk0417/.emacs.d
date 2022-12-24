;;;;; init-notes.el --- Note -*- lexical-binding: t -*-

;;; Install packages
(straight-use-package 'denote)
(straight-use-package 'markdown-mode)

;;; Markdown
(autoload 'markdown-mode "markdown-mode")
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;;; Denote
(require 'denote)

(setq denote-directory (expand-file-name "~/Dropbox/peng_notes/"))
(setq denote-known-keywords '("emacs" "python"))
(setq denote-infer-keywords t)
(setq denote-sort-keywords t)
(setq denote-file-type nil)
(setq denote-prompts '(title subdirectory))
(setq denote-allow-multi-word-keywords t)
(setq denote-date-format nil)
(setq denote-link-fontify-backlinks t)

;; (require 'denote-retrieve)
;; (require 'denote-link)

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
    "fn" '(p-find-file-in-notes :which-key "find notes")))

(provide 'init-notes)
;;;;; init-notes.el ends here
