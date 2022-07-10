;;;;; init-org.el --- Org-mode -*- lexical-binding: t -*-

;;; Install packages
(straight-use-package 'org-modern)

;;; Org
(setq org-directory "~/Dropbox/org")
(setq org-agenda-files '("~/Dropbox/org/todo.org"))
(setq org-log-done t)
(setq org-src-fontify-natively t)
(setq org-export-use-babel nil)
(setq org-confirm-babel-evaluate nil)
(setq org-fontify-quote-and-verse-blocks t)
(setq org-hide-emphasis-markers t)
(setq org-export-coding-system 'utf-8)
(setq org-html-validation-link nil)
(setq org-insert-heading-respect-content t)
(setq org-pretty-entities t)
(setq org-ellipsis "…")
(setq org-tags-column 0)

;;; Org-modern-mode
(setq org-modern-label-border 1)
(setq org-modern-timestamp t)
(setq org-modern-table t)
(setq org-modern-table-vertical 1)
(setq org-modern-table-horizontal 0)
(setq org-modern-horizontal-rule t)
(setq org-modern-list
      '((?+ . "•")
        (?- . "–")
        (?* . "◦")))

(add-hook 'org-mode-hook #'org-modern-mode)
(add-hook 'org-agenda-finalize-hook #'org-modern-agenda)

;;; Keybindings
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(with-eval-after-load 'org
  (define-key org-mode-map (kbd "<tab>") 'org-cycle)
  (define-key org-mode-map (kbd "C-c l") 'org-store-link))

(with-eval-after-load 'evil
  (general-create-definer p-org-leader-def
    :prefix ";"
    :states '(normal visual)
    :keymaps 'org-mode-map)
  (p-org-leader-def
    "d" '(org-set-tags-command :which-key "set org tag")
    "t" '(:ignore t :which-key "table")
    "tk" '(org-table-move-row-up :which-key "move row up")
    "tj" '(org-table-move-row-down :which-key "move row down")
    "tl" '(org-table-move-column-right :which-key "move column right")
    "th" '(org-table-move-column-left :which-key "move column left")
    "tc" '(org-table-convert-region :which-key "convert region to table")))

(provide 'init-org)
;;;;; init-org.el ends here
