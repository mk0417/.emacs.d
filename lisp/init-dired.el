;;;;; init-dired.el --- Dired -*- lexical-binding: t -*-

;;; package
(straight-use-package 'diredfl)

;;; dired
(require 'dired)

(setq dired-recursive-deletes 'always)
(setq dired-recursive-copies 'always)
(setq dired-dwim-target t)
(when (eq system-type 'darwin)
  (setq dired-use-ls-dired nil))

(add-hook 'after-init-hook 'diredfl-global-mode)

(with-eval-after-load 'dired
  (general-create-definer p-dired-leader-def
    :prefix ";"
    :states '(normal visual)
    :keymaps 'dired-mode-map)
  (p-dired-leader-def
    "m"  '(dired-mark-files-regexp :which-key "mark file by regex")
    "M"  '(dired-mark-files-containing-regexp :which-key "mark file containing by regex")
    "c"  '(dired-do-copy :which-key "copy file")
    "r"  '(dired-do-rename :which-key "rename file")
    "p"  '(dired-up-directory :which-key "parent directory")
    "n"  '(dired-create-empty-file :which-key "add new file")
    "N"  '(dired-create-directory :which-key "add new directory")
    "i"  '(dired-find-file :which-key "enter directory")))

(provide 'init-dired)
;;;;; init-dired.el ends here
