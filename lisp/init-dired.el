;;;;; init-dired.el --- Dired -*- lexical-binding: t -*-

(setq dired-kill-when-opening-new-dired-buffer t)

(with-eval-after-load 'evil
  (evil-define-key 'normal dired-mode-map
    (kbd "M-RET") 'dired-display-file
    (kbd "P") 'dired-up-directory
    (kbd "t") 'dired-toggle-marks
    (kbd "u") 'dired-unmark
    (kbd "C") 'dired-do-copy
    (kbd "D") 'dired-do-delete
    (kbd "J") 'dired-goto-file
    (kbd "R") 'dired-do-rename
    (kbd "T") 'dired-do-touch
    (kbd "Y") 'dired-copy-filenamecopy-filename-as-kill
    (kbd "Z") 'dired-do-compress
    (kbd "; ;") 'dired-mark
    (kbd "; n") 'dired-create-directory
    (kbd "; f") 'dired-create-empty-file
    (kbd "; k") 'dired-do-kill-lines
    (kbd "; l") 'dired-downcase
    (kbd "; m") 'dired-mark-files-regexp
    (kbd "; u") 'dired-upcase
    (kbd "; d") 'dired-mark-directories))

(provide 'init-dired)
;;;;; init-dired.el ends here
