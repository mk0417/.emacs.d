;;;;; init-dired.el --- Dired -*- lexical-binding: t -*-

(with-eval-after-load 'evil
  (evil-define-key 'normal dired-mode-map
    (kbd "M-RET") 'dired-display-file
    (kbd "P") 'dired-up-directory
    (kbd "F") 'dired-open-file
    (kbd "t") 'dired-toggle-marks
    (kbd "u") 'dired-unmark
    (kbd "C") 'dired-do-copy
    (kbd "D") 'dired-do-delete
    (kbd "J") 'dired-goto-file
    (kbd "M") 'dired-do-chmod
    (kbd "O") 'dired-do-chown
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
    (kbd "; .") 'dired-mark-extension
    (kbd "; d") 'dired-mark-directories))

(provide 'init-dired)
;;;;; init-dired.el ends here
