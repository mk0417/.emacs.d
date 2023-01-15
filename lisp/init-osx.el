;;;;; init-osx.el --- osx specific config -*- lexical-binding: t -*-

;;; Special keys
(setq mac-command-modifier 'super)
(setq ns-function-modifier 'hyper)

;;; Enable push back option after deleting a file
(when (featurep 'ns)
  ;; https://christiantietze.de/posts/2021/06/emacs-trash-file-macos/
  (setq trash-directory "~/.Trash")
  (defun system-move-file-to-trash (path)
    (shell-command (concat "trash -vF \"" path "\"" "| sed -e 's/^/Trashed: /'")
                   nil
                   "*Trash Error Buffer*")))

;;; Keybindings
;; ⌘-W = Close window
(global-set-key (kbd "s-W") 'delete-frame)
;; ⌘-Z = Redo
(unless (< emacs-major-version 28)
  (global-set-key (kbd "s-Z") 'undo-redo))

(provide 'init-osx)
;;;;; init-osx.el ends here
