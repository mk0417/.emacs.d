;;; Dired file manager and prot-dired.el extras

(require 'dired)
(require 'prot-dired)

(setq dired-recursive-copies 'always)
(setq dired-recursive-deletes 'always)
(setq delete-by-moving-to-trash t)
(setq dired-listing-switches "-AGFhlv --group-directories-first --time-style=long-iso")
(setq dired-dwim-target t)
(setq dired-auto-revert-buffer #'dired-directory-changed-p) ; also see `dired-do-revert-buffer'
(setq dired-make-directory-clickable t) ; Emacs 29.1
(setq dired-free-space nil) ; Emacs 29.1
(setq dired-mouse-drag-files t) ; Emacs 29.1
(setq dired-guess-shell-alist-user ; those are the suggestions for ! and & in Dired
      '(("\\.\\(png\\|jpe?g\\|tiff\\)" "feh" "xdg-open")
        ("\\.\\(mp[34]\\|m4a\\|ogg\\|flac\\|webm\\|mkv\\)")
        (".*")))

(add-hook 'dired-mode-hook #'dired-hide-details-mode)
(add-hook 'dired-mode-hook #'hl-line-mode)

(setq dired-isearch-filenames 'dwim)
(setq dired-create-destination-dirs 'ask) ; Emacs 27
(setq dired-vc-rename-file t)             ; Emacs 27
(setq dired-do-revert-buffer (lambda (dir) (not (file-remote-p dir)))) ; Emacs 28
(setq dired-create-destination-dirs-on-trailing-dirsep t) ; Emacs 29

(prot-emacs-keybind dired-mode-map
  "C-+" #'dired-create-empty-file
  "M-s f" #'nil
  "C-x v v" #'dired-vc-next-action) ; Emacs 28

(setq dired-clean-up-buffers-too t)
(setq dired-clean-confirm-killing-deleted-buffers t)
(setq dired-x-hands-off-my-keys t)    ; easier to show the keys I use
(setq dired-bind-man nil)
(setq dired-bind-info nil)
(define-key dired-mode-map (kbd "I") #'dired-info)

(add-hook 'dired-mode-hook #'prot-dired-setup-imenu)

(prot-emacs-keybind dired-mode-map
  "i" #'prot-dired-insert-subdir ; override `dired-maybe-insert-subdir'
  "/" #'prot-dired-limit-regexp
  "C-c C-l" #'prot-dired-limit-regexp
  "M-n" #'prot-dired-subdirectory-next
  "C-c C-n" #'prot-dired-subdirectory-next
  "M-p" #'prot-dired-subdirectory-previous
  "C-c C-p" #'prot-dired-subdirectory-previous
  "M-s G" #'prot-dired-grep-marked-files) ; M-s g is `prot-search-grep'

(setq wdired-allow-to-change-permissions t)
(setq wdired-create-parent-directories t)

(provide 'prot-emacs-dired)