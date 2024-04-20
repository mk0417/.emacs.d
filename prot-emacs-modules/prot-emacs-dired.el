;;; Dired file manager and prot-dired.el extras
(use-package dired
  :ensure nil
  :commands (dired)
  :config
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (setq delete-by-moving-to-trash t))

(use-package dired
  :ensure nil
  :commands (dired)
  :config
  (setq dired-listing-switches
        "-AGFhlv --group-directories-first --time-style=long-iso"))

(use-package dired
  :ensure nil
  :commands (dired)
  :config
  (setq dired-dwim-target t))

(use-package dired
  :ensure nil
  :commands (dired)
  :config
  (setq dired-guess-shell-alist-user ; those are the suggestions for ! and & in Dired
        '(("\\.\\(png\\|jpe?g\\|tiff\\)" "feh" "xdg-open")
          ("\\.\\(mp[34]\\|m4a\\|ogg\\|flac\\|webm\\|mkv\\)" "mpv" "xdg-open")
          (".*" "xdg-open"))))

(use-package dired
  :ensure nil
  :commands (dired)
  :config
  (setq dired-auto-revert-buffer #'dired-directory-changed-p) ; also see `dired-do-revert-buffer'
  (setq dired-make-directory-clickable t) ; Emacs 29.1
  (setq dired-free-space nil) ; Emacs 29.1
  (setq dired-mouse-drag-files t) ; Emacs 29.1

  (add-hook 'dired-mode-hook #'dired-hide-details-mode)
  (add-hook 'dired-mode-hook #'hl-line-mode)

  ;; In Emacs 29 there is a binding for `repeat-mode' which lets you
  ;; repeat C-x C-j just by following it up with j.  For me, this is a
  ;; problem as j calls `dired-goto-file', which I often use.
  (define-key dired-jump-map (kbd "j") nil))

(use-package dired-aux
  :ensure nil
  :after dired
  :bind
  ( :map dired-mode-map
    ("C-+" . dired-create-empty-file)
    ("M-s f" . nil)
    ("C-<return>" . dired-do-open) ; Emacs 30
    ("C-x v v" . dired-vc-next-action)) ; Emacs 28
  :config
  (setq dired-isearch-filenames 'dwim)
  (setq dired-create-destination-dirs 'ask) ; Emacs 27
  (setq dired-vc-rename-file t)             ; Emacs 27
  (setq dired-do-revert-buffer (lambda (dir) (not (file-remote-p dir)))) ; Emacs 28
  (setq dired-create-destination-dirs-on-trailing-dirsep t)) ; Emacs 29

(use-package dired-x
  :ensure nil
  :after dired
  :bind
  ( :map dired-mode-map
    ("I" . dired-info))
  :config
  (setq dired-clean-up-buffers-too t)
  (setq dired-clean-confirm-killing-deleted-buffers t)
  (setq dired-x-hands-off-my-keys t)    ; easier to show the keys I use
  (setq dired-bind-man nil)
  (setq dired-bind-info nil))

(use-package prot-dired
  :ensure nil
  :hook (dired-mode . prot-dired-setup-imenu)
  :bind
  ( :map dired-mode-map
    ("i" . prot-dired-insert-subdir) ; override `dired-maybe-insert-subdir'
    ("/" . prot-dired-limit-regexp)
    ("C-c C-l" . prot-dired-limit-regexp)
    ("M-n" . prot-dired-subdirectory-next)
    ("C-c C-n" . prot-dired-subdirectory-next)
    ("C-c C-p" . prot-dired-subdirectory-previous)
    ("M-s G" . prot-dired-grep-marked-files) ; M-s g is `prot-search-grep'
    ("M-p" . prot-dired-subdirectory-previous)))

(use-package dired-subtree
  :ensure t
  :bind
  ( :map dired-mode-map
    ("<tab>" . dired-subtree-toggle)
    ("<backtab>" . dired-subtree-remove)) ; S-TAB
  :config
  (setq dired-subtree-use-backgrounds nil))

(use-package wdired
  :ensure nil
  :commands (wdired-change-to-wdired-mode)
  :config
  (setq wdired-allow-to-change-permissions t)
  (setq wdired-create-parent-directories t))

(provide 'prot-emacs-dired)
