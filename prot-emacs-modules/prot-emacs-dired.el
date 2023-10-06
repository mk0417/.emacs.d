;;; Dired file manager and prot-dired.el extras
(prot-emacs-package dired
  (:delay 5)
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (setq delete-by-moving-to-trash t)
  (setq dired-listing-switches
        "-AGFhlv --group-directories-first --time-style=long-iso")
  (setq dired-dwim-target t)
  (setq dired-auto-revert-buffer #'dired-directory-changed-p) ; also see `dired-do-revert-buffer'
  (setq dired-make-directory-clickable t) ; Emacs 29.1
  (setq dired-free-space nil) ; Emacs 29.1
  (setq dired-mouse-drag-files t) ; Emacs 29.1
  (setq dired-guess-shell-alist-user ; those are the suggestions for ! and & in Dired
        '(("\\.\\(png\\|jpe?g\\|tiff\\)" "feh" "xdg-open")
          ("\\.\\(mp[34]\\|m4a\\|ogg\\|flac\\|webm\\|mkv\\)" "mpv" "xdg-open")
		  (".*" "xdg-open")))

  (add-hook 'dired-mode-hook #'dired-hide-details-mode)
  (add-hook 'dired-mode-hook #'hl-line-mode)

  ;; In Emacs 29 there is a binding for `repeat-mode' which let you
  ;; repeat C-x C-j just by following it up with j.  For me, this is a
  ;; problem as j calls `dired-goto-file', which I often use.
  (define-key dired-jump-map (kbd "j") nil))

(prot-emacs-package dired-aux
  (:delay 5)
  (setq dired-isearch-filenames 'dwim)
  (setq dired-create-destination-dirs 'ask) ; Emacs 27
  (setq dired-vc-rename-file t)             ; Emacs 27
  (setq dired-do-revert-buffer (lambda (dir) (not (file-remote-p dir)))) ; Emacs 28
  (setq dired-create-destination-dirs-on-trailing-dirsep t) ; Emacs 29

  (prot-emacs-keybind dired-mode-map
    "C-+" dired-create-empty-file
    "M-s f" nil
    "C-x v v" dired-vc-next-action)) ; Emacs 28

;; ;; NOTE 2021-05-10: I do not use `find-dired' and related commands
;; ;; because there are other tools that offer a better interface, such
;; ;; as `consult-find', `consult-grep', `project-find-file',
;; ;; `project-find-regexp', `prot-vc-git-grep'.
;; (prot-emacs-package find-dired
;;   (setq find-ls-option
;;         '("-ls" . "-AGFhlv --group-directories-first --time-style=long-iso"))
;;   (setq find-name-arg "-iname"))

(prot-emacs-package dired-x
  (:delay 5)
  (setq dired-clean-up-buffers-too t)
  (setq dired-clean-confirm-killing-deleted-buffers t)
  (setq dired-x-hands-off-my-keys t)    ; easier to show the keys I use
  (setq dired-bind-man nil)
  (setq dired-bind-info nil)
  (define-key dired-mode-map (kbd "I") #'dired-info))

(prot-emacs-package prot-dired
  (:delay 5)
  (add-hook 'dired-mode-hook #'prot-dired-setup-imenu)

  (prot-emacs-keybind dired-mode-map
    "i" prot-dired-insert-subdir ; override `dired-maybe-insert-subdir'
    "/" prot-dired-limit-regexp
    "C-c C-l" prot-dired-limit-regexp
    "M-n" prot-dired-subdirectory-next
    "C-c C-n" prot-dired-subdirectory-next
    "M-p" prot-dired-subdirectory-previous
    "C-c C-p" prot-dired-subdirectory-previous
    "M-s G" prot-dired-grep-marked-files)) ; M-s g is `prot-search-grep'

(prot-emacs-package wdired
  (:delay 5)
  (setq wdired-allow-to-change-permissions t)
  (setq wdired-create-parent-directories t))

(prot-emacs-package image-dired
  (:delay 10)
  (setq image-dired-thumbnail-storage 'standard)
  (setq image-dired-external-viewer "xdg-open")
  (setq image-dired-thumb-size 80)
  (setq image-dired-thumb-margin 2)
  (setq image-dired-thumb-relief 0)
  (setq image-dired-thumbs-per-row 4)
  (define-key image-dired-thumbnail-mode-map
              (kbd "<return>") #'image-dired-thumbnail-display-external))

(provide 'prot-emacs-dired)
