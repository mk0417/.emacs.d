;;; Vertical completion layout
(prot-emacs-package vertico
  (:install t)
  ;; (:delay 1)
  (setq vertico-scroll-margin 0)
  (setq vertico-count 5)
  (setq vertico-resize nil)
  (setq vertico-cycle t)

  (vertico-mode 1)

  ;; This works with `file-name-shadow-mode' enabled.  When you are in
  ;; a sub-directory and use, say, `find-file' to go to your home '~/'
  ;; or root '/' directory, Vertico will clear the old path to keep
  ;; only your current input.
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy))

;;; Custom tweaks for vertico (prot-vertico.el)
(prot-emacs-package prot-vertico
  (:delay 5)
  (setq vertico-multiform-categories
        `(;; Maximal
          (embark-keybinding ,@prot-vertico-multiform-maximal)
          (multi-category ,@prot-vertico-multiform-maximal)
          (consult-location ,@prot-vertico-multiform-maximal)
          (imenu ,@prot-vertico-multiform-maximal)
          (unicode-name ,@prot-vertico-multiform-maximal)
          ;; Minimal
          (file ,@prot-vertico-multiform-minimal
                (vertico-preselect . prompt)
                (vertico-sort-function . prot-vertico-sort-directories-first))
          (t ,@prot-vertico-multiform-minimal)))

  (vertico-multiform-mode 1)

  (prot-emacs-keybind vertico-map
    "<left>" #'backward-char
    "<right>" #'forward-char
    "TAB" #'prot-vertico-private-complete
    "DEL" #'vertico-directory-delete-char
    "M-DEL" #'vertico-directory-delete-word
    "M-," #'vertico-quick-insert
    "M-." #'vertico-quick-exit
    "M-h" #'vertico-directory-up)

  (prot-emacs-keybind vertico-multiform-map
    "C-n" #'prot-vertico-private-next
    "<down>" #'prot-vertico-private-next
    "C-p" #'prot-vertico-private-previous
    "<up>" #'prot-vertico-private-previous
    "C-l" #'vertico-multiform-vertical))

(provide 'prot-emacs-completion-vertico)
