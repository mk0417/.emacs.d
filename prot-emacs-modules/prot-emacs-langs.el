;;; General language/editing settings
(prot-emacs-configure
  (:delay 2)
;;;; Tabs, indentation, and the TAB key
  (setq-default tab-always-indent 'complete
                tab-first-completion 'word-or-paren-or-punct ; Emacs 27
                tab-width 4
                indent-tabs-mode nil)

;;;; Disable "electric" behaviour
  (electric-pair-mode -1)
  (electric-quote-mode -1)
  ;; I don't like auto indents in Org and related.  They are okay for
  ;; programming.
  (electric-indent-mode -1)
  (add-hook 'prog-mode-hook #'electric-indent-local-mode)

;;;; Parentheses (show-paren-mode)
  (setq show-paren-style 'parenthesis)
  (setq show-paren-when-point-in-periphery nil)
  (setq show-paren-when-point-inside-paren nil)
  (setq show-paren-context-when-offscreen 'overlay) ; Emacs 29
  (add-hook 'after-init-hook #'show-paren-mode)

;;;; Emacs Lisp (emacs-lisp-mode)
  (prot-emacs-keybind emacs-lisp-mode-map
    "C-x e" #'edebug-defun ; override `kmacro-end-and-call-macro'
    "C-x E" #'edebug-remove-instrumentation)

;;;; Plain text (text-mode)
  (setq sentence-end-double-space nil)

  (defun prot/prog-mode-sentence-end-double-space ()
    "Set `sentence-end-double-space' to non-nil in the current buffer.
Meant to be added to `prog-mode-hook'."
    (setq-local sentence-end-double-space t))

  (add-hook 'prog-mode-hook #'prot/prog-mode-sentence-end-double-space)

  (setq sentence-end-without-period nil)
  (setq colon-double-space nil)
  (setq use-hard-newlines nil)
  (setq adaptive-fill-mode t)

  (add-hook 'text-mode-hook #'turn-on-auto-fill)

  (add-to-list 'auto-mode-alist '("\\(README\\|CHANGELOG\\|COPYING\\|LICENSE\\)\\'" . text-mode))

;;;; Arch Linux and AUR package scripts (sh-mode)
  (add-to-list 'auto-mode-alist '("PKGBUILD" . sh-mode))

;;;; SystemD and other configuration files (conf-mode)
  (add-to-list 'auto-mode-alist '("\\.\\(service\\|timer\\)\\'" . conf-mode))
  (add-to-list 'auto-mode-alist '("dircolors" . conf-mode))

;;;; Eldoc (elisp live documentation feedback)
  (setq eldoc-message-function #'message) ; don't use mode line for M-x eval-expression, etc.
  (global-eldoc-mode 1)

;;;; Eglot (built-in client for the language server protocol)
  (setq eglot-sync-connect nil)
  (setq eglot-autoshutdown t)

;;;; Handle performance for very long lines (so-long.el)
  (global-so-long-mode 1))

;;; Markdown (markdown-mode)
(prot-emacs-package markdown-mode
  (:install t)
  (:delay 5)
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
  (setq markdown-fontify-code-blocks-natively t))

;; FIXME 2023-08-28: The laptop I use now that I do not have
;; electricity/Internet at home does not build the Jinx C module.  I
;; am not sure what is happening.  Commenting out for the time being,
;; as I need to have Emacs up and running.

;;; Jinx (highly performant spell checker)
;; (if (executable-find "enchant-2")
;;   (prot-emacs-package jinx
;;     (:install t)
;;     (:delay 10)
;;     (setq jinx-languages "en_GB el_GR fr_FR es_ES pt_PT-preao")
;;     (setq jinx-include-modes '(text-mode prog-mode))
;;     (setq jinx-include-faces
;;           '((prog-mode font-lock-doc-face)
;;             (conf-mode font-lock-comment-face)))
;;     (setq jinx-exclude-regexps
;;           '((t "[A-Z]+\\>"
;;                "\\<[[:upper:]][[:lower:]]+\\>"
;;                "\\w*?[0-9\.'\"-]\\w*"
;;                "[a-z]+://\\S-+"
;;                "<?[-+_.~a-zA-Z][-+_.~:a-zA-Z0-9]*@[-.a-zA-Z0-9]+>?")))
;;
;;     (global-jinx-mode 1)
;;
;;     (define-key ctl-x-x-map "j" #'jinx-mode) ; C-x x j
;;
;;     (prot-emacs-keybind global-map
;;       "M-$" jinx-correct
;;       "C-M-$" jinx-languages))
;;   ;; I would use an `error' but I do not want it to interrupt startup.
;;   (message "libenchant is not available"))

;;; Flyspell and prot-spell.el (spell check)
;; See FIXME for `jinx'.
(prot-emacs-package flyspell
  (:delay 30)
  (setq flyspell-issue-message-flag nil)
  (setq flyspell-issue-welcome-flag nil)
  (setq ispell-program-name "aspell")
  (setq ispell-dictionary "en_GB")
  (define-key flyspell-mode-map (kbd "C-;") nil)
  (define-key ctl-x-x-map "s" flyspell-mode)) ; C-x x s

(prot-emacs-package prot-spell
  (:delay 30)
  (setq prot-spell-dictionaries
        '(("EN English" . "en")
          ("EL Ελληνικά" . "el")
          ("FR Français" . "fr")
          ("ES Espanõl" . "es")))

  (setq ispell-choices-buffer "*ispell-top-choices*") ; see my `display-buffer-alist'

  ;; Also check prot-spell.el for what I am doing with
  ;; `prot-spell-ispell-display-buffer'.  Then refer to the
  ;; `display-buffer-alist' for the relevant entry.

  (prot-emacs-keybind global-map
    "M-$" #'prot-spell-spell-dwim
    "C-M-$" #'prot-spell-change-dictionary))

;;; General configurations for prose/writing
(prot-emacs-configure
;;;; `outline' (`outline-mode' and `outline-minor-mode')
  (:delay 10)
  (setq outline-minor-mode-highlight nil) ; emacs28
  (setq outline-minor-mode-cycle t)             ; emacs28
  (setq outline-minor-mode-use-buttons nil) ; emacs29---bless you for the nil option!
  (setq outline-minor-mode-use-margins nil) ; as above
  (define-key global-map (kbd "<f10>") #'outline-minor-mode)

;;;; `docview' (simple PDF viewer)
  ;; The "mupdf" is a reference to the Arch Linux system packages
  ;; `mupdf', `mupdf-tools', `libmupdf'.
  (setq doc-view-pdf->png-converter-function #'doc-view-pdf->png-converter-mupdf)
  (setq doc-view-mupdf-use-svg (image-type-available-p 'svg)) ; Emacs 30
  (setq doc-view-resolution 300) ; (doc-view-clear-cache)

;;;; `dictionary'
  (setq dictionary-server "dict.org"
        dictionary-default-popup-strategy "lev" ; read doc string
        dictionary-create-buttons nil
        dictionary-use-single-buffer t)
  (define-key global-map (kbd "C-c d") #'dictionary-search))

;;; Denote (simple note-taking and file-naming)
;; Read the manual: <https://protesilaos.com/emacs/denote>.
(prot-emacs-package denote
  (:install t)
  (:delay 5)
  ;; Remember to check the doc strings of those variables.
  (setq denote-directory (expand-file-name "~/Dropbox/peng_notes/"))
  (setq denote-known-keywords '("emacs" "economics"))
  (setq denote-infer-keywords t)
  (setq denote-sort-keywords t)
  ;; (setq denote-file-type 'text) ; Org is the default, set others here like I do
  (setq denote-excluded-directories-regexp nil)
  (setq denote-allow-multi-word-keywords nil)
  (setq denote-date-format nil) ; read its doc string

  ;; By default, we fontify backlinks in their bespoke buffer.
  (setq denote-link-fontify-backlinks t)

  (denote-rename-buffer-mode 1)

  ;; Also see `denote-link-backlinks-display-buffer-action' which is a bit
  ;; advanced.

  ;; If you use Markdown or plain text files you want to buttonise
  ;; existing buttons upon visiting the file (Org renders links as
  ;; buttons right away).
  (add-hook 'find-file-hook #'denote-link-buttonize-buffer)

  ;; We use different ways to specify a path for demo purposes.
  (setq denote-dired-directories
        (list denote-directory
              (thread-last denote-directory (expand-file-name "attachments"))
              (expand-file-name "~/Documents/books")))

  ;; Generic (great if you rename files Denote-style in lots of places):
  (add-hook 'dired-mode-hook #'denote-dired-mode)
  ;;
  ;; OR if only want it in `denote-dired-directories':
  ;; (add-hook 'dired-mode-hook #'denote-dired-mode-in-directories)

  ;; Here is a custom, user-level command from one of the examples we
  ;; show in this manual.  We define it here and add it to a key binding
  ;; below.  The manual: <https://protesilaos.com/emacs/denote>.
  (defun prot/denote-journal ()
    "Create an entry tagged 'journal' with the date as its title.
If a journal for the current day exists, visit it.  If multiple
entries exist, prompt with completion for a choice between them.
Else create a new file."
    (interactive)
    (let* ((today (format-time-string "%A %e %B %Y"))
           (string (denote-sluggify today))
           (files (denote-directory-files-matching-regexp string)))
      (cond
       ((> (length files) 1)
        (find-file (completing-read "Select file: " files nil :require-match)))
       (files
        (find-file (car files)))
       (t
        (denote
         today
         '("journal"))))))

  ;; Denote DOES NOT define any key bindings.  This is for the user to
  ;; decide.  For example:
  (prot-emacs-keybind global-map
    "C-c n j" #'prot/denote-journal
    "C-c n n" #'denote
    "C-c n N" #'denote-type
    "C-c n d" #'denote-date
    "C-c n z" #'denote-signature ; "zettelkasten" mnemonic
    "C-c n s" #'denote-subdirectory
    ;; If you intend to use Denote with a variety of file types, it is
    ;; easier to bind the link-related commands to the `global-map', as
    ;; shown here.  Otherwise follow the same pattern for `org-mode-map',
    ;; `markdown-mode-map', and/or `text-mode-map'.
    "C-c n i" #'denote-link ; "insert" mnemonic
    "C-c n I" #'denote-add-links
    "C-c n b" #'denote-backlinks
    "C-c n f f" #'denote-find-link
    "C-c n f b" #'denote-find-backlink
    ;; Note that `denote-rename-file' can work from any context, not
    ;; just Dired buffers.  That is why we bind it here to the
    ;; `global-map'.
    ;;
    ;; Also see `denote-rename-file-using-front-matter' further below.
    "C-c n r" #'denote-rename-file)

  ;; Key bindings specifically for Dired.
  (prot-emacs-keybind dired-mode-map
    "C-c C-d C-i" #'denote-link-dired-marked-notes
    "C-c C-d C-r" #'denote-dired-rename-marked-files)

  ;; Also see `denote-rename-file' further above.
  (define-key text-mode-map (kbd "C-c n R") #'denote-rename-file-using-front-matter)

  (with-eval-after-load 'org-capture
    (setq denote-org-capture-specifiers "%l\n%i\n%?")
    (add-to-list 'org-capture-templates
                 '("n" "New note (with denote.el)" plain
                   (file denote-last-path)
                   #'denote-org-capture
                   :no-save t
                   :immediate-finish nil
                   :kill-buffer t
                   :jump-to-captured t))))

;;; Custom extensions for "focus mode" (logos.el)
;; Read the manual: <https://protesilaos.com/emacs/logos>.
(prot-emacs-package olivetti
  (:install t)
  (:delay 10)
  (setq olivetti-body-width 0.7)
  (setq olivetti-minimum-body-width 80)
  (setq olivetti-recall-visual-line-mode-entry-state t))

(provide 'prot-emacs-langs)
