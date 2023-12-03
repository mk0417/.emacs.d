;;; General minibuffer settings
(prot-emacs-configure
  (:delay 1)
;;;; Minibuffer configurations
  (setq completion-styles '(basic substring initials flex orderless)) ; also see `completion-category-overrides'
  (setq completion-category-defaults nil)

  ;; A list of known completion categories:
  ;;
  ;; - `bookmark'
  ;; - `buffer'
  ;; - `charset'
  ;; - `coding-system'
  ;; - `color'
  ;; - `command' (e.g. `M-x')
  ;; - `customize-group'
  ;; - `environment-variable'
  ;; - `expression'
  ;; - `face'
  ;; - `file'
  ;; - `function' (the `describe-function' command bound to `C-h f')
  ;; - `info-menu'
  ;; - `imenu'
  ;; - `input-method'
  ;; - `kill-ring'
  ;; - `library'
  ;; - `minor-mode'
  ;; - `multi-category'
  ;; - `package'
  ;; - `project-file'
  ;; - `symbol' (the `describe-symbol' command bound to `C-h o')
  ;; - `theme'
  ;; - `unicode-name' (the `insert-char' command bound to `C-x 8 RET')
  ;; - `variable' (the `describe-variable' command bound to `C-h v')
  ;;
  ;; From the `consult' package:
  ;;
  ;; - `consult-grep'
  ;; - `consult-isearch'
  ;; - `consult-kmacro'
  ;; - `consult-location'
  ;;
  ;; From the `embark' package:
  ;;
  ;; - `embark-keybinding'
  ;;
  (setq completion-category-overrides
        ;; NOTE 2021-10-25: I am adding `basic' because it works better as a
        ;; default for some contexts.  Read:
        ;; <https://debbugs.gnu.org/cgi/bugreport.cgi?bug=50387>.
        ;;
        ;; `partial-completion' is a killer app for files, because it
        ;; can expand ~/.l/s/fo to ~/.local/share/fonts.
        ;;
        ;; If `basic' cannot match my current input, Emacs tries the
        ;; next completion style in the given order.  In other words,
        ;; `orderless' kicks in as soon as I input a space or one of its
        ;; style dispatcher characters.
        '((file (styles . (basic partial-completion orderless)))
          (bookmark (styles . (basic substring)))
          (library (styles . (basic substring)))
          (embark-keybinding (styles . (basic substring)))
          (imenu (styles . (basic substring orderless)))
          (consult-location (styles . (basic substring orderless)))
          (kill-ring (styles . (emacs22 orderless)))
          (eglot (styles . (emacs22 substring orderless)))))

  (setq completion-ignore-case t)
  (setq read-buffer-completion-ignore-case t)
  (setq read-file-name-completion-ignore-case t)
  (setq-default case-fold-search t)   ; For general regexp

  (setq enable-recursive-minibuffers t)
  ;; Allow Emacs to resize mini windows, otherwise this does not work:
  ;;   (setq org-use-fast-todo-selection 'expert)
  (setq resize-mini-windows t)
  (setq minibuffer-eldef-shorten-default t)

  (setq read-answer-short t) ; also check `use-short-answers' for Emacs28
  (setq echo-keystrokes 0.25)
  (setq kill-ring-max 60)               ; Keep it small

  ;; Do not allow the cursor to move inside the minibuffer prompt.  I
  ;; got this from the documentation of Daniel Mendler's Vertico
  ;; package: <https://github.com/minad/vertico>.
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))

  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Add prompt indicator to `completing-read-multiple'.  We display
  ;; [`completing-read-multiple': <separator>], e.g.,
  ;; [`completing-read-multiple': ,] if the separator is a comma.  This
  ;; is adapted from the README of the `vertico' package by Daniel
  ;; Mendler.  I made some small tweaks to propertize the segments of
  ;; the prompt.
  (defun crm-indicator (args)
    (cons (format "[`completing-read-multiple': %s]  %s"
                  (propertize
                   (replace-regexp-in-string
                    "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                    crm-separator)
                   'face 'error)
                  (car args))
          (cdr args)))

  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Settings for the default completion UI.  These do not come into
  ;; effect unless `prot-emacs-completion-ui' is nil or when not using
  ;; any package for in-buffer completion.
  (setq completion-show-help nil)
  (setq completion-auto-help t)
  (setq completion-auto-select nil)
  (setq completions-detailed t)
  (setq completion-show-inline-help nil)
  (setq completions-max-height 6)
  (setq completions-header-format
        (propertize "%s candidates:\n" 'face 'font-lock-comment-face))
  (setq completions-highlight-face 'completions-highlight)

  (file-name-shadow-mode 1)
  (minibuffer-depth-indicate-mode 1)
  (minibuffer-electric-default-mode 1)
  
  (define-key minibuffer-local-map (kbd "C-k") 'delete-backward-char)
  (define-key minibuffer-local-map (kbd "C-w") 'backward-kill-word)

;;;; `dabbrev' (dynamic word completion (dynamic abbreviations))
  (setq dabbrev-abbrev-char-regexp "\\sw\\|\\s_")
  (setq dabbrev-abbrev-skip-leading-regexp "[$*/=~']")
  (setq dabbrev-backward-only nil)
  (setq dabbrev-case-distinction 'case-replace)
  (setq dabbrev-case-fold-search nil)
  (setq dabbrev-case-replace 'case-replace)
  (setq dabbrev-check-other-buffers t)
  (setq dabbrev-eliminate-newlines t)
  (setq dabbrev-upcase-means-case-search t)
  (setq dabbrev-ignored-buffer-modes
        '(archive-mode image-mode docview-mode pdf-view-mode))

;;;; `abbrev' (Abbreviations, else Abbrevs)
  (setq abbrev-file-name (locate-user-emacs-file "abbrevs"))
  (setq only-global-abbrevs nil)

  (prot-emacs-abbrev global-abbrev-table
    "meweb" "https://protesilaos.com"
    "megit" "https://git.sr.ht/~protesilaos"
    "mehub" "https://github.com/protesilaos"
    "melab" "https://gitlab.com/protesilaos")

  (prot-emacs-abbrev text-mode-abbrev-table
    "asciidoc"       "AsciiDoc"
    "auctex"         "AUCTeX"
    "cliche"         "cliché"
    "clojurescript"  "ClojureScript"
    "emacsconf"      "EmacsConf"
    "github"         "GitHub"
    "gitlab"         "GitLab"
    "javascript"     "JavaScript"
    "latex"          "LaTeX"
    "libreplanet"    "LibrePlanet"
    "linkedin"       "LinkedIn"
    "paypal"         "PayPal"
    "sourcehut"      "SourceHut"
    "texmacs"        "TeXmacs"
    "typescript"     "TypeScript"
    "visavis"        "vis-à-vis"
    "youtube"        "YouTube")

  (with-eval-after-load 'message
    (prot-emacs-abbrev message-mode-abbrev-table
      "bestregards"  "Best regards,\nProtesilaos (or simply \"Prot\")"
      "allthebest"   "All the best,\nProtesilaos (or simply \"Prot\")"
      "abest"        "All the best,\nProt"
      "bregards"     "Best regards,\nProt"))

  ;; message-mode derives from text-mode, so we don't need a separate
  ;; hook for it.
  (dolist (hook '(text-mode-hook prog-mode-hook git-commit-mode-hook))
    (add-hook hook #'abbrev-mode))

  ;; By default, abbrev asks for confirmation on whether to use
  ;; `abbrev-file-name' to save abbrevations.  I do not need that, nor
  ;; do I want it.
  (remove-hook 'save-some-buffers-functions #'abbrev--possibly-save)
  
  (prot-emacs-keybind global-map
    "M-/" #'dabbrev-expand
    "C-x M-/" #'dabbrev-completion
    "C-x a e" #'expand-abbrev ; default, just here for visibility
    "C-x a u" #'unexpand-abbrev))

;;; Orderless completion style (and prot-orderless.el)
(prot-emacs-package orderless
  (:install t)
  (:delay 5)
  (setq orderless-component-separator " +")
  ;; Remember to check my `completion-styles' and the
  ;; `completion-category-overrides'.
  (setq orderless-matching-styles
        '(orderless-prefixes orderless-regexp))

  ;; SPC should never complete: use it for `orderless' groups.
  ;; The `?' is a regexp construct.
  (prot-emacs-keybind minibuffer-local-completion-map
    "SPC" nil
    "?" nil))

(prot-emacs-package prot-orderless
  (setq orderless-style-dispatchers
        '(prot-orderless-literal
          prot-orderless-file-ext
          prot-orderless-beg-or-end)))

;;; Enhanced minibuffer commands (consult.el)
(prot-emacs-package consult
  (:install t)
  (:delay 5)
  (setq consult-line-numbers-widen t)
  ;; (setq completion-in-region-function #'consult-completion-in-region)
  (setq consult-async-min-input 3)
  (setq consult-async-input-debounce 0.5)
  (setq consult-async-input-throttle 0.8)
  (setq consult-narrow-key nil)
  (setq register-preview-delay 0.8
        register-preview-function #'consult-register-format)
  (setq consult-find-args
        (concat "find . -not ( "
                "-path */.git* -prune "
                "-or -path */.cache* -prune )"))
  (setq consult-preview-key 'any)

  (add-to-list 'consult-mode-histories '(vc-git-log-edit-mode . log-edit-comment-ring))

  (add-hook 'completion-list-mode-hook #'consult-preview-at-point-mode)

  (require 'consult-imenu) ; the `imenu' extension is in its own file

  (prot-emacs-keybind global-map
    "M-g M-g" #'consult-goto-line
    "M-K" #'consult-keep-lines ; M-S-k is similar to M-S-5 (M-%)
    "M-F" #'consult-focus-lines ; same principle
    "M-s M-b" #'consult-buffer
    "M-s M-f" #'consult-find
    "M-s M-g" #'consult-grep
    "M-s M-h" #'consult-history
    "M-s M-i" #'consult-imenu
    "M-s M-l" #'consult-line
    "M-s M-m" #'consult-mark
    "M-s M-y" #'consult-yank-pop
    "M-s M-s" #'consult-outline)
  (define-key consult-narrow-map (kbd "?") #'consult-narrow-help)
  (define-key minibuffer-local-map (kbd "C-s") #'consult-history)

  (with-eval-after-load 'pulsar
    ;; see my `pulsar' package: <https://protesilaos.com/emacs/pulsar>
    (setq consult-after-jump-hook nil) ; reset it to avoid conflicts with my function
    (dolist (fn '(pulsar-recenter-top pulsar-reveal-entry))
      (add-hook 'consult-after-jump-hook fn))))

;;; Extended minibuffer actions and more (embark.el and prot-embark.el)
(prot-emacs-package embark
  (:install t)
  ;; (:delay 5)
  (setq prefix-help-command #'embark-prefix-help-command)
  ;; (setq prefix-help-command #'describe-prefix-bindings) ; the default of the above

  (setq embark-confirm-act-all nil)
  (setq embark-mixed-indicator-both nil)
  (setq embark-mixed-indicator-delay 1.0)
  (setq embark-indicators '(embark-mixed-indicator embark-highlight-indicator))
  (setq embark-verbose-indicator-nested nil) ; I think I don't have them, but I do not want them either
  (setq embark-verbose-indicator-buffer-sections '(bindings))
  (setq embark-verbose-indicator-excluded-actions
        '(embark-cycle embark-act-all embark-collect embark-export embark-insert))

  ;; I never cycle and want to disable the key.  Normally, a nil value
  ;; disables a key binding but here that value is interpreted as the
  ;; binding for `embark-act'.  So I just add some obscure key that I
  ;; do not have.  I absolutely do not want to cycle by accident!
  (setq embark-cycle-key "<XF86Travel>")

  ;; The minimal indicator shows cycling options, but I have no use
  ;; for those.  I want it to be silent.
  (defun prot/embark-no-minimal-indicator ())
  (advice-add #'embark-minimal-indicator :override #'prot/embark-no-minimal-indicator)

  (defun prot/embark-act-no-quit ()
    "Call `embark-act' but do not quit after the action."
    (interactive)
    (let ((embark-quit-after-action nil))
      (call-interactively #'embark-act)))

  (defun prot/embark-act-quit ()
    "Call `embark-act' and quit after the action."
    (interactive)
    (let ((embark-quit-after-action t))
      (call-interactively #'embark-act))
    (when (and (> (minibuffer-depth) 0)
               (derived-mode-p 'completion-list-mode))
      (abort-recursive-edit)))

  (dolist (map (list global-map embark-collect-mode-map minibuffer-local-filename-completion-map))
    (define-key map (kbd "C-,") #'prot/embark-act-no-quit)
    (define-key map (kbd "C-.") #'prot/embark-act-quit))

  ;; I do not want `embark-org' and am not sure what is loading it.
  ;; So I just unsert all the keymaps...
  (with-eval-after-load 'embark-org
    (defvar prot/embark-org-keymaps
      '(embark-org-table-cell-map
        embark-org-table-map
        embark-org-link-copy-map
        embark-org-link-map
        embark-org-src-block-map
        embark-org-item-map
        embark-org-plain-list-map
        embark-org-export-in-place-map)
      "List of Embark keymaps for Org.")

    ;; Reset `prot/embark-org-keymaps'.
    (seq-do
     (lambda (keymap)
       (set keymap (make-sparse-keymap)))
     prot/embark-org-keymaps)))

(prot-emacs-package prot-embark
  (:delay 5)
  (setq embark-keymap-alist
        '((buffer prot-embark-buffer-map)
          (command prot-embark-command-map)
          (expression prot-embark-expression-map)
          (file prot-embark-file-map)
          (function prot-embark-function-map)
          (identifier prot-embark-identifier-map)
          (package prot-embark-package-map)
          (region prot-embark-region-map)
          (symbol prot-embark-symbol-map)
          (url prot-embark-url-map)
          (variable prot-embark-variable-map))))

;; Needed for correct exporting while using Embark with Consult
;; commands.
(prot-emacs-package embark-consult (:install t) (:delay 5))

;;; Template-based in-buffer completion (tempel.el)
;; NOTE 2023-01-12: Check the `templates' file that I distribute with
;; my Emacs files as part of my dotfiles:
;; <https://git.sr.ht/~protesilaos/dotfiles>.
(prot-emacs-package tempel
  (:install t)
  (:delay 5)
  (setq tempel-path (expand-file-name "tempel-templates" user-emacs-directory))

  (prot-emacs-keybind global-map
    "M-+" #'tempel-complete ; Alternative: `tempel-expand'
    "M-*" #'tempel-insert)
  (prot-emacs-keybind tempel-map
    "RET" #'tempel-done
    "C-p" #'tempel-previous
    "C-n" #'tempel-next
    "<tab>" #'tempel-next
    "<backtab>" #'tempel-previous
    "C-S-<iso-lefttab>" #'tempel-previous))

;;; Detailed completion annotations (marginalia.el)
(prot-emacs-package marginalia
  (:install t)
  (:delay 5)
  (setq marginalia-max-relative-age 0) ; absolute time
  (marginalia-mode 1))

;;;; Custom completion annotations
(prot-emacs-package prot-marginalia
  (:delay 5)
  (setq marginalia-annotator-registry
        '((bookmark prot-marginalia-bookmark)
          (buffer prot-marginalia-buffer)
          (command marginalia-annotate-command)
          (function prot-marginalia-symbol)
          (symbol prot-marginalia-symbol)
          (variable prot-marginalia-symbol)
          (face marginalia-annotate-face)
          (imenu marginalia-annotate-imenu)
          (package prot-marginalia-package)
          (unicode-name marginalia-annotate-char))))

(provide 'prot-emacs-completion-common)
