;;;;; init-completion.el --- Completion -*- lexical-binding: t -*-

;;; Install packages
(straight-use-package '(vertico :files ("*.el" "extensions/*.el")))
(straight-use-package 'orderless)
(straight-use-package 'consult)
(straight-use-package 'embark)
(straight-use-package 'embark-consult)
(straight-use-package 'marginalia)

;;; Vertico
(require 'vertico)

(setq vertico-cycle t)
(setq vertico-scroll-margin 0)
(setq vertico-resize nil)

(vertico-mode 1)
(vertico-multiform-mode)

;; sort directories first
(defun sort-directories-first (files)
  (setq files (vertico-sort-history-alpha files))
  (nconc (seq-filter (lambda (x) (string-suffix-p "/" x)) files)
         (seq-remove (lambda (x) (string-suffix-p "/" x)) files)))

(setq vertico-multiform-categories
      '((symbol (vertico-sort-function . vertico-sort-alpha))
        (file (vertico-sort-function . sort-directories-first))
        (t (vertico-sort-function . vertico-sort-history-alpha))))

(add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)

(define-key vertico-map (kbd "M-h") 'vertico-directory-up)

;;; General minibuffer settings
;;;; Minibuffer configurations
(setq completion-styles '(emacs22 substring orderless)) ; also see `completion-category-overrides'
(setq completion-category-defaults nil)
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
        (project-file (styles . (basic substring partial-completion orderless)))
        (imenu (styles . (emacs22 substring orderless)))
        (kill-ring (styles . (emacs22 substring orderless)))
        (consult-location (styles . (emacs22 substring orderless)))
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

;; Do not allow the cursor to move inside the minibuffer prompt.  I
;; got this from the documentation of Daniel Mendler's Vertico
;; package: <https://github.com/minad/vertico>.
(setq minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt))

(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

;; Add prompt indicator to `completing-read-multiple'.  We display
;; [CRM<separator>], e.g., [CRM,] if the separator is a comma.  This
;; is copied from the README of the `vertico' package.  I made some
;; small tweaks to propertize the segments of the prompt.
(defun crm-indicator (args)
  (cons (format "[`crm-separator': %s]  %s"
                (propertize
                 (replace-regexp-in-string
                  "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                  crm-separator)
                 'face 'error)
                (car args))
        (cdr args)))

(advice-add #'completing-read-multiple :filter-args #'crm-indicator)

(file-name-shadow-mode 1)
(minibuffer-depth-indicate-mode 1)
(minibuffer-electric-default-mode 1)

;;; Orderless
;;; Orderless completion style (and prot-orderless.el)
(require 'prot-orderless)

(setq orderless-component-separator " +")
;; Remember to check my `completion-styles' and the
;; `completion-category-overrides'.
(setq orderless-matching-styles
      '(orderless-prefixes orderless-flex orderless-regexp))

;; SPC should never complete: use it for `orderless' groups.
;; The `?' is a regexp construct.
(let ((map minibuffer-local-completion-map))
  (define-key map (kbd "SPC") nil)
  (define-key map (kbd "?") nil))

(setq orderless-style-dispatchers
      '(prot-orderless-literal
        prot-orderless-file-ext
        prot-orderless-beg-or-end))

;;; Consult
;;; Enhanced minibuffer commands (consult.el)
(require 'consult)

(setq consult-line-numbers-widen t)
(setq consult-async-min-input 3)
(setq consult-async-input-debounce 0.5)
(setq consult-async-input-throttle 0.8)
(setq consult-narrow-key nil)
(setq register-preview-delay 0.8
      register-preview-function #'consult-register-format)
(setq consult-find-args "find . -not ( -path */.git* -prune )")
(setq consult-preview-key "M-v")

(add-to-list 'consult-mode-histories '(vc-git-log-edit-mode . log-edit-comment-ring))

(add-hook 'completion-list-mode-hook #'consult-preview-at-point-mode)

(require 'consult-imenu) ; the `imenu' extension is in its own file

(defun p-consult-line-at-point ()
  (interactive)
  (consult-line (thing-at-point 'symbol)))

(defun p-consult-ripgrep-at-point ()
  (interactive)
  (consult-ripgrep nil (thing-at-point 'symbol)))

(defun p-consult-ripgrep-dir ()
  (interactive)
  (consult-ripgrep (universal-argument)))

(define-key consult-narrow-map (kbd "?") #'consult-narrow-help)
(define-key minibuffer-local-map (kbd "C-s") #'consult-history)

(with-eval-after-load 'pulsar
  ;; see my `pulsar' package: <https://protesilaos.com/emacs/pulsar>
  (setq consult-after-jump-hook nil) ; reset it to avoid conflicts with my function
  (dolist (fn '(pulsar-recenter-top pulsar-reveal-entry))
    (add-hook 'consult-after-jump-hook fn)))

;;; Embark
;;; Extended minibuffer actions and more (embark.el and prot-embark.el)
(require 'embark)

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
    (call-interactively #'embark-act)))

(dolist (map (list global-map embark-collect-mode-map minibuffer-local-filename-completion-map))
  (define-key map (kbd "C-,") #'prot/embark-act-no-quit)
  (define-key map (kbd "C-.") #'prot/embark-act-quit))

;; NOTE 2023-03-15: I am working on making my Embark buffers easier
;; to read.  I am removing keys I do not use.  What follows is a
;; drastic measure.  I still need to test that it works as intended.
(seq-do
 (lambda (cell)
   (let* ((keymap (cdr-safe cell))
          (map (if (listp keymap) (car keymap) keymap)))
     (set map (make-sparse-keymap))))
 embark-keymap-alist)

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
   prot/embark-org-keymaps))

(prot-emacs-keybind embark-general-map
  "i" #'embark-insert
  "w" #'embark-copy-as-kill
  "E" #'embark-export
  "S" #'embark-collect
  "A" #'embark-act-all
  "DEL" #'delete-region)

(prot-emacs-keybind embark-url-map
  "b" #'browse-url
  "d" #'embark-download-url
  "e" #'eww)

(prot-emacs-keybind embark-buffer-map
  "k" #'prot-simple-kill-buffer
  "o" #'switch-to-buffer-other-window
  "e" #'ediff-buffers)

(add-to-list 'embark-post-action-hooks (list 'prot-simple-kill-buffer 'embark--restart))

(prot-emacs-keybind embark-file-map
  "f" #'find-file
  "j" #'embark-dired-jump
  "c" #'copy-file
  "e" #'ediff-files)

(prot-emacs-keybind embark-identifier-map
  "h" #'display-local-help
  "." #'xref-find-definitions
  "o" #'occur)

(prot-emacs-keybind embark-command-map
  "h" #'describe-command
  "." #'embark-find-definition)

(prot-emacs-keybind embark-expression-map
  "e" #'pp-eval-expression
  "m" #'pp-macroexpand-expression)

(prot-emacs-keybind embark-function-map
  "h" #'describe-function
  "." #'embark-find-definition)

(prot-emacs-keybind embark-symbol-map
  "h" #'describe-symbol
  "." #'embark-find-definition)

(prot-emacs-keybind embark-variable-map
  "h" #'describe-variable
  "." #'embark-find-definition)

(prot-emacs-keybind embark-region-map
  "a" #'align-regexp
  "D" #'delete-duplicate-lines
  "f" #'flush-lines
  "i" #'epa-import-keys-region
  "d" #'epa-decrypt-armor-in-region
  "r" #'repunctuate-sentences
  "s" #'sort-lines
  "u" #'untabify)

;; FIXME 2023-04-13: Why `embark-defun-map' has `embark-expression-map' as parent?
(set-keymap-parent embark-defun-map embark-expression-map)

(dolist (map '( embark-url-map embark-buffer-map embark-file-map
                embark-identifier-map embark-command-map embark-expression-map
                embark-function-map embark-symbol-map
                embark-variable-map embark-region-map))
  (set-keymap-parent (symbol-value map) embark-general-map))

;;; Marginalia
;;; Detailed completion annotations (marginalia.el)
(setq marginalia-max-relative-age 0) ; absolute time
(marginalia-mode 1)

(define-key minibuffer-local-map (kbd "C-k") 'delete-backward-char)
(define-key minibuffer-local-map (kbd "C-w") 'backward-kill-word)

(provide 'init-completion)
;;;;; init-completion.el ends here
