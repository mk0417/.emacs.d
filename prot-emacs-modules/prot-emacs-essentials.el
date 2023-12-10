;;; Essential configurations
(prot-emacs-configure
  (:delay 1)

  ;; NOTE 2023-05-20: Normally those would not have to be `require'd
  ;; as every point of entry is autoloaded.  But Emacs does not have
  ;; an autoloads file for them, as they are not installed the usual
  ;; way.
  (require 'prot-common)
  (require 'prot-simple)
  (require 'prot-scratch)
  (require 'prot-pair)
  (require 'prot-comment)
  (require 'prot-prefix)

;;;; General settings and common custom functions (prot-simple.el)
  (setq blink-matching-paren nil)
  (setq delete-pair-blink-delay 0.1) ; Emacs28 -- see `prot-simple-delete-pair-dwim'
  (setq help-window-select t)
  (setq next-error-recenter '(4)) ; center of the window
  (setq find-library-include-other-files nil) ; Emacs 29
  (setq remote-file-name-inhibit-delete-by-moving-to-trash t) ; Emacs 30
  (setq remote-file-name-inhibit-auto-save t)                 ; Emacs 30
  (setq save-interprogram-paste-before-kill t)
  (setq mode-require-final-newline 'visit-save)
  (setq-default truncate-partial-width-windows nil)
  (setq eval-expression-print-length nil)
  (setq kill-do-not-save-duplicates t)
  (setq duplicate-line-final-position -1 ; both are Emacs 29
        duplicate-region-final-position -1)
  (setq scroll-error-top-bottom t)

  (setq prot-simple-date-specifier "%F")
  (setq prot-simple-time-specifier "%R %z")

  (setq prot-scratch-default-mode 'text-mode)

;;;; Comments (prot-comment.el)
  (setq comment-empty-lines t)
  (setq comment-fill-column nil)
  (setq comment-multi-line t)
  (setq comment-style 'multi-line)
  (setq-default comment-column 0)

  (setq prot-comment-comment-keywords '("TODO" "NOTE" "XXX" "REVIEW" "FIXME"))
  (setq prot-comment-timestamp-format-concise "%F")
  (setq prot-comment-timestamp-format-verbose "%F %T %z")

  ;; General commands
  (prot-emacs-keybind global-map
    "<insert>" nil
    "C-x C-z" nil
    "C-x C-c" nil ; avoid accidentally exiting Emacs
    "C-x C-c C-c" #'save-buffers-kill-emacs
    "C-h h" nil
    "M-`" nil
    "C-z" prot-prefix-map
    "<f2>" prot-prefix-map ; override that two-column gimmick
    "C-g" #'prot-simple-keyboard-quit-dwim
    "C-x ." #'prot-simple-goto-definition ; overrides `set-fill-prefix'
    "C-h ." #'prot-simple-describe-symbol ; overrides `display-local-help'
    "C-h F" #'describe-face ; overrides `Info-goto-emacs-command-node'
    "C-h K" #'describe-keymap ; overrides `Info-goto-emacs-key-command-node'
    "C-h c" #'describe-char ; overrides `describe-key-briefly'
    "C-M-SPC" #'prot-simple-mark-sexp   ; will be overriden by `expreg' if tree-sitter is available
    "C-c +" #'prot-simple-number-increment
    "C-c -" #'prot-simple-number-decrement
    ;; Commands for lines
    "M-o" #'delete-blank-lines   ; alias for C-x C-o
    "M-k" #'prot-simple-kill-line-backward
    "C-S-w" #'prot-simple-copy-line
    "C-S-d" #'duplicate-dwim ; Emacs 29
    "C-S-y" #'prot-simple-yank-replace-line-or-region
    "M-SPC" #'cycle-spacing
    "C-v" #'prot-simple-multi-line-below ; overrides `scroll-up-command'
    "M-v" #'prot-simple-multi-line-above ; overrides `scroll-down-command'
    "<C-return>" #'prot-simple-new-line-below
    "<C-S-return>" #'prot-simple-new-line-above
    ;; Commands for text insertion or manipulation
    "C-=" #'prot-simple-insert-date
    "C-<" #'prot-simple-escape-url-dwim
    ;; "C->" #'prot-simple-insert-line-prefix-dwim
    "C-'" #'prot-pair-insert
    "M-'" #'prot-pair-insert
    "M-\\" #'prot-pair-delete
    "M-z" #'zap-up-to-char ; NOT `zap-to-char'
    "M-Z" #'prot-simple-zap-to-char-backward
    "<C-M-backspace>" #'backward-kill-sexp
    "M-c" #'capitalize-dwim
    "M-l" #'downcase-dwim        ; "lower" case
    "M-u" #'upcase-dwim
    ;; Commands for object transposition
    "C-S-p" #'prot-simple-move-above-dwim
    "C-S-n" #'prot-simple-move-below-dwim
    "C-t" #'prot-simple-transpose-chars
    "C-x C-t" #'prot-simple-transpose-lines
    "C-S-t" #'prot-simple-transpose-paragraphs
    "C-x M-t" #'prot-simple-transpose-sentences
    "C-M-t" #'prot-simple-transpose-sexps
    "M-t" #'prot-simple-transpose-words
    ;; Commands for paragraphs
    "M-Q" #'prot-simple-unfill-region-or-paragraph
    ;; Commands for windows and pages
    "C-x O" #'prot-simple-other-windor-or-frame
    "C-x n k" #'prot-simple-delete-page-delimiters
    "C-x M-r" #'prot-simple-swap-window-buffers
    ;; Commands for buffers
    "M-=" #'count-words
    "<C-f2>" #'prot-simple-rename-file-and-buffer
    "C-x k" #'prot-simple-kill-buffer-current
    "C-x K" #'kill-buffer
    "M-s b" #'prot-simple-buffers-major-mode
    "M-s v" #'prot-simple-buffers-vc-root
    ;; Scratch buffer for major mode of choice
    "C-c s" #'prot-scratch-buffer
    ;; Comments
    "C-;" #'prot-comment
    "C-x C-;" #'prot-comment-timestamp-keyword)

  (prot-emacs-keybind prog-mode-map
    "C-M-d" #'up-list) ; confusing name for what looks like "down" to me

  ;; Keymap for buffers (Emacs28)
  (prot-emacs-keybind ctl-x-x-map
    "f" #'follow-mode  ; override `font-lock-update'
    "r" #'rename-uniquely
    "l" #'visual-line-mode)

;;;; Mouse and mouse wheel behaviour
  (setq mouse-autoselect-window t) ; complements the auto-selection of my tiling window manager

  ;; In Emacs 27+, use Control + mouse wheel to scale text.
  (setq mouse-wheel-scroll-amount
        '(1
          ((shift) . 5)
          ((meta) . 0.5)
          ((control) . text-scale))
        mouse-drag-copy-region nil
        make-pointer-invisible t
        mouse-wheel-progressive-speed t
        mouse-wheel-follow-mouse t)

  ;; Scrolling behaviour
  (setq-default scroll-preserve-screen-position t
                scroll-conservatively 1 ; affects `scroll-step'
                scroll-margin 0
                next-screen-context-lines 0)

  (mouse-wheel-mode 1)
  (define-key global-map (kbd "C-M-<mouse-3>") #'tear-off-window)

;;;; Repeatable key chords (repeat-mode)
  (setq repeat-on-final-keystroke t
        repeat-exit-timeout 5
        repeat-exit-key "<escape>"
        repeat-keep-prefix nil
        repeat-check-key t
        repeat-echo-function 'ignore
        ;; Technically, this is not in repeal.el, though it is the
        ;; same idea.
        set-mark-command-repeat-pop t)
  (repeat-mode 1)

;;;; Built-in bookmarking framework (bookmark.el)
  (setq bookmark-use-annotations nil)
  (setq bookmark-automatically-show-annotations nil)
  (setq bookmark-fringe-mark nil) ; Emacs 29 to hide bookmark fringe icon
  ;; Write changes to the bookmark file as soon as 1 modification is
  ;; made (addition or deletion).  Otherwise Emacs will only save the
  ;; bookmarks when it closes, which may never happen properly
  ;; (e.g. power failure).
  (setq bookmark-save-flag 1)

  (add-hook 'bookmark-bmenu-mode-hook #'hl-line-mode)

;;;; Auto revert mode
  (setq auto-revert-verbose t)
  (global-auto-revert-mode 1)

;;;; Delete selection
  (delete-selection-mode 1)

;;;; Tooltips (tooltip-mode)
  (setq tooltip-delay 0.5
        tooltip-short-delay 0.5
        x-gtk-use-system-tooltips nil
        tooltip-frame-parameters
        '((name . "tooltip")
          (internal-border-width . 10)
          (border-width . 0)
          (no-special-glyphs . t)))

  (autoload #'tooltip-mode "tooltip")
  (tooltip-mode 1)

;;;; TRAMP (remote connections)
  (setq tramp-connection-timeout (* 60 10)) ; seconds

;;;; Display current time
  (setq display-time-format " %a %e %b, %H:%M ")
  ;;;; Covered by `display-time-format'
  ;; (setq display-time-24hr-format t)
  ;; (setq display-time-day-and-date t)
  (setq display-time-interval 60)
  (setq display-time-default-load-average nil)

  ;; I don't need the load average and the mail indicator, so let this
  ;; be simple:
  (setq display-time-string-forms
        '((propertize
           (format-time-string display-time-format now)
           'face 'display-time-date-and-time
           'help-echo (format-time-string "%a %b %e, %Y" now))
          " "))

  (display-time-mode 1)

;;;; World clock (M-x world-clock)
  (setq display-time-world-list t)
  (setq zoneinfo-style-world-list ; M-x shell RET timedatectl list-timezones
        '(("America/Los_Angeles" "Los Angeles")
          ("America/Vancouver" "Vancouver")
          ("Canada/Pacific" "Canada/Pacific")
          ("America/Chicago" "Chicago")
          ("Brazil/Acre" "Rio Branco")
          ("America/New_York" "New York")
          ("Canada/Atlantic" "Canada/Atlantic")
          ("Brazil/East" "Brasília")
          ("UTC" "UTC")
          ("Europe/Lisbon" "Lisbon")
          ("Europe/Brussels" "Brussels")
          ("Europe/Athens" "Athens")
          ("Asia/Riyadh" "Riyadh")
          ("Asia/Tehran" "Tehran")
          ("Asia/Tbilisi" "Tbilisi")
          ("Asia/Yekaterinburg" "Yekaterinburg")
          ("Asia/Kolkata" "Kolkata")
          ("Asia/Singapore" "Singapore")
          ("Asia/Shanghai" "Shanghai")
          ("Asia/Seoul" "Seoul")
          ("Asia/Tokyo" "Tokyo")
          ("Asia/Vladivostok" "Vladivostok")
          ("Australia/Brisbane" "Brisbane")
          ("Australia/Sydney" "Sydney")
          ("Pacific/Auckland" "Auckland")))

  ;; All of the following variables are for Emacs 28
  (setq world-clock-list t)
  (setq world-clock-time-format "%R %z (%Z)	%A %d %B")
  (setq world-clock-buffer-name "*world-clock*") ; Placement handled by `display-buffer-alist'
  (setq world-clock-timer-enable t)
  (setq world-clock-timer-second 60)

;;;; `man' (manpages)
  (setq Man-notify-method 'pushy) ; does not obey `display-buffer-alist'

;;;; `proced' (process monitor, similar to `top')
  (setq proced-auto-update-flag t)
  (setq proced-enable-color-flag t) ; Emacs 29
  (setq proced-auto-update-interval 5)
  (setq proced-descend t)
  (setq proced-filter 'user)

;;;; Emacs server (allow emacsclient to connect to running session)
  ;; The "server" is functionally like the daemon, except it is run by
  ;; the first Emacs frame we launch.  When we close that frame, the
  ;; server is terminated.  Whereas the daemon remains active even if
  ;; all Emacs frames are closed.
  ;;
  ;; I experimented with the daemon for a while.  Emacs would crash
  ;; whenever I would encounter an error in some Lisp evaluation.
  ;; Whereas the server works just fine when I need to connect to it via
  ;; the emacsclient.
  (require 'server)
  (setq server-client-instructions nil)
  (unless (server-running-p)
    (server-start)))

;;; Substitute
;; Another package of mine... Video demo:
;; <https://protesilaos.com/codelog/2023-01-16-emacs-substitute-package-demo/>.
(prot-emacs-package substitute
  (:install t)
  (:delay 10)
  ;; Set this to non-nil to highlight all occurrences of the current
  ;; target.
  (setopt substitute-highlight t)

  ;; Set this to t if you want to always treat the letter casing
  ;; literally.  Otherwise each command accepts a `C-u' prefix
  ;; argument to do this on-demand.
  (setq substitute-fixed-letter-case nil)

  ;; Produce a message after the substitution that reports on what
  ;; happened.  It is a single line, like "Substituted `TARGET' with
  ;; `SUBSTITUTE' N times across the buffer.
  (add-hook 'substitute-post-replace-hook #'substitute-report-operation)

  ;; The mnemonic for the prefix is that M-# (or M-S-3) is close to
  ;; M-% (or M-S-5).
  (prot-emacs-keybind global-map
    "M-# s" #'substitute-target-below-point ; Forward motion like isearch (C-s)
    "M-# r" #'substitute-target-above-point ; Backward motion like isearch (C-r)
    "M-# d" #'substitute-target-in-defun    ; "defun" mnemonic
    "M-# b" #'substitute-target-in-buffer)) ; "buffer" mnemonic

;;; Mark syntactic constructs efficiently if tree-sitter is available (expreg)
(when (treesit-available-p)
  (prot-emacs-package expreg
    (:install t)
    (:delay 10)
    (defun prot/expreg-expand (n)
      "Expand to N syntactic units, defaulting to 1 if none is provided interactively."
      (interactive "p")
      (dotimes (_ n)
        (expreg-expand)))

    (defun prot/expreg-expand-dwim ()
      "Do-What-I-Mean `expreg-expand' to start with symbol or word.
If over a real symbol, mark that directly, else start with a
word.  Fall back to regular `expreg-expand'."
      (interactive)
      (let ((symbol (bounds-of-thing-at-point 'symbol)))
        (cond
         ((equal (bounds-of-thing-at-point 'word) symbol)
          (prot/expreg-expand 1))
         (symbol (prot/expreg-expand 2))
         (t (expreg-expand)))))

    ;; There is also an `expreg-contract' command, though I have no use for it.
    (define-key global-map (kbd "C-M-SPC") #'prot/expreg-expand-dwim))) ; overrides `mark-sexp'

;;; Visualise undo ring (`vundo')
(prot-emacs-package vundo
  (:install t)
  (:delay 30)
  (setq vundo-glyph-alist vundo-unicode-symbols)

  (define-key global-map (kbd "C-?") #'vundo) ; override `undo-redo'

  ;; Check: <https://github.com/casouri/vundo/pull/74>.
  (defvar prot/vundo-diff-buffer-window nil
    "Window object of `prot/vundo-diff-buffer'.")

  (defun prot/vundo-quit-diff-window ()
    "Quit `prot/vundo-diff-buffer-window' if it is live.
Assign this function to the `vundo-post-exit-hook'."
    (when (and prot/vundo-diff-buffer-window
               (window-live-p prot/vundo-diff-buffer-window))
      (quit-window nil prot/vundo-diff-buffer-window)
      (setq prot/vundo-diff-buffer-window nil)))

  (defun prot/vundo-diff-buffer (buffer)
    "Diff BUFFER with its underlying file, if possible.
Assign this to `vundo-after-undo-functions'.  BUFFER is provided
by that special hook."
    (when (buffer-file-name buffer)
      (with-current-buffer (window-buffer (diff-buffer-with-file buffer))
        (setq prot/vundo-diff-buffer-window (get-buffer-window)))))

  (add-hook 'vundo-after-undo-functions #'prot/vundo-diff-buffer)
  (add-hook 'vundo-post-exit-hook #'prot/vundo-quit-diff-window))

;;; Laptop settings
(prot-emacs-configure
  (:delay 10)
;;;; Show battery status on the mode line (battery.el
  (require 'battery)
  (setq battery-mode-line-format
        (cond
         ((eq battery-status-function #'battery-linux-proc-acpi)
	      "⏻%b%p%%,%d°C ")
	     (battery-status-function
	      "⏻%b%p%% ")))

  (display-battery-mode 1))

;;; Cursor appearance (cursory)
;; Read the manual: <https://protesilaos.com/emacs/cursory>.
(prot-emacs-package cursory
  (:install t)
  (:delay 1)
  (setq cursory-presets
        '((box
           :blink-cursor-interval 0.8)
          (box-no-blink
           :blink-cursor-mode -1)
          (bar
           :cursor-type (bar . 2)
           :blink-cursor-interval 0.5)
          (bar-no-other-window
           :inherit bar
           :cursor-in-non-selected-windows nil)
          (underscore
           :cursor-type (hbar . 3)
           :blink-cursor-blinks 50)
          (underscore-thin-other-window
           :inherit underscore
           :cursor-in-non-selected-windows (hbar . 1))
          (underscore-thick
           :cursor-type (hbar . 8)
           :blink-cursor-interval 0.3
           :blink-cursor-blinks 50
           :cursor-in-non-selected-windows (hbar . 3))
          (t ; the default values
           :cursor-type box
           :cursor-in-non-selected-windows hollow
           :blink-cursor-mode 1
           :blink-cursor-blinks 10
           :blink-cursor-interval 0.2
           :blink-cursor-delay 0.2)))

  ;; I am using the default values of `cursory-latest-state-file'.

  ;; Set last preset or fall back to desired style from `cursory-presets'.
  (cursory-set-preset (or (cursory-restore-latest-preset) 'box))

  ;; The other side of `cursory-restore-latest-preset'.
  (add-hook 'kill-emacs-hook #'cursory-store-latest-preset)

  ;; We have to use the "point" mnemonic, because C-c c is often the
  ;; suggested binding for `org-capture' and is the one I use as well.
  (define-key global-map (kbd "C-c p") #'cursory-set-preset))

(provide 'prot-emacs-essentials)
