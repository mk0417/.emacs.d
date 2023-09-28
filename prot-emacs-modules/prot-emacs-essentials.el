;;; Essential configurations

;; NOTE 2023-05-20: Normally those would not have to be `require'd
;; as every point of entry is autoloaded.  But Emacs does not have
;; an autoloads file for them, as they are not installed the usual
;; way.
(require 'prot-common)
(require 'prot-simple)
(require 'prot-scratch)
(require 'prot-pair)
(require 'prot-prefix)
(require 'prot-comment)

;;;; General settings and common custom functions (prot-simple.el)
(setq delete-pair-blink-delay 0.15) ; Emacs28 -- see `prot-simple-delete-pair-dwim'
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
  "C-x C-c C-c" save-buffers-kill-emacs
  "C-h h" nil
  "M-`" nil
  "C-g" prot-simple-keyboard-quit-dwim
  "C-h ." prot-simple-describe-symbol ; overrides `display-local-help'
  "C-h F" describe-face ; overrides `Info-goto-emacs-command-node'
  "C-h K" describe-keymap ; overrides `Info-goto-emacs-key-command-node'
  "C-h c" describe-char ; overrides `describe-key-briefly'
  "C-c +" prot-simple-number-increment
  "C-c -" prot-simple-number-decrement
  ;; Commands for lines
  "M-o" delete-blank-lines   ; alias for C-x C-o
  "M-k" prot-simple-kill-line-backward
  "C-S-w" prot-simple-copy-line
  "C-S-d" duplicate-dwim ; Emacs 29
  "C-S-y" prot-simple-yank-replace-line-or-region
  "M-SPC" cycle-spacing
  "C-v" prot-simple-multi-line-below ; overrides `scroll-up-command'
  "M-v" prot-simple-multi-line-above ; overrides `scroll-down-command'
  "<C-return>" prot-simple-new-line-below
  "<C-S-return>" prot-simple-new-line-above
  ;; Commands for text insertion or manipulation
  "C-=" prot-simple-insert-date
  "C-<" prot-simple-escape-url-dwim
  "C->" prot-simple-insert-line-prefix-dwim
  "C-'" prot-pair-insert
  "M-'" prot-pair-insert
  "M-\\" prot-pair-delete
  "M-z" zap-up-to-char ; NOT `zap-to-char'
  "M-Z" prot-simple-zap-to-char-backward
  "<C-M-backspace>" backward-kill-sexp
  "M-c" capitalize-dwim
  "M-l" downcase-dwim        ; "lower" case
  "M-u" upcase-dwim
  ;; Commands for object transposition
  "C-S-p" prot-simple-move-above-dwim
  "C-S-n" prot-simple-move-below-dwim
  "C-t" prot-simple-transpose-chars
  "C-x C-t" prot-simple-transpose-lines
  "C-S-t" prot-simple-transpose-paragraphs
  "C-x M-t" prot-simple-transpose-sentences
  "C-M-t" prot-simple-transpose-sexps
  "M-t" prot-simple-transpose-words
  ;; Commands for marking objects
  "M-@" prot-simple-mark-word       ; replaces `mark-word'
  "C-M-SPC" prot-simple-mark-construct-dwim
  ;; Commands for paragraphs
  "M-Q" prot-simple-unfill-region-or-paragraph
  ;; Commands for windows and pages
  "C-x n k" prot-simple-delete-page-delimiters
  "C-x M-r" prot-simple-swap-window-buffers
  ;; Commands for buffers
  "M-=" count-words
  "<C-f2>" prot-simple-rename-file-and-buffer
  "C-x k" prot-simple-kill-buffer-current
  "C-x K" kill-buffer
  "M-s b" prot-simple-buffers-major-mode
  "M-s v" prot-simple-buffers-vc-root
  ;; Scratch buffer for major mode of choice
  "C-c s" prot-scratch-buffer
  ;; Prefix keymap (prot-prefix.el)
  "C-z" prot-prefix
  ;; Comments
  "C-;" prot-comment
  "C-x C-;" prot-comment-timestamp-keyword)

(prot-emacs-keybind prog-mode-map
  "C-M-d" up-list) ; confusing name for what looks like "down" to me

;; Keymap for buffers (Emacs28)
(prot-emacs-keybind ctl-x-x-map
  "f" follow-mode  ; override `font-lock-update'
  "r" rename-uniquely
  "l" visual-line-mode)

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

;;;; Display current time
(setq display-time-format " %a %e %b, %H:%M ")
  ;;;; Covered by `display-time-format'
;; (setq display-time-24hr-format t)
;; (setq display-time-day-and-date t)
(setq display-time-interval 60)
(setq display-time-default-load-average nil)
;; NOTE 2022-09-21: For all those, I have implemented my own solution
;; that also shows the number of new items, although it depends on
;; notmuch: the `notmuch-indicator' package.
(setq display-time-mail-directory nil)
(setq display-time-mail-function nil)
(setq display-time-use-mail-icon nil)
(setq display-time-mail-string nil)
(setq display-time-mail-face nil)

;; I don't need the load average and the mail indicator, so let this
;; be simple:
(setq display-time-string-forms
      '((if (and (not display-time-format) display-time-day-and-date)
	        (format-time-string "%a %b %e " now)
          "")
        (propertize
         (format-time-string (or display-time-format
			                     (if display-time-24hr-format "%H:%M" "%-I:%M%p"))
			                 now)
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
(setq server-client-instructions nil)
(server-start)

;;; Laptop settings
;;;; Show battery status on the mode line (battery.el
(require 'battery)

(setq battery-mode-line-format
      (cond
       ((eq battery-status-function #'battery-linux-proc-acpi)
        "⏻%b%p%%,%d°C ")
       (battery-status-function
        "⏻%b%p%% ")))

(display-battery-mode 1)

;;;; Configure suitable fonts for the laptop
(with-eval-after-load 'fontaine
  (add-to-list 'fontaine-presets
               '(laptop-regular
                 :default-family "Iosevka Comfy"
                 :default-height 120
                 :variable-pitch-family "Iosevka Comfy Duo"))
  (fontaine-set-preset 'laptop-regular))

(provide 'prot-emacs-essentials)
