;;; General window and buffer configurations

(require 'prot-sideline)

;;;; `uniquify' (unique names for buffers)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-strip-common-suffix t)
(setq uniquify-after-kill-buffer-p t)

;;;; `window', `display-buffer-alist', and related
(require 'prot-window)

;; NOTE 2023-03-17: Remember that I am using development versions of
;; Emacs.  Some of my `display-buffer-alist' contents are for Emacs
;; 29+.
(setq display-buffer-alist
      `(;; no window
        ("\\`\\*Async Shell Command\\*\\'"
         (display-buffer-no-window))
        ;; bottom side window
        ("\\*Org Select\\*" ; the `org-capture' key selection
         (display-buffer-in-side-window)
         (dedicated . t)
         (side . bottom)
         (slot . 0)
         (window-parameters . ((mode-line-format . none))))
        ;; bottom buffer (NOT side window)
        ((or . ((derived-mode . messages-buffer-mode)
                (derived-mode . backtrace-mode)
                "\\*\\(Warnings\\|Compile-Log\\|Org Links\\)\\*"
                ,world-clock-buffer-name))
         (display-buffer-reuse-mode-window display-buffer-at-bottom)
         (window-height . 0.3)
         (dedicated . t)
         (preserve-size . (t . t)))
        ("\\*Embark Actions\\*"
         (display-buffer-reuse-mode-window display-buffer-at-bottom)
         (window-height . fit-window-to-buffer)
         (window-parameters . ((no-other-window . t)
                               (mode-line-format . none))))
        ("\\*\\(Output\\|Register Preview\\).*"
         (display-buffer-reuse-mode-window display-buffer-at-bottom))
        ;; below current window
        ((derived-mode . help-mode) ; See the hooks for `visual-line-mode'
         (display-buffer-reuse-mode-window display-buffer-below-selected))
        ("\\*\\vc-\\(incoming\\|outgoing\\|git : \\).*"
         (display-buffer-reuse-mode-window display-buffer-below-selected)
         (window-height . 0.1)
         (dedicated . t)
         (preserve-size . (t . t)))
        ((derived-mode . log-view-mode)
         (display-buffer-reuse-mode-window display-buffer-below-selected)
         (window-height . 0.3)
         (dedicated . t)
         (preserve-size . (t . t)))
        ((derived-mode . reb-mode) ; M-x re-builder
         (display-buffer-reuse-mode-window display-buffer-below-selected)
         (window-height . 4) ; note this is literal lines, not relative
         (dedicated . t)
         (preserve-size . (t . t)))
        ((or . ((derived-mode . occur-mode)
                (derived-mode . Buffer-menu-mode)
                "\\*\\(|Buffer List\\|Occur\\).*"
                prot-window-shell-or-term-p))
         (display-buffer-reuse-mode-window display-buffer-below-selected)
         (dedicated . t)
         (body-function . prot-window-select-fit-size))
        ("\\*\\(Calendar\\|Bookmark Annotation\\|ert\\|Embark Collect\\).*"
         (display-buffer-reuse-mode-window display-buffer-below-selected)
         (dedicated . t)
         (window-height . fit-window-to-buffer))
        ;; NOTE 2022-09-10: The following is for `ispell-word', though
        ;; it only works because I override `ispell-display-buffer'
        ;; with `prot-spell-ispell-display-buffer' and change the
        ;; value of `ispell-choices-buffer'.
        ("\\*ispell-top-choices\\*.*"
         (display-buffer-reuse-mode-window display-buffer-below-selected)
         (window-height . fit-window-to-buffer))
        ;; same window

        ;; NOTE 2023-02-17: `man' does not fully obey the
        ;; `display-buffer-alist'.  It works for new frames and for
        ;; `display-buffer-below-selected', but otherwise is
        ;; unpredictable.  See `Man-notify-method'.
        ((or . ((derived-mode . Man-mode)
                (derived-mode . woman-mode)
                "\\*\\(Man\\|woman\\).*"))
         (display-buffer-same-window))))

(setq window-combination-resize t)
(setq even-window-sizes 'height-only)
(setq window-sides-vertical nil)
(setq switch-to-buffer-in-dedicated-window 'pop)

(dolist (hook '(epa-info-mode-hook help-mode-hook custom-mode-hook))
  (add-hook hook #'visual-line-mode))

;; NOTE 2022-09-17: Also see `prot-simple-swap-window-buffers'.
(prot-emacs-keybind global-map
  "C-x <down>" #'next-buffer
  "C-x <up>" #'previous-buffer
  "C-x C-n" #'next-buffer     ; override `set-goal-column'
  "C-x C-p" #'previous-buffer ; override `mark-page'
  "C-x !" #'delete-other-windows-vertically
  "C-x _" #'balance-windows      ; underscore
  "C-x -" #'fit-window-to-buffer ; hyphen
  "C-x +" #'balance-windows-area
  "C-x }" #'enlarge-window
  "C-x {" #'shrink-window
  "C-x >" #'enlarge-window-horizontally ; override `scroll-right'
  "C-x <" #'shrink-window-horizontally) ; override `scroll-left'
(prot-emacs-keybind resize-window-repeat-map
  ">" #'enlarge-window-horizontally
  "<" #'shrink-window-horizontally)

;;; Line numbers and relevant indicators (prot-sideline.el)
(require 'display-line-numbers)
;; Set absolute line numbers.  A value of "relative" is also useful.
(setq display-line-numbers-type t)
;; Those two variables were introduced in Emacs 27.1
(setq display-line-numbers-major-tick 0)
(setq display-line-numbers-minor-tick 0)
;; Use absolute numbers in narrowed buffers
(setq-default display-line-numbers-widen t)

(require 'hl-line)
(setq hl-line-sticky-flag nil)
(setq hl-line-overlay-priority -50) ; emacs28

(require 'whitespace)

(prot-emacs-keybind global-map
  "<f6>" #'prot-sideline-negative-space-toggle
  "<f7>" #'prot-sideline-mode
  "C-c z" #'delete-trailing-whitespace)

(provide 'prot-emacs-window)
