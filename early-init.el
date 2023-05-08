;;;;; early-init.el --- Early init file -*- lexical-binding: t -*-

;;; Don't use package.el, use straight.el instead
(setq package-enable-at-startup nil)
(advice-add #'package--ensure-init-file :override #'ignore)

;;; Prefer loading newest compiled .el file
(setq load-prefer-newer t)

;;; Inhibit resizing frame
(setq frame-inhibit-implied-resize t)

;;; Make the initial buffer load faster by setting its mode to fundamental-mode
(setq initial-major-mode 'fundamental-mode)

(defvar p-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist p-file-name-handler-alist)))

;;; Increase the GC threshold for faster startup
(setq gc-cons-threshold most-positive-fixnum)
(setq gc-cons-percentage 0.6)

;;; Do not compact font cache
(setq inhibit-compacting-font-caches t)

;;; Setup straight as package manager
;; https://github.com/doomemacs/doomemacs/issues/5682
(defvar native-comp-deferred-compilation-deny-list nil)
(setq straight-repository-branch "develop")
;; (setq straight-vc-git-default-clone-depth 1)
(setq straight-vc-git-default-clone-depth '(1 single-branch))
;; quickier init time
;; https://emacs.stackexchange.com/questions/71302/reducing-straight-el-bloat
(setq straight-check-for-modifications '(check-on-save find-when-checking))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;;; Native compilation settings
(when (featurep 'native-compile)
  ;; silence compiler warnings as they can be pretty disruptive
  (setq native-comp-async-report-warnings-errors nil)
  ;; Make native compilation happens asynchronously
  (setq native-comp-deferred-compilation t)
  (setq compilation-scroll-output t))

;;; No titlebar
(add-to-list 'default-frame-alist '(undecorated . t))

;;; Maximize frame at startup
(setq initial-frame-alist (quote ((fullscreen . maximized))))

;;; Premature redisplays can substantially affect startup times and produce ugly flashes of unstyled Emacs
;; https://github.com/doomemacs/doomemacs/blob/master/early-init.el
(setq-default inhibit-redisplay t)
(add-hook 'window-setup-hook
          (lambda ()
            (setq-default inhibit-redisplay nil)
            (redisplay)))

;;; Minimal UI
(setq inhibit-startup-message t)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(horizontal-scroll-bars) default-frame-alist)
(setq inhibit-startup-buffer-menu t)

;;; Modus themes
;; Load in early-init.el to avoid white screen flash
;; https://gitlab.com/protesilaos/dotfiles/-/blob/master/emacs/.emacs.d/prot-emacs-modules/prot-emacs-modus-themes.el
(setq modus-themes-custom-auto-reload nil)
(setq modus-themes-mixed-fonts t)
(setq modus-themes-variable-pitch-ui nil)
(setq modus-themes-bold-constructs t)
(setq modus-themes-completions '((selection . (extrabold))))
(setq modus-themes-org-blocks 'gray-background)
(setq modus-themes-headings
      '((agenda-structure . (variable-pitch light 2.2))
        (agenda-date . (variable-pitch regular 1.3))
        (t . (regular 1.15))))

(setq modus-themes-common-palette-overrides
      '((fg-completion-match-0 fg-main)
        (fg-completion-match-1 fg-main)
        (fg-completion-match-2 fg-main)
        (bg-completion-match-0 bg-cyan-intense)
        (bg-completion-match-1 bg-green-intense)
        (bg-completion-match-2 bg-red-intense)
        (bg-completion bg-blue-nuanced)
        ;; Make line numbers less intense and add a shade of cyan
        ;; for the current line number.
        (fg-line-number-inactive "gray50")
        (fg-line-number-active cyan-cooler)
        (bg-line-number-inactive unspecified)
        (bg-line-number-active unspecified)
        ;; Make the current line of `hl-line-mode' a fine shade of gray
        (bg-hl-line bg-dim)
        ;; Make the region have a cyan-green background with no
        ;; specific foreground (use foreground of underlying text).
        ;; "bg-sage" refers to Salvia officinalis, else the common sage.
        (bg-region bg-sage)
        (fg-region unspecified)
        ;; Make matching parentheses a shade of magenta. It
        ;; complements the region nicely.
        (bg-paren-match bg-magenta-intense)
        ;; Change dates to a set of more subtle combinations.
        (date-deadline magenta-cooler)
        (date-scheduled green-cooler)
        (date-weekday fg-main)
        (date-event fg-dim)
        (date-now blue-faint)
        ;; Make tags (Org) less colorful and tables look the same as
        ;; the default foreground.
        (prose-done cyan-cooler)
        (prose-tag fg-dim)
        (prose-table fg-main)
        ;; Make headings less colorful
        (fg-heading-2 blue-faint)
        (fg-heading-3 magenta-faint)
        (fg-heading-4 blue-faint)
        (fg-heading-5 magenta-faint)
        (fg-heading-6 blue-faint)
        (fg-heading-7 magenta-faint)
        (fg-heading-8 blue-faint)
        ;; mode-line
        (bg-mode-line-inactive bg-dim)
        (border-mode-line-inactive bg-inactive)
        ;; Make the prompts a shade of magenta (rosy), to fit in
        ;; nicely with the overall blue-cyan-purple style of the
        ;; other overrides. Add a nuanced background as well.
        (bg-prompt bg-magenta-nuanced)
        (fg-prompt magenta-cooler)
        ;; Tweak some more constructs for stylistic constistency.
        (name blue-warmer)
        (identifier magenta-faint)
        (keybind magenta-cooler)))

(load-theme 'modus-vivendi t)

;;; Consistent color with theme
;; (when (featurep 'ns)
;;   (push '(ns-transparent-titlebar . t) default-frame-alist))

;;;;; early-init.el ends here
