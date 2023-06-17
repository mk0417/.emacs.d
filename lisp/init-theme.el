;;;;; init-theme.el --- Emacs theme -*- lexical-binding: t -*-

;;; Install package
(straight-use-package 'modus-themes)

;;; Modus themes
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
      '((bg-completion bg-blue-nuanced)

        ;; (fg-completion-match-0 fg-main)
        ;; (fg-completion-match-1 fg-main)
        ;; (fg-completion-match-2 fg-main)
        ;; (fg-completion-match-3 fg-main)
        ;; (bg-completion-match-0 bg-green-intense)
        ;; (bg-completion-match-1 bg-red-intense)
        ;; (bg-completion-match-2 bg-yellow-intense)
        ;; (bg-completion-match-3 bg-blue-intense)

        (fg-completion-match-0 green-cooler)
        (fg-completion-match-1 red-cooler)
        (fg-completion-match-2 yellow-cooler)
        (fg-completion-match-3 blue-cooler)
        (bg-completion-match-0 bg-green-subtle)
        (bg-completion-match-1 bg-red-subtle)
        (bg-completion-match-2 bg-yellow-subtle)
        (bg-completion-match-3 bg-blue-subtle)

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

(provide 'init-theme)
;;;;; init-theme.el ends here
