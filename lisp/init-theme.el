;;; init-theme.el --- Defaults for themes -*- lexical-binding: t -*-

;; package
(straight-use-package 'modus-themes)

;; modus
(setq modus-themes-bold-constructs t
      modus-themes-italic-constructs t
      modus-themes-links '(faint bold background)
      modus-themes-prompts '(bold background)
      modus-themes-mode-line '(moody)
      modus-themes-completions 'opinionated
      modus-themes-fringes 'intense
      modus-themes-hl-line '(intense)
      modus-themes-paren-match '(intense bold underline)
      modus-themes-region '(no-extend accented))

(setq modus-themes-org-agenda
      '((header-block . (variable-pitch scale-title))
        (header-date . (grayscale bold-today))
        (scheduled . rainbow)))

(setq modus-themes-org-blocks 'gray-background)

(setq modus-themes-intense-markup t)

(setq modus-themes-headings
      '((1 . (background overline variable-pitch 1.7))
        (2 . (background overline variable-pitch 1.5))
        (3 . (background overline variable-pitch 1.2))
        (t . (background overline variable-pitch 1))))

(setq x-underline-at-descent-line t)

(modus-themes-load-themes)
(modus-themes-load-vivendi)

(provide 'init-theme)
;;; init-theme.el ends here
