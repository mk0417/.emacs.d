;;; init-theme.el --- Defaults for themes -*- lexical-binding: t -*-

;; package
(straight-use-package 'modus-themes)

;; modus
(setq modus-themes-bold-constructs t)
(setq modus-themes-italic-constructs t)
(setq modus-themes-links '(faint bold background))
(setq modus-themes-prompts '(bold background))
(setq modus-themes-mode-line '(moody))
(setq modus-themes-fringes 'intense)
(setq modus-themes-hl-line '(intense))
(setq modus-themes-paren-match '(intense bold underline))
(setq modus-themes-region '(no-extend accented))
(setq modus-themes-completions '((t extrabold background intense accented)))

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
