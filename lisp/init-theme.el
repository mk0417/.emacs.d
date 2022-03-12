;;;;; init-theme.el --- Defaults for themes -*- lexical-binding: t -*-

;;; package
(straight-use-package 'modus-themes)

;;; modus
(setq modus-themes-bold-constructs t)
(setq modus-themes-italic-constructs t)
(setq modus-themes-links '(faint bold background))
(setq modus-themes-prompts '(bold background))
(setq modus-themes-mode-line '(moody))
(setq modus-themes-fringes 'intense)
(setq modus-themes-hl-line '(intense))
(setq modus-themes-paren-match '(intense bold underline))
(setq modus-themes-region '(no-extend accented))
(setq modus-themes-completions '((matches . (background intense))))

(setq modus-themes-org-agenda
      '((header-block . (variable-pitch scale-title))
        (header-date . (grayscale bold-today))
        (scheduled . rainbow)))

(setq modus-themes-org-blocks 'gray-background)

(setq modus-themes-intense-markup t)

(setq modus-themes-headings
      '((1 . (variable-pitch 1.7))
        (2 . (variable-pitch 1.5))
        (3 . (variable-pitch 1.2))
        (t . (variable-pitch 1))))

(setq x-underline-at-descent-line t)

(modus-themes-load-themes)
(modus-themes-load-vivendi)

;;; Toggle themes
;; https://protesilaos.com/emacs/modus-themes#h:b40aca50-a3b2-4c43-be58-2c26fcd14237
(defun my-modus-themes-toggle ()
  "Toggle between `modus-operandi' and `modus-vivendi' themes.
This uses `enable-theme' instead of the standard method of
`load-theme'.  The technicalities are covered in the Modus themes
manual."
  (interactive)
  (pcase (modus-themes--current-theme)
    ('modus-operandi (progn (enable-theme 'modus-vivendi)
			    (disable-theme 'modus-operandi)))
    ('modus-vivendi (progn (enable-theme 'modus-operandi)
			   (disable-theme 'modus-vivendi)))
    (_ (error "No Modus theme is loaded; evaluate `modus-themes-load-themes' first"))))

(provide 'init-theme)
;;;;; init-theme.el ends here
