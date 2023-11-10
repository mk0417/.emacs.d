;;; The Modus themes

;; The themes are highly customisable.  Read the manual:
;; <https://protesilaos.com/emacs/modus-themes>.
(prot-emacs-package modus-themes
  (:install t)
  (:delay 1)
  (setq modus-themes-custom-auto-reload nil
        modus-themes-to-toggle '(modus-operandi modus-vivendi)
        ;; modus-themes-to-toggle '(modus-operandi-tinted modus-vivendi-tinted)
        ;; modus-themes-to-toggle '(modus-operandi-deuteranopia modus-vivendi-deuteranopia)
        ;; modus-themes-to-toggle '(modus-operandi-tritanopia modus-vivendi-tritanopia)
        modus-themes-mixed-fonts t
        modus-themes-variable-pitch-ui nil
        modus-themes-italic-constructs t
        modus-themes-bold-constructs nil
        modus-themes-org-blocks nil
        modus-themes-completions '((t . (extrabold)))
        modus-themes-prompts '(extrabold)
        modus-themes-headings
        '((agenda-structure . (variable-pitch light 2.2))
          (agenda-date . (variable-pitch regular 1.3))
          (t . (regular 1.15))))

  (setq modus-themes-common-palette-overrides
        `((cursor cyan-intense)
          (bg-region bg-ochre)
          (fg-region unspecified)
          (bg-paren-match bg-blue-intense)
          (bg-mode-line-active bg-lavender)
          (border-mode-line-active magenta-cooler)
          (border-mode-line-inactive border)
          (bg-hl-line bg-dim)
          (bg-line-number-active bg-hl-line)
          (bg-line-number-inactive unspecified)
          (fg-line-number-active fg-main)
          (bg-prompt bg-blue-nuanced)
          (fg-prompt blue-warmer)
          ;; ,@modus-themes-preset-overrides-warmer
          (fg-completion-match-0 green-cooler)
          (fg-completion-match-1 red-cooler)
          (fg-completion-match-2 yellow-cooler)
          (fg-completion-match-3 blue-cooler)
          (bg-completion-match-0 bg-green-subtle)
          (bg-completion-match-1 bg-red-subtle)
          (bg-completion-match-2 bg-yellow-subtle)
          (bg-completion-match-3 bg-blue-subtle)
          ))

  (modus-themes-load-theme (cadr modus-themes-to-toggle))

  ;; Also check `modus-themes-select'.
  (define-key global-map (kbd "<f5>") #'modus-themes-toggle))

;; ;; NOTE: For testing purposes
;; (progn
;;   (mapc #'disable-theme custom-enabled-themes)
;;
;;   (add-to-list 'load-path "/home/prot/Git/Projects/modus-themes/")
;;
;;   (require 'modus-themes)
;;
;;   (setq modus-themes-custom-auto-reload nil
;;         ;; modus-themes-to-toggle '(modus-operandi modus-vivendi)
;;         ;; modus-themes-to-toggle '(modus-operandi-tinted modus-vivendi-tinted)
;;         ;; modus-themes-to-toggle '(modus-operandi-deuteranopia modus-vivendi-deuteranopia)
;;         modus-themes-to-toggle '(modus-operandi-tritanopia modus-vivendi-tritanopia)
;;         modus-themes-mixed-fonts t
;;         modus-themes-variable-pitch-ui nil
;;         modus-themes-italic-constructs t
;;         modus-themes-bold-constructs nil
;;         modus-themes-org-blocks nil
;;         modus-themes-completions '((t . (extrabold)))
;;         modus-themes-prompts nil
;;         modus-themes-headings
;;         '((agenda-structure . (variable-pitch light 2.2))
;;           (agenda-date . (variable-pitch regular 1.3))
;;           (t . (regular 1.15))))
;;
;;   ;; (setq modus-themes-common-palette-overrides nil)
;;
;;   (setq modus-themes-common-palette-overrides
;;         `((fringe unspecified)
;;           ;; (bg-mode-line-active bg-lavender)
;;           ;; (border-mode-line-active unspecified)
;;           ;; (border-mode-line-inactive unspecified)
;;           (bg-line-number-active bg-hl-line)
;;           (bg-line-number-inactive unspecified)
;;           (fg-line-number-active fg-main)
;;           ;; ,@modus-themes-preset-overrides-warmer
;;           ))
;;
;;   ;; ;; Make the active mode line have a pseudo 3D effect (this assumes
;;   ;; ;; you are using the default mode line and not an extra package).
;;   ;; (custom-set-faces
;;   ;;  '(mode-line ((t :box (:style unspecified)))))
;;
;;   (if (prot-emacs-theme-environment-dark-p)
;;       (modus-themes-load-theme (cadr modus-themes-to-toggle))
;;     (modus-themes-load-theme (car modus-themes-to-toggle)))
;;
;;   ;; Also check `modus-themes-select'.  To list the palette's colours,
;;   ;; use `modus-themes-list-colors', `modus-themes-list-colors-current'.
;;   (define-key global-map (kbd "<f5>") #'modus-themes-toggle))

(provide 'prot-emacs-modus-themes)
