;;;;; init-modeline.el --- Modeline -*- lexical-binding: t -*-

;; https://gitlab.com/protesilaos/dotfiles/-/blob/master/emacs/.emacs.d/prot-emacs-modules/prot-emacs-modeline.el
(setq mode-line-percent-position '(-3 "%p"))
;; (setq mode-line-position-column-line-format '(" %l,%c"))
(setq mode-line-defining-kbd-macro (propertize " Macro" 'face 'mode-line-emphasis))

(setq-default mode-line-format
              '("%e"
                mode-line-front-space
                mode-line-mule-info
                mode-line-client
                mode-line-modified
                mode-line-remote
                mode-line-frame-identification
                mode-line-buffer-identification
                "  "
                mode-line-position
                mode-line-modes
                "  "
                (vc-mode vc-mode)
                "  "
                mode-line-misc-info
                mode-line-end-spaces))

(provide 'init-modeline)
;;;;; init-modeline.el ends here
