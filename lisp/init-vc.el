;;;;; init-vc.el --- Version control -*- lexical-binding: t -*-

(use-package diff-hl
  :ensure t
  :defer
  :init
  (setq diff-hl-draw-borders t)
  (setq-default diff-hl-inline-popup--height 4)
  (add-hook 'prog-mode-hook #'diff-hl-mode)
  :bind
  (:map diff-hl-command-map
        ("n" . diff-hl-next-hunk)
        ("p" . diff-hl-previous-hunk)
        ("[" . nil)
        ("]" . nil)
        ("DEL"   . diff-hl-revert-hunk)
        ("<delete>" . diff-hl-revert-hunk)
        ("SPC" . diff-hl-mark-hunk)
        :map vc-prefix-map
        ("n" . diff-hl-next-hunk)
        ("p" . diff-hl-previous-hunk)
        ("s" . diff-hl-stage-dwim)
        ("DEL"   . diff-hl-revert-hunk)
        ("<delete>" . diff-hl-revert-hunk)
        ("SPC" . diff-hl-mark-hunk))
  :config
  (setq-default fringes-outside-margins t)
  (put 'diff-hl-inline-popup-hide 'repeat-map 'diff-hl-command-map)
  (advice-add 'diff-hl-next-hunk :after
              (defun p-diff-hl-recenter (&optional _) (recenter)))
  (diff-hl-flydiff-mode 1))

(provide 'init-vc)
