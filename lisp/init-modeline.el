;;;;; init-modeline.el --- Modeline -*- lexical-binding: t -*-

;; https://gitlab.com/protesilaos/dotfiles/-/blob/master/emacs/.emacs.d/prot-lisp/prot-modeline.el
;; https://gitlab.com/protesilaos/dotfiles/-/blob/master/emacs/.emacs.d/prot-emacs-modules/prot-emacs-modeline.el
(defface p-modeline-intense
  '((t :inherit (bold highlight)))
  "Face for intense mode line constructs.")

(defvar p-modeline-modes
  (list (propertize "%[" 'face 'error)
        `(:propertize ("" mode-name)
                      mouse-face mode-line-highlight
                      local-map ,mode-line-major-mode-keymap)
        '("" mode-line-process)
        (propertize "%]" 'face 'error)
        " ")
  "Mode line construct for displaying major modes.")

(defvar p-modeline-align-right
  '(:eval (propertize
           " " 'display
           `((space :align-to
                    (- (+ right right-fringe right-margin)
                       ,(string-width
                         (format-mode-line mode-line-misc-info)))))))
  "Mode line construct to align following elements to the right.")

(defvar p-modeline-misc-info
  '(:eval
    (when (mode-line-window-selected-p)
      mode-line-misc-info))
  "Mode line construct displaying `mode-line-misc-info'.
Specific to the current window's mode line.")

;;; Full path in mode-line
(setq-default mode-line-buffer-identification
              (list 'buffer-file-name
                    '(:eval (propertize (format "  %s" buffer-file-truename)))))

(setq mode-line-percent-position '(-3 "%p"))
(setq mode-line-position-column-line-format '("%l,%c"))
(setq mode-line-compact nil)

(setq-default mode-line-format
              '("%e"
                " "
                mode-line-mule-info
                mode-line-modified
                mode-line-remote
                " "
                mode-line-buffer-identification
                "  "
                mode-line-position
                p-modeline-modes
                " "
                (vc-mode vc-mode)
                " "
                p-modeline-align-right
                p-modeline-misc-info))

(add-hook 'after-init-hook #'column-number-mode)

(provide 'init-modeline)
;;;;; init-modeline.el ends here
