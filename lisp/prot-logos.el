;;; prot-logos.el --- Extensions for my dotemacs to help read, write, present -*- lexical-binding: t -*-
;; Copyright (C) 2020-2021  Protesilaos Stavrou
;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; URL: https://protesilaos.com/emacs/dotemacs

(defgroup prot-logos ()
  "Setup for reading and presenting text-heavy buffers."
  :group 'files)

(defcustom prot-logos-org-presentation nil
  "Org files should switch to presentation view.
This concerns cases where variable `prot-logos-focus-mode' is set
to non-nil and determines whether headings should be converted
into pseudo slides and indentation be adjusted accordingly."
  :type 'boolean
  :group 'prot-logos)

(defcustom prot-logos-variable-pitch nil
  "Non-programming buffers should switch to `variable-pitch-mode'.
In programming modes the default font is always used, as that is
assumed to be a monospaced typeface."
  :type 'boolean
  :group 'prot-logos)

(defcustom prot-logos-scroll-lock nil
  "Use centred scrolling while in focused view."
  :type 'boolean
  :group 'prot-logos)

(defcustom prot-logos-hidden-modeline nil
  "Hide the modeline."
  :type 'boolean
  :group 'prot-logos)

(defvar prot-logos--focus-mode-hook nil
  "Hook that runs from function `prot-logos-focus-mode'.")

;;;###autoload
(define-minor-mode prot-logos-focus-mode
  "Buffer-local wrapper mode for presentations.
Other tools should hook into `prot-logos--focus-mode-hook' to
introduce their effects.  Otherwise this minor mode has no effect
on its own."
  :init-value nil
  :global nil
  :lighter " -Λ-"           ; greek lambda majuscule
  (run-hooks 'prot-logos--focus-mode-hook))

(autoload 'buffer-face-mode "face-remap")
(autoload 'variable-pitch-mode "face-remap")

(defun prot-logos--variable-pitch-toggle ()
  "Make text use `variable-pitch' face, except for programming."
  (when (and prot-logos-variable-pitch
             (derived-mode-p 'text-mode))
    (if (or (bound-and-true-p buffer-face-mode)
            (not (bound-and-true-p prot-logos-focus-mode)))
        (variable-pitch-mode -1)
      (variable-pitch-mode 1))))

(add-hook 'prot-logos--focus-mode-hook #'prot-logos--variable-pitch-toggle)

(autoload 'olivetti-mode "olivetti")

(defun prot-logos--olivetti-toggle ()
  "Toggle the variable `olivetti-mode', if available."
  (if (or (bound-and-true-p olivetti-mode)
          (not (bound-and-true-p prot-logos-focus-mode)))
      (olivetti-mode -1)
    (olivetti-mode 1)))

(add-hook 'prot-logos--focus-mode-hook #'prot-logos--olivetti-toggle)

(defface prot-logos-fringe
  `((((class color) (background light))
     :background "#ffffff")
    (((class color) (background dark))
     :background "#000000")
    (t :background ,(face-attribute 'default :background)))
  "Face to remove background from fringes.
Only tested with the Modus themes.")

(defvar-local prot-logos--fringe-cookie nil
  "Cookie returned by `face-remap-add-relative'.")

(declare-function face-remap-add-relative "face-remap" (face &rest specs))
(declare-function face-remap-remove-relative "face-remap" (cookie))

(defun prot-logos--fringe-toggle ()
  "Toggle fringe visibility."
  (if (bound-and-true-p prot-logos-focus-mode)
      (progn
        (set-window-fringes (selected-window) 0 0)
        (setq prot-logos--fringe-cookie
              (face-remap-add-relative 'olivetti-fringe 'prot-logos-fringe)))
    (set-window-fringes (selected-window) nil)
    (face-remap-remove-relative prot-logos--fringe-cookie)))

(add-hook 'prot-logos--focus-mode-hook #'prot-logos--fringe-toggle)

(autoload 'org-tree-slide-mode "org-tree-slide")

(defun prot-logos--org-tree-slide-mode ()
  "Toggle variable `org-tree-slide-mode' if loaded and needed."
  (let* ((buf (window-buffer (get-mru-window)))
         (mode (with-current-buffer buf major-mode)))
    (when (and prot-logos-org-presentation
               (eq mode 'org-mode))
      (if (or (bound-and-true-p org-tree-slide-mode)
              (not (bound-and-true-p prot-logos-focus-mode)))
          (org-tree-slide-mode -1)
        (org-tree-slide-mode 1)))))

(add-hook 'prot-logos--focus-mode-hook #'prot-logos--org-tree-slide-mode)

(autoload 'org-indent-mode "org")

(defun prot-logos--org-indent-mode ()
  "Toggle variable `org-tree-slide-mode' if loaded and needed."
  (let* ((buf (window-buffer (get-mru-window)))
         (mode (with-current-buffer buf major-mode)))
    (when (and prot-logos-org-presentation
               (eq mode 'org-mode))
      (if (or (bound-and-true-p org-indent-mode)
              (not (bound-and-true-p prot-logos-focus-mode)))
          (org-indent-mode -1)
        (org-indent-mode 1)))))

(add-hook 'prot-logos--focus-mode-hook #'prot-logos--org-indent-mode)

(defun prot-logos--scroll-lock ()
  "Keep the point at the centre."
  (when prot-logos-scroll-lock
    (if (or (bound-and-true-p scroll-lock-mode)
            (not (bound-and-true-p prot-logos-focus-mode)))
        (scroll-lock-mode -1)
      (recenter nil)
      (scroll-lock-mode 1))))

(add-hook 'prot-logos--focus-mode-hook #'prot-logos--scroll-lock)

;; Based on Paul W. Rankin's code:
;; https://gist.github.com/rnkn/a522429ed7e784ae091b8760f416ecf8
(defun prot-logos--hidden-modeline ()
  "Toggle mode line visibility."
  (when prot-logos-hidden-modeline
    (if (or (null mode-line-format)
            (not (bound-and-true-p prot-logos-focus-mode)))
        (kill-local-variable 'mode-line-format)
      (setq-local mode-line-format nil)
      (force-mode-line-update))))

(add-hook 'prot-logos--focus-mode-hook #'prot-logos--hidden-modeline)

(provide 'prot-logos)
;;; prot-logos.el ends here
