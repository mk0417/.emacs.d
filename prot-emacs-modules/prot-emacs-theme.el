;;; Theme setup and related

;;;; Load the desired theme module
;; These all reference my packages: `modus-themes', `ef-themes',
;; `standard-themes'.
(pcase prot-emacs-load-theme-family
  ('ef (require 'prot-emacs-ef-themes))
  ('modus (require 'prot-emacs-modus-themes))
  ('standard (require 'prot-emacs-standard-themes)))

;;;; Pulsar
;; Read the pulsar manual: <https://protesilaos.com/emacs/pulsar>.
(prot-emacs-package pulsar
  (:install t)
  (:delay 1)
  (dolist (cmd '( narrow-to-page narrow-to-defun
                  narrow-to-region widen
                  logos-forward-page-dwim
                  logos-backward-page-dwim))
    (add-to-list 'pulsar-pulse-functions cmd))

  (setopt pulsar-pulse t
          pulsar-delay 0.055
          pulsar-iterations 10
          pulsar-face 'pulsar-blue
          pulsar-highlight-face 'pulsar-yellow)

  (pulsar-global-mode 1)

  ;; There are convenience functions/commands which pulse the line using
  ;; a specific colour: `pulsar-pulse-line-red' is one of them.
  (add-hook 'next-error-hook #'pulsar-pulse-line-red)

  (add-hook 'minibuffer-setup-hook #'pulsar-pulse-line-red)

  ;; pulsar does not define any key bindings.  This is just my personal
  ;; preference.  Remember to read the manual on the matter.  Evaluate:
  ;;
  ;; (info "(elisp) Key Binding Conventions")
  (prot-emacs-keybind global-map
    "C-x l" #'pulsar-pulse-line ; override `count-lines-page'
    "C-x L" #'pulsar-highlight-dwim)) ; or use `pulsar-highlight-line'

;;;; Lin
;; Read the lin manual: <https://protesilaos.com/emacs/lin>.
(prot-emacs-package lin
  (:install t)
  (:delay 1)
  ;; You can use this to live update the face:
  ;;
  ;; (customize-set-variable 'lin-face 'lin-green)
  ;;
  ;; Or `setopt' on Emacs 29: (setopt lin-face 'lin-yellow)
  ;;
  ;; I still prefer `setq' for consistency.
  (setq lin-face 'lin-magenta)

  (lin-global-mode 1)) ; applies to all `lin-mode-hooks'

;;;; Increase padding of windows/frames
;; Yet another one of my packages:
;; <https://protesilaos.com/codelog/2023-06-03-emacs-spacious-padding/>.
(prot-emacs-package spacious-padding
  (:install t)
  (:delay 1)

  ;; (spacious-padding-mode 1)

  (define-key global-map (kbd "<f8>") #'spacious-padding-mode))

;;;; Rainbow mode for colour previewing (rainbow-mode.el)
(prot-emacs-package rainbow-mode
  (:install t)
  (:delay 10)
  (setq rainbow-ansi-colors nil)
  (setq rainbow-x-colors nil)

  (defun prot/rainbow-mode-in-themes ()
    (when-let* ((file (buffer-file-name))
                ((derived-mode-p 'emacs-lisp-mode))
                ((string-match-p "-theme" file)))
      (rainbow-mode 1)))

  (add-hook 'emacs-lisp-mode-hook #'prot/rainbow-mode-in-themes)

  (define-key ctl-x-x-map "c" #'rainbow-mode)) ; C-x x c

;;;; Theme buffet
;; A package by Bruno Boal, that I am co-developing.  It cycles
;; through themes periodically.  Check its documentation for the
;; technicalities: <https://git.sr.ht/~bboal/theme-buffet>.
(prot-emacs-package theme-buffet
  (:install t)
  (:delay 1)
  (theme-buffet-modus-ef))

(provide 'prot-emacs-theme)
