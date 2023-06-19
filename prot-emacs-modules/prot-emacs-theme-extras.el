;;; Pulsar
;; Read the pulsar manual: <https://protesilaos.com/emacs/pulsar>.

(straight-use-package 'pulsar)
(straight-use-package 'lin)
(straight-use-package 'rainbow-mode)

(require 'pulsar)

(dolist (cmd '( narrow-to-page narrow-to-defun
                evil-goto-line evil-window-right evil-window-left
                evil-window-up evil-window-down
                evil-yank evil-delete
                scroll-up-command scroll-down-command
                recenter-top-bottom other-window
                narrow-to-region widen))
  (add-to-list 'pulsar-pulse-functions cmd))

(setopt pulsar-pulse t
        pulsar-delay 0.055
        pulsar-iterations 10
        pulsar-face 'pulsar-magenta
        pulsar-highlight-face 'pulsar-yellow)

(pulsar-global-mode 1)

;; There are convenience functions/commands which pulse the line using
;; a specific colour: `pulsar-pulse-line-red' is one of them.
(add-hook 'next-error-hook #'pulsar-pulse-line-red)

;; pulsar does not define any key bindings.  This is just my personal
;; preference.  Remember to read the manual on the matter.  Evaluate:
;;
;; (info "(elisp) Key Binding Conventions")
(prot-emacs-keybind global-map
  "C-x l" #'pulsar-pulse-line ; override `count-lines-page'
  "C-x L" #'pulsar-highlight-dwim) ; or use `pulsar-highlight-line'

;;; Lin
;; Read the lin manual: <https://protesilaos.com/emacs/lin>.
;; You can use this to live update the face:
;;
;; (customize-set-variable 'lin-face 'lin-green)
;;
;; Or `setopt' on Emacs 29: (setopt lin-face 'lin-yellow)
;;
;; I still prefer `setq' for consistency.
(setq prot-emacs-load-theme-family 'modus)
(setq lin-face
      (if (eq prot-emacs-load-theme-family 'modus)
          'lin-cyan
        'hl-line))

(setq lin-mode-hooks
      '(bongo-mode-hook
        dired-mode-hook
        elfeed-search-mode-hook
        git-rebase-mode-hook
        ibuffer-mode-hook
        ilist-mode-hook
        ledger-report-mode-hook
        log-view-mode-hook
        occur-mode-hook
        org-agenda-mode-hook
        tabulated-list-mode-hook
        vc-dir-mode-hook))

(lin-global-mode 1) ; applies to all `lin-mode-hooks'

;;; Rainbow mode for colour previewing (rainbow-mode.el)
(setq rainbow-ansi-colors nil)
(setq rainbow-x-colors nil)

(defun prot/rainbow-mode-in-themes ()
  (when-let* ((file (buffer-file-name))
              ((derived-mode-p 'emacs-lisp-mode))
              ((string-match-p "-theme" file)))
    (rainbow-mode 1)))

(add-hook 'emacs-lisp-mode-hook #'prot/rainbow-mode-in-themes)

(define-key ctl-x-x-map "c" #'rainbow-mode) ; C-x x c

(provide 'prot-emacs-theme-extras)
