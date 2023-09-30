;;;;; init-evil.el --- Evil mode configuration -*- lexical-binding: t -*-

;;; Install packages
(straight-use-package 'evil-surround)

;; (setq evil-want-C-i-jump nil)
;; (setq evil-respect-visual-line-mode t)
;;
;; (evil-mode 1)

;;; Make evil search more like vim
;; (evil-select-search-module 'evil-search-module 'evil-search)

;;; Make sure some modes start in Emacs state
;; (dolist
;;     (mode
;;      '(custom-mode eshell-mode term-mode xref--xref-buffer-mode lsp-bridge-ref-mode occur-mode occur-edit-mode grep-mode color-rg-mode))
;;   (add-to-list 'evil-emacs-state-modes mode))

;;; Set evil normal state for grep mode
;; (dolist (mode '(dired-mode-map debugger-mode))
;;   (evil-set-initial-state mode 'normal))

;;; Change cursor type and color
;; https://github.com/hlissner/doom-emacs/issues/1848
;; (setq evil-normal-state-cursor '(box "#cf5a65"))
;; (setq evil-insert-state-cursor '(hbar "#cf5a65"))
;; (setq evil-visual-state-cursor '(hollow "#cf5a65"))

;; curosr in minibuffer
;; (add-hook 'minibuffer-setup-hook
;;           (lambda ()
;;             (setq-local cursor-type 'hbar)
;;             (set-face-attribute 'cursor nil :background "#cf5a65")))

;;; Evil surround
(global-evil-surround-mode 1)

;;; Vim style replace
(defun p-ex-evil-buffer-replace ()
  (interactive)
  (evil-ex (concat "%s/")))

(defun p-ex-evil-selection-replace ()
  (interactive)
  (evil-ex (concat "'<,'>s/")))

(defun p-ex-evil-selection-replace-yank ()
  (interactive)
  (evil-ex (concat "'<,'>s/" (substring-no-properties (car kill-ring)) "/")))

(defun p-ex-evil-replace-yank ()
  (interactive)
  (kill-new (thing-at-point 'symbol))
  (p-mark-paragraph)
  ;; make sure to run evil ex after above functions
  ;; https://emacs.stackexchange.com/questions/11003/run-a-function-after-control-returns-to-the-command-loop
  (run-with-timer 0 nil 'p-ex-evil-selection-replace-yank))

(defun p-ex-evil-replace-yank-def ()
  (interactive)
  (kill-new (thing-at-point 'symbol))
  (p-mark-defun)
  (run-with-timer 0 nil 'p-ex-evil-selection-replace-yank))

;;; Keybindings
(with-eval-after-load 'evil
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (define-key evil-normal-state-map (kbd "C-.") nil)
  (define-key evil-normal-state-map (kbd "C-r") nil)
  (define-key evil-normal-state-map (kbd "C-n") nil)
  (define-key evil-normal-state-map (kbd "C-p") nil)
  (define-key evil-insert-state-map (kbd "C-o") nil)
  (define-key evil-insert-state-map (kbd "C-n") nil)
  (define-key evil-insert-state-map (kbd "C-p") nil)
  (define-key evil-motion-state-map (kbd "C-e") nil)
  (define-key evil-motion-state-map (kbd "C-o") nil)

  (define-key evil-inner-text-objects-map "d" 'evil-inner-double-quote)
  (define-key evil-inner-text-objects-map "s" 'evil-inner-single-quote)
  (define-key evil-inner-text-objects-map "f" 'evil-inner-bracket)
  (define-key evil-inner-text-objects-map "h" 'evil-inner-curly)
  (define-key evil-inner-text-objects-map "a" 'evil-inner-angle)

  (define-key evil-outer-text-objects-map "d" 'evil-a-double-quote)
  (define-key evil-outer-text-objects-map "s" 'evil-a-single-quote)
  (define-key evil-outer-text-objects-map "f" 'evil-a-bracket)
  (define-key evil-outer-text-objects-map "h" 'evil-a-curly)
  (define-key evil-outer-text-objects-map "a" 'evil-an-angle))

(provide 'init-evil)
;;;;; init-evil.el ends here
