;;;;; init-avy.el --- Avy -*- lexical-binding: t -*-

;;; Install packages
(straight-use-package 'avy)

(require 'avy)

(setq avy-background t)
(setq avy-style 'at)
(setq avy-styles-alist '((avy-goto-line . pre)))
(setq avy-dispatch-alist nil)
;; https://karthinks.com/software/avy-can-do-anything/
(setq avy-keys '(?q ?e ?r ?y ?u ?o ?p ?a ?s ?d ?f ?g ?h ?j ?l ?x ?c ?v ?b ?n))

;;; Avy command
(global-set-key (kbd "C-c m") 'avy-goto-char-timer)

(defun p-avy-goto-word-current-line ()
  (interactive)
  (avy-with avy-goto-word-0
    (avy-goto-word-0 nil (line-beginning-position) (line-end-position))))

(defun p-avy-goto-word-block ()
  (interactive)
  (let ((block-beginning-position (progn (region-beginning) (line-beginning-position)))
        (block-end-position (progn (region-end) (line-end-position))))
    (avy-with avy-goto-word-0
      (avy-goto-word-0 nil block-beginning-position block-end-position))))

(defun p-avy-goto-bracket (&optional BEG END)
  (interactive)
  (let ((avy-command this-command))
    (avy-jump "\\((+\\|\\[+\\|)+\\|]+\\)" :beg BEG :end END)))
(add-to-list 'avy-orders-alist '(p-avy-goto-bracket . avy-order-closest))

(defun p-avy-goto-bracket-block ()
  (interactive)
  (let ((block-beginning-position (progn (region-beginning) (line-beginning-position)))
        (block-end-position (progn (region-end) (line-end-position))))
    (avy-with p-avy-goto-bracket
      (p-avy-goto-bracket block-beginning-position block-end-position))))

(defun p-avy-goto-space (&optional BEG END)
  (interactive)
  (let ((avy-command this-command))
    (avy-jump " " :beg BEG :end END)))
(add-to-list 'avy-orders-alist '(p-avy-goto-space . avy-order-closest))

(defun p-avy-goto-space-current-line ()
  (interactive)
  (avy-with p-avy-goto-space
    (p-avy-goto-space (line-beginning-position) (line-end-position))))

(defun p-avy-goto-equal (&optional BEG END)
  (interactive)
  (let ((avy-command this-command))
    (avy-jump "=" :beg BEG :end END)))
(add-to-list 'avy-orders-alist '(p-avy-goto-equal . avy-order-closest))

(defun p-avy-goto-equal-block ()
  (interactive)
  (let ((block-beginning-position (progn (region-beginning) (line-beginning-position)))
        (block-end-position (progn (region-end) (line-end-position))))
    (avy-with p-avy-goto-equal
      (p-avy-goto-equal block-beginning-position block-end-position))))

;;; Keybindings
(with-eval-after-load 'evil
  (define-key evil-normal-state-map (kbd "f") nil)
  (define-key evil-normal-state-map (kbd "ft") 'avy-goto-char-timer)
  (define-key evil-normal-state-map (kbd "fw") 'avy-goto-word-0)
  (define-key evil-normal-state-map (kbd "fl") 'avy-goto-line)
  (define-key evil-normal-state-map (kbd "fr") 'avy-copy-region)
  (define-key evil-normal-state-map (kbd "fc") 'avy-copy-line)
  (define-key evil-normal-state-map (kbd "f.") 'p-avy-goto-word-current-line)
  (define-key evil-normal-state-map (kbd "f,") 'p-avy-goto-word-block)
  (define-key evil-normal-state-map (kbd "fK") 'p-avy-goto-bracket)
  (define-key evil-normal-state-map (kbd "fk") 'p-avy-goto-bracket-block)
  (define-key evil-normal-state-map (kbd "fS") 'p-avy-goto-space)
  (define-key evil-normal-state-map (kbd "fs") 'p-avy-goto-space-current-line)
  (define-key evil-normal-state-map (kbd "fe") 'p-avy-goto-equal-block))

(provide 'init-avy)
;;;;; init-avy.el ends here
