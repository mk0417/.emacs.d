;;;;; init-text.el --- Text -*- lexical-binding: t -*-

;;; package
(straight-use-package 'key-chord)
(straight-use-package 'smartparens)
(straight-use-package 'cycle-at-point)

;;; select a block
(defun p-select-block ()
  (interactive)
  (if (region-active-p)
      (re-search-forward "\n[ \t]*\n[ \t]*\n*" nil 1)
    (progn
      (skip-chars-forward " \n\t")
      (when (re-search-backward "\n[ \t]*\n" nil 1)
	(goto-char (match-end 0)))
      (push-mark (point) t t)
      (re-search-forward "\n[ \t]*\n" nil 1)
      (previous-line)
      (end-of-line))))

;;; go to beginning of block
(defun p-beginning-of-block ()
  (interactive)
  (skip-chars-forward " \n\t")
  (when (re-search-backward "\n[ \t]*\n" nil 1)
    (goto-char (match-end 0))))

;;; go to end of block
(defun p-end-of-block ()
  (interactive)
  (re-search-forward "\n[ \t]*\n" nil 1)
  (previous-line)
  (end-of-line))

;;; insert date
(defun p-insert-uk-date ()
  (interactive)
  (insert (format-time-string "%d-%m-%Y")))

(defun p-insert-date ()
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))

(defun p-insert-date-alt ()
  (interactive)
  (insert (format-time-string "%Y-%m-%d, %A")))

;;; insert current buffer name
(defun p-insert-file-name ()
  (interactive)
  (insert (buffer-file-name)))

;;; backward kill to the beginning of line
(defun p-kill-to-begin-of-line ()
  (interactive)
  (kill-line 0))

;;; delete to tab
(defun p-delete-backward-to-tab ()
  (interactive)
  (when (evil-normal-state-p)
    (beginning-of-line-text)
    (kill-line 0)
    (insert "    "))
  (when (evil-insert-state-p)
    (kill-line 0)
    (insert "    ")))

;;; clear line
(defun p-clear-line ()
  (interactive)
  (beginning-of-line)
  (kill-line))

;;; add four spaces
(defun p-insert-spaces ()
  (interactive)
  (insert "    "))

;;; add my daily log item
(defun p-insert-log-item ()
  (interactive)
  (insert "** ##"))

;;; key-chord typing
(add-hook 'after-init-hook 'key-chord-mode)
(setq key-chord-two-keys-delay 0.3)

;;; define my key chord
(with-eval-after-load 'evil
  (dolist (m (list evil-insert-state-map evil-ex-completion-map minibuffer-local-map))
    (key-chord-define m "kk" "()\C-b")
    (key-chord-define m ",," "[]\C-b")
    (key-chord-define m "hh" "{}\C-b")
    (key-chord-define m "gt" "!")
    (key-chord-define m "qa" "@")
    (key-chord-define m "mj" "$")
    (key-chord-define m "fh" "%")
    (key-chord-define m "sj" "^")
    (key-chord-define m "aa" "&")
    (key-chord-define m "cj" "*")
    (key-chord-define m "uu" "_")
    (key-chord-define m "ji" "-")
    (key-chord-define m "jj" "+")
    (key-chord-define m "dh" "=")
    (key-chord-define m "zl" "|")
    (key-chord-define m "bw" "~")
    (key-chord-define m "xy" "<")
    (key-chord-define m "dy" ">")
    (key-chord-define m "jk" "<>\C-b")
    (key-chord-define m "ww" "?")
    (key-chord-define m "jh" "#")
    (key-chord-define m "nj" "|>")
    (key-chord-define m ",a" "--")
    (key-chord-define m ",c" "<=")
    (key-chord-define m ",b" ">=")
    (key-chord-define m ",d" "==")
    (key-chord-define m ",." "[[]]\C-b\C-b")
    (key-chord-define m ";r" "<-")
    (key-chord-define m ";j" "%>%")
    (key-chord-define m ";f" "5")
    (key-chord-define m ";i" "6")
    (key-chord-define m ";s" "7")
    (key-chord-define m ";e" "8")
    (key-chord-define m ";n" "9")
    (key-chord-define m ";t" "0"))

  ;; british pound
  ;; £ is not working in key-chord
  (with-eval-after-load 'general
    (general-evil-setup t)
    (defun p-insert-pound ()
      (interactive)
      (insert "£"))
    (general-imap "y"
      (general-key-dispatch 'self-insert-command
	:timeout 0.3
	"b" 'p-insert-pound))))

;;; smartparens
(add-hook 'after-init-hook 'smartparens-global-mode)

(with-eval-after-load 'smartparens
  (diminish 'smartparens-mode)

  (sp-local-pair 'ess-stata-mode "`" "'")
  (sp-with-modes '(lisp-mode emacs-lisp-mode lisp-interaction-mode)
    (sp-local-pair "'" nil :actions nil))
  (sp-with-modes '(markdown-mode text-mode)
    (sp-local-pair "`" nil :actions nil))
  ;; automatically close f string in Python
  (sp-local-pair 'python-mode "f'" "'")

  (defun p-add-single-quote ()
    (interactive)
    (sp-wrap-with-pair "'"))

  (defun p-add-double-quote ()
    (interactive)
    (sp-wrap-with-pair "\""))

  (defun p-add-paren ()
    (interactive)
    (sp-wrap-with-pair "("))

  (defun p-add-bracket ()
    (interactive)
    (sp-wrap-with-pair "["))

  (defun p-add-curly ()
    (interactive)
    (sp-wrap-with-pair "{")))

;;; kill-sexp and insert
(defun p-kill-sexp-and-insert ()
    (interactive)
    (kill-sexp)
    (evil-insert 0))

;;; keybindings
(with-eval-after-load 'evil
  (define-key evil-normal-state-map (kbd ";u") 'p-beginning-of-block)
  (define-key evil-normal-state-map (kbd ";l") 'p-end-of-block)
  (define-key evil-normal-state-map (kbd "C-i") 'p-delete-backward-to-tab)
  (define-key evil-normal-state-map (kbd "goc") 'p-clear-line)
  (define-key evil-normal-state-map (kbd "god") 'kill-sexp)
  (define-key evil-normal-state-map (kbd "goi") 'p-kill-sexp-and-insert)
  (define-key evil-normal-state-map (kbd ";c") 'cycle-at-point)

  (define-key evil-visual-state-map (kbd ";u") 'p-beginning-of-block)
  (define-key evil-visual-state-map (kbd ";l") 'p-end-of-block)

  (define-key evil-insert-state-map (kbd "C-u") 'p-kill-to-begin-of-line)
  (define-key evil-insert-state-map (kbd "C-i") 'p-delete-backward-to-tab)
  (define-key evil-insert-state-map (kbd "C-.") 'p-insert-spaces)

  (general-create-definer p-space-leader-def
    :prefix "SPC"
    :states '(normal visual))
  (p-space-leader-def
    "f"  '(:ignore t :which-key "file")
    "fi" '(p-insert-file-name :which-key "insert file path and name")
    "e"  '(:ignore t :which-key "editing")
    "ei" '(p-insert-log-item :which-key "insert log item")
    "ed" '(p-insert-date :which-key "insert date")
    "eD" '(p-insert-date-alt :which-key "insert date alt")
    "eu" '(p-insert-uk-date :which-key "insert UK date"))

  (general-create-definer p-comma-leader-def
    :prefix ","
    :states '(normal visual))
  (p-comma-leader-def
    "," '(p-select-block :which-key "select block")
    "m" '(p-goto-matching-bracket :which-key "match bracket")
    "c" '(sp-splice-sexp :which-key "clear surround")
    "k" '(p-add-paren :which-key "p-add-paren")
    "f" '(p-add-bracket :which-key "p-add-bracket")
    "h" '(p-add-curly :which-key "p-add-curly")
    "s" '(p-add-single-quote :which-key "p-add-single-quote")
    "d" '(p-add-double-quote :which-key "p-add-double-quote")))

(provide 'init-text)
;;;;; init-text.el ends here
