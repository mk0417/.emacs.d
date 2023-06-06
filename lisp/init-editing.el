;;;;; init-editing.el --- Editing -*- lexical-binding: t -*-

;;; Install packages
(straight-use-package 'stupid-indent-mode)
(straight-use-package 'smartparens)
(straight-use-package 'grugru)
(straight-use-package '(query-replace-many :type git :host github :repo "slotThe/query-replace-many"))
(straight-use-package '(thing-edit :type git :host github :repo "manateelazycat/thing-edit"))
(straight-use-package 'expand-region)

;;; Stupid-indent-mode
(require 'stupid-indent-mode)
(add-hook 'prog-mode-hook 'stupid-indent-mode)
(add-hook 'python-mode-hook
          (lambda ()
            (setq-local stupid-indent-level 4)))
(diminish 'stupid-indent-mode)

;;; Smartparens
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

;; kill-sexp and insert
(defun p-kill-sexp-and-insert ()
  (interactive)
  (kill-sexp)
  (evil-insert 0))

;;; grugru
(grugru-define-multiple
  (emacs-lisp-mode
   (word "t" "nil"))
  (python-mode
   (word "apply" "transform"))
  (word "yes" "no")
  (word "Yes" "No")
  (word "true" "false")
  (word "True" "False")
  (word "TRUE" "FALSE")
  (word "mean" "median")
  (word "min" "max"))

;;; Show whitespace and delete on saving
;; https://github.com/zilongshanren/emacs.d/blob/develop/lisp/init-basic.el
(defun p-emacs-editing-enable-trailing-whitespace ()
  (setq show-trailing-whitespace t)
  (add-hook 'before-save-hook #'delete-trailing-whitespace nil t))

(dolist (hook '(prog-mode-hook markdown-mode-hook org-mode-hook conf-mode-hook text-mode-hook))
  (add-hook hook 'p-emacs-editing-enable-trailing-whitespace))

;;; Parentheses
;; turn on paren match highlighting
(show-paren-mode 1)

;;; expand-region
(autoload 'er/mark-defun "expand-region")

;;; query-replace-many
(autoload 'query-replace-many "query-replace-many")

;;; Useful functions
(defun p-beginning-of-line-or-block ()
  (interactive)
  (let (($p (point)))
    (if (or (equal (point) (line-beginning-position))
            (equal last-command this-command ))
        (if (re-search-backward "\n[\t\n ]*\n+" nil "NOERROR")
            (progn
              (skip-chars-backward "\n\t ")
              (forward-char ))
          (goto-char (point-min)))
      (progn
        (back-to-indentation)
        (when (eq $p (point))
          (beginning-of-line))))))

(defun p-end-of-line-or-block ()
  (interactive)
  (if (or (equal (point) (line-end-position))
          (equal last-command this-command ))
      (progn
        (re-search-forward "\n[\t\n ]*\n+" nil "NOERROR" ))
    (end-of-line)))

(defun p-insert-num-list (start end format-string from)
  (interactive
   (list (region-beginning) (region-end)
         (read-string "Number rectangle: "
                      (if (looking-back "^ *") "%d. " "%d"))
         (read-number "From: " 1)))
  (save-excursion
    (goto-char start)
    (setq start (point-marker))
    (goto-char end)
    (setq end (point-marker))
    (goto-char start)
    (cl-loop with column = (current-column)
             while (and (<= (point) end) (not (eobp)))
             for i from from do
             (move-to-column column t)
             (insert (format format-string i))
             (forward-line 1)))
  (goto-char start))

(defun p-delete-blank-lines ()
  (interactive)
  (flush-lines "^$" (region-beginning) (region-end)))

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

(defun p-select-bottom-block ()
  (interactive)
  (save-excursion
    (mark-paragraph)))

;; insert date
(defun p-insert-uk-date ()
  (interactive)
  (insert (format-time-string "%d-%m-%Y")))

(defun p-insert-date ()
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))

(defun p-insert-date-alt ()
  (interactive)
  (insert (format-time-string "%Y-%m-%d, %A")))

;; backward kill to the beginning of line
(defun p-kill-to-begin-of-line ()
  (interactive)
  (kill-line 0))

;; delete to tab
(defun p-delete-backward-to-tab ()
  (interactive)
  (when (evil-normal-state-p)
    (beginning-of-line-text)
    (kill-line 0)
    (insert "    "))
  (when (evil-insert-state-p)
    (kill-line 0)
    (insert "    ")))

;; clear line
(defun p-clear-line ()
  (interactive)
  (beginning-of-line)
  (kill-line))

;; add whitespaces surrounding current character
(defun p-add-whitespaces ()
  (interactive)
  (insert " ")
  (forward-char 1)
  (insert " "))

;; delete surrounding spaces on current character
(defun p-delete-surrounding-whitespace ()
  (interactive)
  (delete-char -1)
  (forward-char 1)
  (delete-char 1))

;;; Cycle beginning and end of line
;; https://emacs-china.org/t/evil/24251
(defun p-cycle-line-beginning-end ()
  (interactive)
  (cl-block 'my-return
    (when (and (looking-at "[^\s]") (looking-back "^\s*")) (evil-end-of-line) (cl-return-from 'my-return))
    (when (looking-at (if evil-move-beyond-eol "$" ".$")) (evil-beginning-of-line) (cl-return-from 'my-return))
    (when (bolp) (evil-first-non-blank) (cl-return-from 'my-return))
    (evil-first-non-blank)))

;;; Select line (from first char to last char in a line)
(defun p-select-line-range ()
  (interactive)
  (let ((start (progn (back-to-indentation) (point)))
        (end (progn (end-of-line) (skip-chars-backward " \t") (point))))
    (goto-char start)
    (set-mark end)))

;;; Replace with last kill
;; https://gitlab.com/protesilaos/dotfiles/-/blob/master/emacs/.emacs.d/prot-lisp/prot-simple.el
(defun p-simple-yank-replace-line-or-region ()
  (interactive)
  (if (use-region-p)
      (delete-region (region-beginning) (region-end))
    (delete-region (line-beginning-position) (line-end-position)))
  (yank))

;;; Remove extra spaces
(defun p-remove-extra-spaces-in-region (start end)
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-min))
      (while (re-search-forward "\\s-+" nil t)
        (replace-match " " nil nil))
      (goto-char (point-max))
      (while (looking-back "\\s-+")
        (delete-char (- (match-end 0) (match-beginning 0)))))))

(defun p-remove-extra-spaces-in-line ()
  (save-excursion
    (beginning-of-line)
    (let ((start (point))
          (end (line-end-position)))
      (narrow-to-region start end)
      (while (re-search-forward "\\s-+" nil t)
        (replace-match " " nil nil))
      (goto-char (point-max))
      (while (looking-back "\\s-+")
        (delete-char (- (match-end 0) (match-beginning 0))))
      (widen))))

(defun p-remove-extra-spaces ()
  (interactive)
  (if (use-region-p)
      (p-remove-extra-spaces-in-region (region-beginning) (region-end))
    (p-remove-extra-spaces-in-line)))

;;; Deletes text between commas
(defun p-delete-between-commas ()
  (interactive)
  (let ((start (progn (search-backward "," nil t) (forward-char) (skip-chars-forward " \t") (point)))
        (end (progn (search-forward "," nil t) (backward-char) (skip-chars-backward " \t") (point))))
    (delete-region start end)))

;;; Select text between commas
(defun p-select-between-commas ()
  (interactive)
  (let ((start (progn (search-backward "," nil t) (forward-char) (skip-chars-forward " \t") (point)))
        (end (progn (search-forward "," nil t) (backward-char) (skip-chars-backward " \t") (point))))
    (set-mark start)
    (goto-char (- end 1))))

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
  (p-select-block)
  ;; make sure to run evil ex after above functions
  ;; https://emacs.stackexchange.com/questions/11003/run-a-function-after-control-returns-to-the-command-loop
  (run-with-timer 0 nil 'p-ex-evil-selection-replace-yank))

;;; Keybindings
(with-eval-after-load 'evil
  (define-key evil-normal-state-map (kbd "gor") 'p-ex-evil-buffer-replace)
  (define-key evil-normal-state-map (kbd "goa") 'p-ex-evil-replace-yank)
  (define-key evil-normal-state-map (kbd ";a") 'p-beginning-of-line-or-block)
  (define-key evil-normal-state-map (kbd ";e") 'p-end-of-line-or-block)
  (define-key evil-normal-state-map (kbd "C-i") 'p-delete-backward-to-tab)
  (define-key evil-normal-state-map (kbd "goc") 'p-clear-line)
  (define-key evil-normal-state-map (kbd "gci") 'p-kill-sexp-and-insert)
  (define-key evil-normal-state-map (kbd "gce") 'thing-copy-to-line-end)
  (define-key evil-normal-state-map (kbd ";c") 'grugru)
  (define-key evil-normal-state-map (kbd "gcr") 'thing-replace-symbol)
  (define-key evil-normal-state-map (kbd "gom") 'query-replace-many)
  (define-key evil-normal-state-map (kbd ";ii") 'er/expand-region)
  (define-key evil-normal-state-map (kbd ",.") 'er/mark-defun)
  (define-key evil-normal-state-map (kbd ",,") 'p-select-block)
  (define-key evil-normal-state-map (kbd ",b") 'p-select-bottom-block)
  (define-key evil-normal-state-map (kbd "gcy") 'p-simple-yank-replace-line-or-region)
  (define-key evil-normal-state-map (kbd "gcs") 'p-remove-extra-spaces)
  (define-key evil-normal-state-map (kbd "gcj") 'p-delete-between-commas)
  (define-key evil-normal-state-map (kbd "gcm") 'p-select-between-commas)
  (define-key evil-normal-state-map (kbd "gcl") 'p-select-line-range)

  (define-key evil-visual-state-map (kbd "gor") 'p-ex-evil-selection-replace)
  (define-key evil-visual-state-map (kbd "goa") 'p-ex-evil-selection-replace-yank)
  (define-key evil-visual-state-map (kbd ";a") 'p-beginning-of-line-or-block)
  (define-key evil-visual-state-map (kbd ";e") 'p-end-of-line-or-block)
  (define-key evil-visual-state-map (kbd "gom") 'query-replace-many)
  (define-key evil-visual-state-map (kbd ";ii") 'er/expand-region)
  (define-key evil-visual-state-map (kbd ",,") 'p-select-block)
  (define-key evil-visual-state-map (kbd "gcs") 'p-remove-extra-spaces)

  (define-key evil-insert-state-map (kbd "C-u") 'p-kill-to-begin-of-line)
  (define-key evil-insert-state-map (kbd "C-i") 'p-delete-backward-to-tab)

  (general-create-definer p-space-leader-def
    :prefix "SPC"
    :states '(normal visual))
  (p-space-leader-def
    "e" '(:ignore t :which-key "editing")
    "en" '(p-insert-num-list :which-key "insert number sequence")
    "ec" '(whitespace-cleanup :which-key "clear whitespace")
    "eb" '(p-delete-blank-lines :which-key "delete blank lines")
    "ed" '(p-insert-date :which-key "insert date")
    "eD" '(p-insert-date-alt :which-key "insert date alt")
    "eu" '(p-insert-uk-date :which-key "insert UK date")
    "es" '(ispell-buffer :which-key "spell check in buffer")
    "er" '(ispell-region :which-key "spell check in region"))

  (general-create-definer p-comma-leader-def
    :prefix ","
    :states '(normal visual))
  (p-comma-leader-def
    "c" '(sp-splice-sexp :which-key "clear surround")
    "k" '(p-add-paren :which-key "p-add-paren")
    "f" '(p-add-bracket :which-key "p-add-bracket")
    "h" '(p-add-curly :which-key "p-add-curly")
    "s" '(p-add-single-quote :which-key "p-add-single-quote")
    "d" '(p-add-double-quote :which-key "p-add-double-quote")
    "w" '(p-add-whitespaces :which-key "p-add-whitespaces")
    "x" '(p-delete-surrounding-whitespace :which-key "p-delete-whitespaces")))

(provide 'init-editing)
;;;;; init-editing.el ends here
