;;;;; init-editing.el --- Editing -*- lexical-binding: t -*-

;;; Install packages
(straight-use-package 'stupid-indent-mode)
(straight-use-package 'smartparens)
(straight-use-package 'cycle-at-point)
(straight-use-package 's)

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

;;; Show whitespace and delete on saving
;; https://github.com/zilongshanren/emacs.d/blob/develop/lisp/init-basic.el
(defun emacs-editing--enable-trailing-whitespace ()
  (setq show-trailing-whitespace t)
  (add-hook 'before-save-hook #'delete-trailing-whitespace nil t))

(dolist (hook '(prog-mode-hook markdown-mode-hook org-mode-hook conf-mode-hook text-mode-hook))
  (add-hook hook 'emacs-editing--enable-trailing-whitespace))

;;; Parentheses
;; turn on paren match highlighting
(show-paren-mode 1)

;;; Vim style replace
(defun p-ex-evil-buffer-replace ()
  (interactive)
  (evil-ex (concat "%s/")))

(defun p-ex-evil-selection-replace ()
  (interactive)
  (evil-ex (concat "'<,'>s/")))

;;; Useful functions
(defun p-select-function ()
  (interactive)
  (forward-char)
  (beginning-of-defun)
  (evilmi-select-items))

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

; insert date
(defun p-insert-uk-date ()
  (interactive)
  (insert (format-time-string "%d-%m-%Y")))

(defun p-insert-date ()
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))

(defun p-insert-date-alt ()
  (interactive)
  (insert (format-time-string "%Y-%m-%d, %A")))

;; insert current buffer name
(defun p-insert-file-name ()
  (interactive)
  (insert (buffer-file-name)))

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

;; query replace many
;; https://tony-zorman.com/posts/query-replace/2022-08-06-query-replace-many.html
(require 's)

(defun p-get-queries (&optional pairs)
  (-let* (((from to delim arg)
           (query-replace-read-args
            (s-join " "
                    (-non-nil
                     (list "Query replace many"
                           (cond ((eq current-prefix-arg '-) "backward")
                                 (current-prefix-arg "word"))
                           (when (use-region-p) "in region"))))
            nil))
          (from-to (cons (regexp-quote from)
                         (s-replace "\\" "\\\\" to))))
    (if (-contains? pairs from-to)
        (list pairs delim arg)
      (p-get-queries (push from-to pairs)))))

(defun p-query-replace-many
    (pairs &optional delimited start end backward region-noncontiguous-p)
  (interactive
   (let ((common (p-get-queries)))
     (list (nth 0 common) (nth 1 common)
           (if (use-region-p) (region-beginning))
           (if (use-region-p) (region-end))
           (nth 2 common)
           (if (use-region-p) (region-noncontiguous-p)))))
  (perform-replace
   (concat "\\(?:" (mapconcat #'car pairs "\\|") "\\)")
   (cons (lambda (pairs _count)
           (cl-loop for (from . to) in pairs
                    when (string-match from (match-string 0))
                    return to))
         pairs)
   :query :regexp
   delimited nil nil start end backward region-noncontiguous-p))

;;; Keybindings
(with-eval-after-load 'evil
  ;; I prefer to use C-n and C-p in many other places
  (define-key evil-normal-state-map (kbd "C-n") nil)
  (define-key evil-normal-state-map (kbd "C-p") nil)
  (define-key evil-insert-state-map (kbd "C-n") nil)
  (define-key evil-insert-state-map (kbd "C-p") nil)

  (define-key evil-normal-state-map (kbd ",.") 'p-select-function)
  (define-key evil-normal-state-map (kbd "gcc") 'evilnc-comment-or-uncomment-lines)
  (define-key evil-normal-state-map (kbd "gor") 'p-ex-evil-buffer-replace)
  (define-key evil-normal-state-map (kbd "gom") 'p-query-replace-many)
  (define-key evil-normal-state-map (kbd "gos") 'transpose-sexps)
  (define-key evil-normal-state-map (kbd ",a") 'beginning-of-defun)
  (define-key evil-normal-state-map (kbd ",e") 'end-of-defun)
  (define-key evil-normal-state-map (kbd ";a") 'p-beginning-of-line-or-block)
  (define-key evil-normal-state-map (kbd ";e") 'p-end-of-line-or-block)
  (define-key evil-normal-state-map (kbd "C-i") 'p-delete-backward-to-tab)
  (define-key evil-normal-state-map (kbd "goc") 'p-clear-line)
  (define-key evil-normal-state-map (kbd "god") 'kill-sexp)
  (define-key evil-normal-state-map (kbd "goi") 'p-kill-sexp-and-insert)
  (define-key evil-normal-state-map (kbd ";c") 'cycle-at-point)

  (define-key evil-visual-state-map (kbd "gcc") 'evilnc-comment-or-uncomment-lines)
  (define-key evil-visual-state-map (kbd "gor") 'p-ex-evil-selection-replace)
  (define-key evil-visual-state-map (kbd "gom") 'p-query-replace-many)
  (define-key evil-visual-state-map (kbd ",e") 'end-of-defun)
  (define-key evil-visual-state-map (kbd ";a") 'p-beginning-of-line-or-block)
  (define-key evil-visual-state-map (kbd ";e") 'p-end-of-line-or-block)

  (define-key evil-insert-state-map (kbd "C-a") 'evil-beginning-of-line)
  (define-key evil-insert-state-map (kbd "C-e") 'end-of-line)
  (define-key evil-insert-state-map (kbd "C-k") 'delete-backward-char)
  (define-key evil-insert-state-map (kbd "C-u") 'p-kill-to-begin-of-line)
  (define-key evil-insert-state-map (kbd "C-i") 'p-delete-backward-to-tab)

  (define-key evil-ex-completion-map (kbd "C-f") 'forward-char)
  (define-key evil-ex-completion-map (kbd "C-b") 'backward-char)
  (define-key evil-ex-completion-map (kbd "C-k") 'delete-backward-char)

  (define-key evil-inner-text-objects-map "f" 'evil-inner-bracket)
  (define-key evil-inner-text-objects-map "h" 'evil-inner-curly)
  (define-key evil-inner-text-objects-map "d" 'evil-inner-double-quote)
  (define-key evil-outer-text-objects-map "f" 'evil-a-bracket)
  (define-key evil-outer-text-objects-map "h" 'evil-a-curly)
  (define-key evil-outer-text-objects-map "d" 'evil-a-double-quote)

  (general-create-definer p-space-leader-def
    :prefix "SPC"
    :states '(normal visual))
  (p-space-leader-def
    "f"  '(:ignore t :which-key "file")
    "fi" '(p-insert-file-name :which-key "insert file path and name")
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
    "," '(p-select-block :which-key "select block")
    "c" '(sp-splice-sexp :which-key "clear surround")
    "k" '(p-add-paren :which-key "p-add-paren")
    "f" '(p-add-bracket :which-key "p-add-bracket")
    "h" '(p-add-curly :which-key "p-add-curly")
    "s" '(p-add-single-quote :which-key "p-add-single-quote")
    "d" '(p-add-double-quote :which-key "p-add-double-quote")))

(provide 'init-editing)
;;;;; init-editing.el ends here
