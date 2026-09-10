;;; init-xah.el --- Functions from Xah Lee -*- lexical-binding: t -*-

;; https://github.com/xahlee/xah-fly-keys/blob/master/xah-fly-keys.el
(defun xah-open-in-external-app (&optional Fname)
  "Open the current file or dired marked files in external app.
When called in emacs lisp, if Fname is given, open that.

URL `http://xahlee.info/emacs/emacs/emacs_dired_open_file_in_ext_apps.html'
Created: 2019-11-04
Version: 2025-04-18"
  (interactive)
  (let (xfileList xdoIt)
    (setq xfileList
          (if Fname
              (list Fname)
            (if (eq major-mode 'dired-mode)
                (dired-get-marked-files)
              (list buffer-file-name))))
    (setq xdoIt (if (length< xfileList 10) t (y-or-n-p "Open more than 10 files? ")))
    (when xdoIt
      (cond
       ((eq system-type 'windows-nt)
        (let ((xoutbuf (get-buffer-create "*xah open in external app*"))
              (xcmdlist (list "PowerShell" "-Command" "Invoke-Item" "-LiteralPath")))
          (mapc
           (lambda (x)
             (apply 'start-process (append (list "xah open in external app" xoutbuf) xcmdlist (list (format "'%s'" (string-replace "'" "`'" x))) nil)))
           xfileList)))
       ((eq system-type 'darwin)
        (mapc (lambda (xfpath) (shell-command (concat "open " (shell-quote-argument xfpath)))) xfileList))
       ((eq system-type 'gnu/linux)
        (mapc (lambda (xfpath)
                (call-process shell-file-name nil 0 nil
                              shell-command-switch
                              (format "%s %s"
                                      "xdg-open"
                                      (shell-quote-argument xfpath))))
              xfileList))
       ((eq system-type 'berkeley-unix)
        (mapc (lambda (xfpath) (let ((process-connection-type nil)) (start-process "" nil "xdg-open" xfpath))) xfileList))))))

;; https://github.com/xahlee/xah-fly-keys/blob/master/xah-fly-keys.el
(defun xah-shrink-whitespaces ()
  "Remove whitespaces around cursor .
Shrink neighboring spaces, then newlines, then spaces again, leaving one space or newline at each step, till no more white space.
URL `http://xahlee.info/emacs/emacs/emacs_shrink_whitespace.html'
Created: 2014-10-21
Version: 2023-07-12"
  (interactive)
  (let ((xeol-count 0)
        (xp0 (point))
        xbeg  ; whitespace begin
        xend  ; whitespace end
        (xcharBefore (char-before))
        (xcharAfter (char-after))
        xspace-neighbor-p)
    (setq xspace-neighbor-p (or (eq xcharBefore 32) (eq xcharBefore 9) (eq xcharAfter 32) (eq xcharAfter 9)))
    (skip-chars-backward " \n\t　")
    (setq xbeg (point))
    (goto-char xp0)
    (skip-chars-forward " \n\t　")
    (setq xend (point))
    (goto-char xbeg)
    (while (search-forward "\n" xend t)
      (setq xeol-count (1+ xeol-count)))
    (goto-char xp0)
    (cond
     ((eq xeol-count 0)
      (if (> (- xend xbeg) 1)
          (progn
            (delete-horizontal-space) (insert " "))
        (progn (delete-horizontal-space))))
     ((eq xeol-count 1)
      (if xspace-neighbor-p
          (delete-horizontal-space)
        (progn (delete-space--internal "\n" nil) (insert " "))))
     ((eq xeol-count 2)
      (if xspace-neighbor-p
          (delete-horizontal-space)
        (progn
          (delete-space--internal "\n" nil)
          (insert "\n"))))
     ((> xeol-count 2)
      (if xspace-neighbor-p
          (delete-horizontal-space)
        (progn
          (goto-char xend)
          (search-backward "\n")
          (delete-region xbeg (point))
          (insert "\n"))))
     (t (progn
          (message "nothing done. logic error 40873. shouldn't reach here"))))))

;; https://github.com/xahlee/xah-fly-keys/blob/master/xah-fly-keys.el
(defvar xah-smart-delete-dispatch
  '((xah-wolfram-mode . xah-wolfram-smart-delete-backward)
    (xah-html-mode . xah-html-smart-delete-backward))
  "Used by `xah-smart-delete'.
This makes that function behavior dependent on current major-mode.
Value is Alist of pairs, each is of the form
(‹major-mode-name› . ‹function-name›)
If ‹major-mode-name› match current var `major-mode', the paired function is called.
If no major mode matches, `xah-smart-delete' default behavior is used.
Version: 2024-06-05")

(defun xah-smart-delete (&optional BracketOnly SkipDispatch)
  "Smart backward delete.
Typically, delete to the left 1 char or entire bracketed text.
Behavior depends on what's left char, and current `major-mode'.

If `xah-smart-delete-dispatch' match, call the matched function instead.
If region active, delete region.
If cursor left is space tab newline, delete them.
If cursor left is bracket, delete the whole bracket block.
If cursor left is string quote, delete the string.
Else just delete one char to the left.

If `universal-argument' is called first, do not delete bracket's innertext.

In elisp code, arg BracketOnly if true, do not delete innertext. SkipDispatch if true, skip checking `xah-smart-delete-dispatch'.

Created: 2023-07-22
Version: 2025-07-30"
  (interactive (list current-prefix-arg nil))
  (let (xfun)
    (cond
     ((and (not SkipDispatch) (setq xfun (assq major-mode xah-smart-delete-dispatch)))
      (message "calling cdr of %s" xfun)
      (funcall (cdr xfun)))
     ((region-active-p)
      (kill-region (region-beginning) (region-end)))
     ((or
       ;; 32 is space, 9 is tab, 10 is newline
       (eq (char-before) 32)
       (eq (char-before) 10)
       (eq (char-before) 9))
      (if (minibufferp (current-buffer))
          (while (or (eq (char-before) 32) (eq (char-before) 10) (eq (char-before) 9))
            (delete-char -1))
        (let ((xp0 (point)) xbeg xend)
          (skip-chars-backward " \t\n")
          (setq xbeg (point) xend xp0)
          (if (eq real-this-command real-last-command)
              (kill-append (delete-and-extract-region xbeg xend) t)
            (kill-region xbeg xend)))))
     ((prog2 (backward-char) (looking-at "\\s)") (forward-char))
      ;; (message "cursor left is closing bracket")
      (cond
       ;; unmatched bracket, just delete it
       ((not (condition-case nil (scan-sexps (point) -1) (scan-error nil)))
        (warn "There was unmatched bracket: no paired opening bracket on left of cursor")
        (delete-char -1))
       ;; delete just the brackets
       (BracketOnly
        (let ((xp0 (point)) xbeg)
          (forward-sexp -1)
          (while (looking-at "\\s'") (forward-char))
          (setq xbeg (point))
          (goto-char xp0)
          (delete-char -1)
          (goto-char xbeg)
          (delete-char 1)
          (goto-char (- xp0 2))))
       ;; delete the bracket block
       (t
        (let ((xp0 (point)) xbeg xend)
          (forward-sexp -1)
          (while (looking-at "\\s'") (forward-char))
          (setq xbeg (point) xend xp0)
          (if (eq real-this-command real-last-command)
              (kill-append (delete-and-extract-region xbeg xend) t)
            (kill-region xbeg xend))))))
     ((prog2 (backward-char) (looking-at "\\s(") (forward-char))
      ;; (message "cursor left is opening bracket")
      (cond
       ;; unmatched bracket, just delete it
       ((save-excursion
          (backward-char)
          (not (condition-case nil (scan-sexps (point) 1) (scan-error nil))))
        (warn "There was unmatched bracket: no paired closing bracket on right of cursor")
        (delete-char -1))
       ;; delete just the brackets
       (BracketOnly
        (let (xbeg)
          (backward-char)
          (setq xbeg (point))
          (forward-sexp 1)
          (delete-char -1)
          (goto-char xbeg)
          (delete-char 1)))
       ;; delete the bracket block
       (t
        (let (xbeg xend)
          (backward-char)
          (setq xbeg (point))
          (forward-sexp 1)
          (setq xend (point))
          (if (eq real-this-command real-last-command)
              (kill-append (delete-and-extract-region xbeg xend) t)
            (kill-region xbeg xend))))))
     ((prog2 (backward-char) (looking-at "\\s\"") (forward-char))
      (message "calling xah-delete-string-backward")
      (xah-delete-string-backward BracketOnly))
     (t (delete-char -1)))))

(defvar xah-brackets '( "“”" "()" "[]" "{}" "<>" "＜＞" "（）" "［］" "｛｝" "⦅⦆" "〚〛" "⦃⦄" "‹›" "«»" "「」" "〈〉" "《》" "【】" "〔〕" "⦗⦘" "『』" "〖〗" "〘〙" "｢｣" "⟦⟧" "⟨⟩" "⟪⟫" "⟮⟯" "⟬⟭" "⌈⌉" "⌊⌋" "⦇⦈" "⦉⦊" "❛❜" "❝❞" "❨❩" "❪❫" "❴❵" "❬❭" "❮❯" "❰❱" "❲❳" "〈〉" "⦑⦒" "⧼⧽" "﹙﹚" "﹛﹜" "﹝﹞" "⁽⁾" "₍₎" "⦋⦌" "⦍⦎" "⦏⦐" "⁅⁆" "⸢⸣" "⸤⸥" "⟅⟆" "⦓⦔" "⦕⦖" "⸦⸧" "⸨⸩" "｟｠")
  "A list of strings, each element is a string of 2 chars, the left bracket and a matching right bracket.
Used by
`xah-backward-left-bracket'.
`xah-forward-right-bracket'.
`xah-goto-matching-bracket'.
URL `http://xahlee.info/emacs/emacs/emacs_navigating_keys_for_brackets.html'
")

(defconst xah-left-brackets
  (regexp-opt (mapcar (lambda (x) (substring x 0 1)) xah-brackets))
  "Regex string of left bracket chars. Generated from `xah-brackets'.
URL `http://xahlee.info/emacs/emacs/emacs_navigating_keys_for_brackets.html'
")

(defconst xah-right-brackets
  (regexp-opt (mapcar (lambda (x) (substring x 1 2)) xah-brackets))
  "Regex string of right bracket chars. Generated from `xah-brackets'.
URL `http://xahlee.info/emacs/emacs/emacs_navigating_keys_for_brackets.html'
")

(defun xah-backward-left-bracket ()
  "Move cursor to the previous occurrence of left bracket.
The list of brackets to jump to is defined by `xah-left-brackets'.

URL `http://xahlee.info/emacs/emacs/emacs_navigating_keys_for_brackets.html'
Created: 2015-10-01
Version: 2026-07-09"
  (interactive)
  (re-search-backward xah-left-brackets nil t))

(defun xah-forward-right-bracket ()
  "Move cursor to the next occurrence of right bracket.
The list of brackets to jump to is defined by `xah-right-brackets'.

URL `http://xahlee.info/emacs/emacs/emacs_navigating_keys_for_brackets.html'
Created: 2015-10-01
Version: 2026-07-09"
  (interactive)
  (re-search-forward xah-right-brackets nil t))

(defun xah-goto-matching-bracket ()
  "Move cursor to the matching bracket.
If cursor is not on a bracket, call `backward-up-list'.
The list of brackets to jump to is defined by `xah-left-brackets' and `xah-right-brackets'.

URL `http://xahlee.info/emacs/emacs/emacs_navigating_keys_for_brackets.html'
Created: 2016-11-22
Version: 2026-07-09"
  (interactive)
  (if (nth 3 (syntax-ppss))
      (backward-up-list 1 t t)
    (cond
     ((eq (char-after) ?\") (forward-sexp))
     ((eq (char-before) ?\") (backward-sexp))
     ((looking-at xah-left-brackets)
      (forward-sexp))
     ((if (eq (point-min) (point))
          nil
        (prog2
            (backward-char)
            (looking-at xah-right-brackets)
          (forward-char)))
      (backward-sexp)
      (while (looking-at "\\s'") (forward-char)))
     (t (backward-up-list 1 t t)))))

(provide 'init-xah)
