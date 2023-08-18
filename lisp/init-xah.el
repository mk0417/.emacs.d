;;;;; init-xah.el --- Functions from Xah Lee -*- lexical-binding: t -*-

(defvar xah-brackets '("“”" "()" "[]" "{}" "<>")
  "A list of strings, each element is a string of 2 chars, the left bracket and a matching right bracket.
Used by `xah-select-text-in-quote' and others.
Version 2023-07-31")

(defconst xah-left-brackets
  (mapcar (lambda (x) (substring x 0 1)) xah-brackets)
  "List of left bracket chars. Each element is a string.")

(defconst xah-right-brackets
  (mapcar (lambda (x) (substring x 1 2)) xah-brackets)
  "List of right bracket chars. Each element is a string.")

(defconst xah-punctuation-regex "[\"=]+"
  "A regex string for the purpose of moving cursor to a punctuation, used by `xah-forward-punct'.")

(defun xah-forward-punct (&optional n)
  "Move cursor to the next occurrence of `xah-punctuation-regex'.

URL `http://xahlee.info/emacs/emacs/emacs_jump_to_punctuations.html'
Version: 2017-06-26 2023-07-25"
  (interactive "p")
  (re-search-forward xah-punctuation-regex nil t n))

(defun xah-backward-punct (&optional n)
  "Move cursor to the previous occurrence of punctuation.
See `xah-forward-punct'
URL `http://xahlee.info/emacs/emacs/emacs_jump_to_punctuations.html'
Version: 2017-06-26"
  (interactive "p")
  (re-search-backward xah-punctuation-regex nil t n))

(defun xah-backward-left-bracket ()
  "Move cursor to the previous occurrence of left bracket.
The list of brackets to jump to is defined by `xah-left-brackets'.

URL `http://xahlee.info/emacs/emacs/emacs_navigating_keys_for_brackets.html'
Version: 2015-10-01"
  (interactive)
  (re-search-backward (regexp-opt xah-left-brackets) nil t))

(defun xah-forward-right-bracket ()
  "Move cursor to the next occurrence of right bracket.
The list of brackets to jump to is defined by `xah-right-brackets'.

URL `http://xahlee.info/emacs/emacs/emacs_navigating_keys_for_brackets.html'
Version: 2015-10-01"
  (interactive)
  (re-search-forward (regexp-opt xah-right-brackets) nil t))

(defun xah-forward-quote ()
  "Move cursor to the next occurrence of \".
If there are consecutive quotes of the same char, keep moving until none.
Returns `t' if found, else `nil'.

URL `http://xahlee.info/emacs/emacs/emacs_navigating_keys_for_brackets.html'
Version: 2016-07-23"
  (interactive)
  (if (re-search-forward "\\\"+" nil t)
      t
    (progn
      (message "No more quotes after cursor..")
      nil)))

(defun xah-forward-quote-smart ()
  "Move cursor to the current or next string quote.
Place cursor at the position after the left quote.
Repeated call will find the next string.

URL `http://xahlee.info/emacs/emacs/emacs_navigating_keys_for_brackets.html'
Version: 2016-11-22"
  (interactive)
  (let ((xpos (point)))
    (if (nth 3 (syntax-ppss))
        (progn
          (backward-up-list 1 'ESCAPE-STRINGS 'NO-SYNTAX-CROSSING)
          (forward-sexp)
          (re-search-forward "\\\"" nil t))
      (progn (re-search-forward "\\\"" nil t)))
    (when (<= (point) xpos)
      (progn (re-search-forward "\\\"" nil t)))))

(defun xah-search-current-word ()
  "Call `isearch' on current word or selection.
“word” here is A to Z, a to z, and hyphen [-] and lowline [_], independent of syntax table.

URL `http://xahlee.info/emacs/emacs/modernization_isearch.html'
Version: 2015-04-09"
  (interactive)
  (let (xp1 xp2)
    (if (region-active-p)
        (setq xp1 (region-beginning) xp2 (region-end))
      (save-excursion
        (skip-chars-backward "-_A-Za-z0-9")
        (setq xp1 (point))
        (right-char)
        (skip-chars-forward "-_A-Za-z0-9")
        (setq xp2 (point))))
    (setq mark-active nil)
    (when (< xp1 (point))
      (goto-char xp1))
    (isearch-mode t)
    (isearch-yank-string (buffer-substring-no-properties xp1 xp2))))

;; https://github.com/xahlee/xah-fly-keys/blob/master/xah-fly-keys.el
(defun xah-get-bounds-of-block ()
  (let ( $p1 $p2 ($blankRegex "\n[ \t]*\n"))
    (save-excursion
      (setq $p1 (if (re-search-backward $blankRegex nil 1)
                    (goto-char (match-end 0))
                  (point)))
      (setq $p2 (if (re-search-forward $blankRegex nil 1)
                    (match-beginning 0)
                  (point))))
    (cons $p1 $p2 )))

(defun xah-get-bounds-of-block-or-region ()
  (if (region-active-p)
      (cons (region-beginning) (region-end))
    (xah-get-bounds-of-block)))

;; I removed the face highlight
(defun xah-add-space-after-comma ()
  (interactive)
  (let ($p1 $p2)
    (let (($bds (xah-get-bounds-of-block-or-region))) (setq $p1 (car $bds) $p2 (cdr $bds)))
    (save-restriction
      (narrow-to-region $p1 $p2)
      (goto-char (point-min))
      (while (re-search-forward ",\\b" nil t)
        (replace-match ", ")))))

;; Tweak: only delete blank lines without joining them
(defun xah-delete-blank-lines ()
  (interactive)
  (let ($p3 $p4)
    (skip-chars-backward "\n")
    (setq $p3 (point))
    (skip-chars-forward "\n")
    (previous-line)
    (setq $p4 (point))
    (delete-region $p3 $p4)))

(defun xah-space-to-newline ()
  (interactive)
  (let* (($bds (xah-get-bounds-of-block-or-region))
         ($p1 (car $bds))
         ($p2 (cdr $bds)))
    (save-restriction
      (narrow-to-region $p1 $p2)
      (goto-char (point-min))
      (while (re-search-forward " +" nil t)
        (replace-match "\n")))))

(defun xah-cycle-hyphen-lowline-space (&optional Begin End)
  (interactive)
  (let* ($p1
         $p2
         ($charArray ["-" "_" " "])
         ($n (length $charArray))
         ($regionWasActive-p (region-active-p))
         ($nowState (if (eq last-command this-command) (get 'xah-cycle-hyphen-lowline-space 'state) 0))
         ($changeTo (elt $charArray $nowState)))
    (if (and Begin End)
        (setq $p1 Begin $p2 End)
      (if (region-active-p)
          (setq $p1 (region-beginning) $p2 (region-end))
        (let (($skipChars "^\"<>(){}[]“”‘’‹›«»「」『』【】〖〗《》〈〉〔〕（）"))
          (skip-chars-backward $skipChars (line-beginning-position))
          (setq $p1 (point))
          (skip-chars-forward $skipChars (line-end-position))
          (setq $p2 (point))
          (set-mark $p1))))
    (save-excursion
      (save-restriction
        (narrow-to-region $p1 $p2)
        (goto-char (point-min))
        (while (re-search-forward (elt $charArray (% (+ $nowState 2) $n)) (point-max) 1)
          (replace-match $changeTo t t))))
    (when (or (string-equal $changeTo " ") $regionWasActive-p)
      (goto-char $p2)
      (set-mark $p1)
      (setq deactivate-mark nil))
    (put 'xah-cycle-hyphen-lowline-space 'state (% (+ $nowState 1) $n)))
  (set-transient-map (let (($kmap (make-sparse-keymap))) (define-key $kmap (kbd "-") this-command) $kmap)))

(defun xah-new-empty-buffer ()
  (interactive)
  (let (($buf (generate-new-buffer "untitled")))
    (switch-to-buffer $buf)
    (funcall initial-major-mode)
    $buf))

;; http://xahlee.info/emacs/emacs/elisp_insert-date-time.html
(defun xah-choose-and-insert-date ()
  (interactive)
  (let (($style
         (string-to-number
          (substring
           (completing-read
            "Style:"
            '("1 → yyyy-mm-dd"
              "2 → yyyy-mm-dd, A"
              "3 → dd-mm-yyyy"
              "4 → dd-mm-yyyy, A"
              "5 → yyyymmddHHMMSS"
              "6 → yyyy-mm-ddTHH:MM:SS"
              "7 → yyyy-mm-dd HH:MM:SS"
              "8 → A, dd B yyyy"
              "9 → dd B yyyy"
              )) 0 1))))
    (when (use-region-p) (delete-region (region-beginning) (region-end)))
    (insert
     (cond
      ((= $style 1)
       (format-time-string "%Y-%m-%d"))
      ((= $style 2)
       (format-time-string "%Y-%m-%d, %A"))
      ((= $style 3)
       (format-time-string "%d-%m-%Y"))
      ((= $style 4)
       (format-time-string "%d-%m-%Y, %A"))
      ((= $style 5)
       (replace-regexp-in-string ":" "" (format-time-string "%Y%m%d%T")))
      ((= $style 6)
       (format-time-string "%Y-%m-%dT%T"))
      ((= $style 7)
       (format-time-string "%Y-%m-%d %T"))
      ((= $style 8)
       (format-time-string "%A, %d %B %Y"))
      ((= $style 9)
       (format-time-string "%d %B %Y"))))))

(define-key evil-normal-state-map (kbd "fa") 'xah-forward-right-bracket)
(define-key evil-normal-state-map (kbd "fb") 'xah-backward-left-bracket)
(define-key evil-normal-state-map (kbd "fl") 'xah-forward-punct)
(define-key evil-normal-state-map (kbd "fh") 'xah-backward-punct)
(define-key evil-normal-state-map (kbd "fq") 'xah-forward-quote-smart)
(define-key evil-normal-state-map (kbd "fw") 'xah-search-current-word)
(define-key evil-visual-state-map (kbd "fa") 'xah-forward-right-bracket)
(define-key evil-visual-state-map (kbd "fb") 'xah-backward-left-bracket)
(define-key evil-visual-state-map (kbd "fl") 'xah-forward-punct)
(define-key evil-visual-state-map (kbd "fh") 'xah-backward-punct)

(provide 'init-xah)
;;;;; init-xah.el ends here
