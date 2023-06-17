;;;;; init-xah.el --- Functions from Xah Lee -*- lexical-binding: t -*-

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

(provide 'init-xah)
;;;;; init-xah.el ends here
