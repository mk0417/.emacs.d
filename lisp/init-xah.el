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

;;; Keybindings
(with-eval-after-load 'evil
  (general-create-definer p-space-leader-def
    :prefix "SPC"
    :states '(normal visual))
  (p-space-leader-def
    "x"  '(:ignore t :which-key "xah editing")
    "xd" '(xah-choose-and-insert-date :which-key "choose and insert date")
    "xs" '(xah-add-space-after-comma :which-key "add space after comma")))

(provide 'init-xah)
;;;;; init-xah.el ends here
