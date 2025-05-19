;;; init-functions.el --- My functions -*- lexical-binding: t -*-

(defun p-mark-paragraph ()
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

;; https://github.com/xahlee/xah-fly-keys/blob/master/xah-fly-keys.el
(defun p-open-in-external-app (&optional Fname)
  (interactive)
  (let (xfileList xdoIt)
    (setq xfileList
          (if Fname
              (list Fname)
            (if (eq major-mode 'dired-mode)
                (dired-get-marked-files)
              (list buffer-file-name))))
    (setq xdoIt (if (<= (length xfileList) 10) t (y-or-n-p "Open more than 10 files? ")))
    (when xdoIt
      (cond
       ((eq system-type 'windows-nt)
        (let ((xoutBuf (get-buffer-create "*xah open in external app*"))
              (xcmdlist (list "PowerShell" "-Command" "Invoke-Item" "-LiteralPath")))
          (mapc
           (lambda (x)
             (message "%s" x)
             (apply 'start-process (append (list "xah open in external app" xoutBuf) xcmdlist (list (format "'%s'" (if (string-match "'" x) (replace-match "`'" t t x) x))) nil)))
           xfileList)
          )
        )
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

(defun p-open-current-dir-quarto-html-in-browser()
  (interactive)
  (let ((file (file-name-sans-extension (file-name-nondirectory (buffer-file-name)))))
    (p-open-in-external-app (concat file ".html"))))

(defun p-add-space-around ()
  (interactive)
  (when-let* ((char (char-after))) ;; Get the character at point
    (save-excursion
      (while (looking-back "\\s-" 1) (delete-char -1))
      (insert " ")
      (forward-char 1)
      (while (looking-at "\\s-") (delete-char 1))
      (insert " "))))

(defun p-remove-space-around ()
  (interactive)
  (when-let* ((char (char-after)))
    (save-excursion
      (while (looking-back "\\s-" 1) (delete-char -1))
      (forward-char 1)
      (while (looking-at "\\s-") (delete-char 1)))))

(defvar p-brackets '( "“”" "()" "[]" "{}"))
(defconst p-left-brackets (mapcar (lambda (x) (substring x 0 1)) p-brackets))
(defconst p-right-brackets (mapcar (lambda (x) (substring x 1 2)) p-brackets))

(defun p-move-to-prev-bracket ()
  (interactive)
  (re-search-backward (regexp-opt p-left-brackets) nil t))

(defun p-move-to-next-bracket ()
  (interactive)
  (re-search-forward (regexp-opt p-left-brackets) nil t))

(defun p-insert-dash-line ()
  (interactive)
  (insert "\n------------------------------\n")
  (backward-char)
  (comment-line 1))

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

(defun p-format-to-left ()
  (interactive)
  (backward-paragraph)
  (skip-chars-forward "\n\t ")
  (xah-shrink-whitespaces)
  (let ((start (point)))
    (forward-paragraph)
    (skip-chars-backward "\n\t ")
    (indent-region start (point))))

(defun p-flip-quotes ()
  (interactive)
  (if (use-region-p)
      (let ((beg (region-beginning))
            (end (region-end)))
        (save-excursion
          (goto-char beg)
          (while (re-search-forward "\\(['\"]\\)\\(.*?\\)\\1" end t)
            (let* ((quote (match-string 1))
                   (content (match-string 2))
                   (new-quote (if (string= quote "\"") "'" "\"")))
              (replace-match (concat new-quote content new-quote) t t)))))
    (let* ((syntax (syntax-ppss))
           (start (nth 8 syntax)))
      (cond
       ((nth 3 syntax)
        (let* ((quote-char (char-after start))
               (new-quote (if (eq quote-char ?\") "'" "\""))
               (end (save-excursion (goto-char start) (forward-sexp) (point))))
          (save-excursion
            (goto-char start)
            (delete-char 1)
            (insert new-quote)
            (goto-char (1- end))
            (delete-char 1)
            (insert new-quote))))
       (t
        (save-excursion
          (let ((found nil))
            (when (re-search-backward "'" (line-beginning-position) t)
              (let ((left (point)))
                (forward-char)
                (when (re-search-forward "'" (line-end-position) t)
                  (let ((right (point)))
                    (setq found t)
                    (goto-char left)
                    (delete-char 1)
                    (insert "\"")
                    (goto-char (1- right)) ;; adjust because left was changed
                    (delete-char 1)
                    (insert "\"")))))
            (unless found
              (message "Not inside or near a recognizable quote.")))))))))

(defun p-flip-brackets ()
  (interactive)
  (let* ((syntax (syntax-ppss)))
    (if-let* ((start (nth 1 syntax)))
        (let* ((open-char (char-after start))
               (close-pos (save-excursion
                            (goto-char start)
                            (forward-sexp)
                            (point)))
               (close-char (char-before close-pos)))
          (when (and (memq open-char '(?\( ?\[))
                     (memq close-char '(?\) ?\])))
            (let ((new-open (if (eq open-char ?\() "["
                              (if (eq open-char ?\[) "(")))
                  (new-close (if (eq close-char ?\)) "]"
                               (if (eq close-char ?\]) ")"))))
              (save-excursion
                (goto-char start)
                (delete-char 1)
                (insert new-open)
                (goto-char (1- close-pos)) ;; end moves because of earlier insert
                (delete-char 1)
                (insert new-close)))))
      (message "Not inside parentheses or brackets."))))

(provide 'init-functions)
