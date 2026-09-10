;;; init-functions.el --- My functions -*- lexical-binding: t -*-

(defun p-read-env (name)
  (let ((env-file (expand-file-name "~/.env")))
    (when (file-readable-p env-file)
      (with-temp-buffer
        (insert-file-contents env-file)
        (let ((value nil))
          (dolist (line (split-string (buffer-string) "\n"))
            (when (string-match
                   (concat "^" (regexp-quote name) "=[ \t]*\\(.*\\)$")
                   line)
              (setq value (string-trim (match-string 1 line)))))
          value)))))

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

(defun p-insert-dash-line ()
  (interactive)
  (insert "\n------------------------------\n")
  (backward-char)
  (comment-line 1))

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

(defun p-consult-line-symbol-at-point ()
  (interactive)
  (consult-line (or (thing-at-point 'symbol))))

(provide 'init-functions)
