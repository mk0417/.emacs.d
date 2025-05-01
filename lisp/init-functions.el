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
  (re-search-forward (regexp-opt p-right-brackets) nil t))

(defun p-insert-dash-line ()
  (interactive)
  (insert "\n------------------------------\n")
  (backward-char)
  (comment-line 1))

(provide 'init-functions)
