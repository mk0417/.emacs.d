;;; init-func.el --- Functions -*- lexical-binding: t -*-

(require 'prot-pair)
(require 'prot-comment)

;; Mark paragraph
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

;; Mark paragraph below from current position
(defun p-mark-paragraph-below ()
  (interactive)
  (save-excursion
    (mark-paragraph)))

;; Mark function
(defun p-mark-defun ()
  (interactive)
  (end-of-defun)
  (skip-chars-backward " \t\n")
  (set-mark (point))
  (beginning-of-defun)
  (skip-chars-forward " \t\n"))

;; Mark line (from first char to last char in a line)
(defun p-mark-line-non-blank ()
  (interactive)
  (let ((start (progn (back-to-indentation) (point)))
        (end (progn (end-of-line) (skip-chars-backward " \t") (point))))
    (goto-char start)
    (set-mark end)))

;; Insert surround pairs
(defun p-insert-surround-parentheses ()
  (interactive)
  (prot-pair-insert '(?\( . ?\))))

(defun p-insert-surround-bracket ()
  (interactive)
  (prot-pair-insert '(?\[ . ?\])))

(defun p-insert-surround-curly-bracket ()
  (interactive)
  (prot-pair-insert '(?{ . ?})))

(defun p-insert-surround-single-quote ()
  (interactive)
  (prot-pair-insert '(?' . ?')))

(defun p-insert-surround-double-quotes ()
  (interactive)
  (prot-pair-insert '(?\" . ?\")))

;; Find file in my config
(defun p-find-file-in-config ()
  (interactive)
  (let ((default-directory (file-truename (file-name-directory user-init-file))))
    (call-interactively 'find-file)))

;; Find file in my notes
(defun p-find-file-in-notes ()
  (interactive)
  (let ((default-directory (file-truename (file-name-directory (expand-file-name "~/Dropbox/peng_notes/")))))
    (call-interactively 'find-file)))

;; Find file in my org
(defun p-find-file-in-org ()
  (interactive)
  (let ((default-directory (file-truename (file-name-directory (expand-file-name "~/Dropbox/org/")))))
    (call-interactively 'find-file)))

;; Find file in my log
(defun p-find-file-in-log ()
  (interactive)
  (let ((default-directory (file-truename (file-name-directory (expand-file-name "~/Dropbox/peng_log/")))))
    (call-interactively 'find-file)))

;; Functions to switch buffers
(defun p-switch-to-messages ()
  (interactive)
  (switch-to-buffer "*Messages*"))

(defun p-switch-to-previous-buffer ()
  (interactive)
  (switch-to-buffer nil))

;; Indent whole buffer
(defun p-format-indent-in-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

;; Delete to tab
(defun p-delete-backward-to-tab ()
  (interactive)
  (when (evil-normal-state-p)
    (beginning-of-line-text)
    (kill-line 0)
    (insert "    "))
  (when (evil-insert-state-p)
    (kill-line 0)
    (insert "    ")))

;; Clear line
(defun p-clear-line ()
  (interactive)
  (beginning-of-line)
  (kill-line))

;; Remove extra spaces
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

;; Deletes text between commas
(defun p-delete-between-commas ()
  (interactive)
  (let ((start (progn (search-backward "," nil t) (forward-char) (skip-chars-forward " \t") (point)))
        (end (progn (search-forward "," nil t) (backward-char) (skip-chars-backward " \t") (point))))
    (delete-region start end)))

;; Select text between commas
(defun p-select-between-commas ()
  (interactive)
  (let ((start (progn (search-backward "," nil t) (forward-char) (skip-chars-forward " \t") (point)))
        (end (progn (search-forward "," nil t) (backward-char) (skip-chars-backward " \t") (point))))
    (set-mark start)
    (goto-char (- end 1))))

;; kill-sexp and insert
(defun p-kill-sexp-and-insert ()
  (interactive)
  (kill-sexp)
  (evil-insert 0))

;; http://yummymelon.com/devnull/moving-text-elegantly-in-emacs.html
(defun p-move-sexp-forward ()
  "Move balanced expression (sexp) to the right of point forward one sexp.
Point must be at the beginning of balanced expression (sexp)."
  (interactive)
  (forward-sexp 1)
  (transpose-sexps 1)
  (forward-sexp -1))

(defun p-move-sexp-backward ()
  "Move balanced expression (sexp) to the right of point backward one sexp.
Point must be at the beginning of balanced expression (sexp)."
  (interactive)
  (transpose-sexps 1)
  (forward-sexp -2))

;; Create a scratch file
(defun p-create-scratch-file ()
  "Prompts to create or open a scratch file based on chosen file type."
  (interactive)
  (let* ((file-types '(("py" . "scratch.py")
                       ("txt" . "scratch.txt")))
         (chosen-type (completing-read "Choose file type: " (mapcar #'car file-types))))
    (if (string-empty-p chosen-type)
        (setq chosen-type (read-string "Enter file extension: ")))
    (let* ((default-directory (expand-file-name "~/Desktop/"))
           (file-name (cond ((assoc chosen-type file-types)
                             (cdr (assoc chosen-type file-types)))
                            ((not (string-empty-p chosen-type))
                             (concat "scratch." chosen-type))
                            (t nil)))
           (full-path (and file-name (concat default-directory file-name))))
      (when full-path
        (if (file-exists-p full-path)
            (find-file full-path)
          (progn
            (write-region "" nil full-path)
            (find-file full-path)
            (message "Created file '%s'." file-name)))
        (unless file-name
          (message "Invalid file type: '%s'. No file created." chosen-type))))))

;; Reveal file in Finder
;; https://github.com/xuchunyang/emacs.d/blob/master/lisp/chunyang-mac.el
(defun p-reveal-file-in-finder (file)
  (interactive (list (or (buffer-file-name) ".")))
  (do-applescript
   (format (concat "tell application \"Finder\"\n"
                   "	activate\n"
                   "	reveal POSIX file \"%s\"\n"
                   "end tell")
           (expand-file-name file))))

(provide 'init-func)
;;; init-func.el ends here
