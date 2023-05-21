;;;;; init-abbrev.el --- Abbrev -*- lexical-binding: t -*-

;; http://xahlee.info/emacs/emacs/emacs_abbrev_mode.html
(set-default 'abbrev-mode t)
(diminish 'abbrev-mode)

(setq save-abbrevs nil)

(clear-abbrev-table global-abbrev-table)

;; http://xahlee.info/emacs/emacs/emacs_abbrev_no_space.html
(defun p-abbrev-no-space ()
  t)

(put 'p-abbrev-no-space 'no-self-insert t)

(define-abbrev-table 'global-abbrev-table
  '(
    ("xeq" "==" p-abbrev-no-space)
    ("xret" "return" p-abbrev-no-space)
    ("xhr" "--------------------------------------------------" p-abbrev-no-space)))

;; https://emacs-china.org/t/emacs-snippet-consult/24550/13
(defun p-emacs-completing-read (prompt collection &optional predicate require-match initial-input hist def inherit-input-method)
  (cl-flet ((assoc-list-p (obj) (and (listp obj) (consp (car obj)))))
    (let* ((choice
            (completing-read prompt collection predicate require-match initial-input hist def inherit-input-method))
           (results (cond
                     ((hash-table-p collection) (gethash choice collection))
                     ((assoc-list-p collection) (alist-get choice collection def nil 'equal))
                     (t choice))))
      (if results
          (if (listp results) (car results) results) choice))))

(defun p-global-abbrev-table-completions ()
  (let ((symbols (abbrev--table-symbols 'global-abbrev-table)) result)
    (dolist (symbol symbols)
      (push (cons (concat (symbol-name symbol) " " (propertize (symbol-value symbol) 'face 'shadow))
                  (symbol-value symbol))
            result))
    result))

(defun p-insert-global-abbrev ()
  (interactive)
  (insert (p-emacs-completing-read "Select: " (p-global-abbrev-table-completions))))

(provide 'init-abbrev)
;;;;; init-abbrev.el ends here
