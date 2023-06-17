;;;;; init-snippet.el --- Snippet -*- lexical-binding: t -*-

;;; Install packages
(straight-use-package 'yasnippet)
(straight-use-package 'consult-yasnippet)

;;; Yasnippet
(setq yas-snippet-dirs '("~/Dropbox/snippet"))
(setq yas-also-auto-indent-first-line t)
(setq yas-verbosity 0)
(setq yas-triggers-in-field t)

(yas-global-mode 1)

(global-set-key (kbd "M-;") 'yas-expand)

;;; Abbrev
(set-default 'abbrev-mode t)

(setq abbrev-file-name "~/Dropbox/abbrev_defs")
(read-abbrev-file abbrev-file-name t)

;; https://www.emacswiki.org/emacs/AbbrevMode
;; http://xahlee.info/emacs/emacs/emacs_abbrev_no_space.html
(defun p-abbrev-no-space () t)
(put 'p-abbrev-no-space 'no-self-insert t)

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

(global-set-key (kbd "M-s-i") #'p-insert-global-abbrev)

(provide 'init-snippet)
;;;;; init-snippet.el ends here
