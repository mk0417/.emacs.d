;;;;; init-vertico.el --- Vertico -*- lexical-binding: t -*-

;;; Install packages
(straight-use-package '(vertico :files ("*.el" "extensions/*.el")))

(require 'vertico)
;; (require 'vertico-directory)

(setq vertico-cycle t)
(setq vertico-scroll-margin 0)
(setq vertico-resize nil)

(vertico-mode 1)
(vertico-multiform-mode)

;; sort directories first
(defun sort-directories-first (files)
  (setq files (vertico-sort-history-alpha files))
  (nconc (seq-filter (lambda (x) (string-suffix-p "/" x)) files)
         (seq-remove (lambda (x) (string-suffix-p "/" x)) files)))

(setq vertico-multiform-categories
      '((symbol (vertico-sort-function . vertico-sort-alpha))
        (file (vertico-sort-function . sort-directories-first))
        (t (vertico-sort-function . vertico-sort-history-alpha))))

(define-key vertico-map (kbd "C-n") 'vertico-next)
(define-key vertico-map (kbd "C-p") 'vertico-previous)
(define-key vertico-map (kbd "M-h") 'vertico-directory-up)

(provide 'init-vertico)
;;;;; init-vertico.el ends here
