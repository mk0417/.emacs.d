;;;;; init-default.el --- Better default -*- lexical-binding: t -*-

;;; package
(straight-use-package 'which-key)
(straight-use-package 'diminish)

;; UTF-8 as default encoding
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

;;; recentf-mode
(add-hook 'after-init-hook 'recentf-mode)
(setq-default recentf-max-saved-items 50
              recentf-exclude `("/Applications/Emacs.app/Contents/Resources/lisp/" "/tmp/" "/ssh:"))

;;; increase the amount of data which Emacs reads from the process
(setq read-process-output-max (* 1024 1024))

;;; do not compact font caches during GC
;; prefer speed at the cost of memory usage
(setq inhibit-compacting-font-caches t)

;;; disable bell sound
(setq ring-bell-function 'ignore)

;;; y or n
(fset 'yes-or-no-p 'y-or-n-p)

;;; revert-mode
(global-auto-revert-mode 1)

;;; custom file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

;;; don't create .# files
(setq create-lockfiles nil)

;;; no backup file
(setq make-backup-files nil)

;;; do not autosave
(setq auto-save-default nil)

;;; do not center when scrolling after last visible line
(setq scroll-conservatively 101)

;;; tab and indent
(setq tab-always-indent 'complete)
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

;;; split new buffer on the right by default
(setq split-height-threshold nil)

;;; trash
(setq delete-by-moving-to-trash t)

;;; mac tweaks
(when (memq window-system '(mac ns))
  ;; enable left option + 3 to type #
  (setq ns-option-modifier 'nil)
  (setq ns-right-option-modifier 'meta)
  ;; enable push back option after deleting a file
  ;; https://christiantietze.de/posts/2021/06/emacs-trash-file-macos/
  (setq trash-directory "~/.Trash")
  (defun system-move-file-to-trash (path)
    (shell-command (concat "trash -vF \"" path "\"" "| sed -e 's/^/Trashed: /'")
                   nil
                   "*Trash Error Buffer*")))

;;; auto-fill column
(defun p-text-mode-auto-fill ()
  (setq-local fill-column 100)
  (auto-fill-mode)
  (diminish 'auto-fill-function))

;;; which-key
(setq-default which-key-idle-delay 0.8)
(which-key-mode)
(diminish 'which-key-mode)

;;; eldoc
(diminish 'eldoc-mode)

;;; keybindings
(global-set-key (kbd "C-x k") 'kill-this-buffer)
(global-set-key (kbd "C-x K") 'kill-buffer-and-window)
(global-set-key (kbd "C-c C-e") 'occur-edit-mode)
(global-set-key (kbd "M-i") #'forward-paragraph)
(global-set-key (kbd "M-o") #'backward-paragraph)
(global-set-key (kbd "C-c r") #'query-replace-regexp)
(define-key minibuffer-local-map (kbd "C-k") 'delete-backward-char)
(define-key minibuffer-local-map (kbd "C-w") 'backward-kill-word)

(provide 'init-default)
;;;;; init-default.el ends here
