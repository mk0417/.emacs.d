;;; init-default.el --- Better default -*- lexical-binding: t -*-

;; package
(straight-use-package 'which-key)
(straight-use-package 'diminish)

;; initial scratch buffer message
(setq initial-scratch-message
      (concat ";; Hello Peng, welcome to EMACS and happy hacking\n"
	      (format ";; Emacs version: %s\n" (car (split-string emacs-version)))))

;; startup time
(add-hook 'emacs-startup-hook
	  (lambda ()
	    (message "*** Emacs loaded in %s."
		     (format "%.2f seconds"
			     (float-time
			      (time-subtract after-init-time before-init-time))))))

;; https://www.masteringemacs.org/article/speed-up-emacs-libjansson-native-elisp-compilation
(if (>= emacs-major-version 28)
    (setq native-comp-async-jobs-number 6
	  native-comp-async-report-warnings-errors nil
	  compilation-scroll-output t
	  load-prefer-newer t))

;; recentf-mode
(add-hook 'after-init-hook 'recentf-mode)
(setq-default recentf-max-saved-items 50
	      recentf-exclude `("/Applications/Emacs.app/Contents/Resources/lisp/" "/tmp/" "/ssh:"))

;; increase the amount of data which Emacs reads from the process
(setq read-process-output-max (* 1024 1024))

;; do not compact font caches during GC
;; prefer speed at the cost of memory usage
(setq inhibit-compacting-font-caches t)

;; disable bell sound
(setq ring-bell-function 'ignore)

;; y or n
(fset 'yes-or-no-p 'y-or-n-p)

;; custom file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

;; don't create .# files
(setq create-lockfiles nil)

;; no backup file
(setq make-backup-files nil)

;; do not autosave
(setq auto-save-default nil)

;; do not center when scrolling after last visible line
(setq scroll-conservatively 101)

;; trash
(setq delete-by-moving-to-trash t)

;; mac tweaks
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

;; auto-fill column
(defun p-text-mode-auto-fill ()
  (setq-local fill-column 100)
  (auto-fill-mode))

;; which-key
(setq-default which-key-idle-delay 0.8)
(which-key-mode 1)
(diminish 'which-key-mode)

;; eldoc
(diminish 'eldoc-mode)

;; keybindings
(global-set-key (kbd "C-x k") 'kill-this-buffer)
(global-set-key (kbd "C-x K") 'kill-buffer-and-window)
(global-set-key (kbd "C-c C-e") 'occur-edit-mode)
(global-set-key (kbd "M-i") #'forward-paragraph)
(global-set-key (kbd "M-o") #'backward-paragraph)
(define-key minibuffer-local-map (kbd "C-k") 'delete-backward-char)
(define-key minibuffer-local-map (kbd "C-w") 'backward-kill-word)

(provide 'init-default)
;;; init-default.el ends here
