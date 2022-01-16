;;; init-function.el --- Useful functions -*- lexical-binding: t -*-

;; list all installed package
;; https://github.com/raxod502/straight.el/issues/262
(defun p-list-installed-packages ()
  (interactive)
  (require 'magit)
  (let ((magit-repository-directories
	 (list (cons (straight--repos-dir) 1))))
    (magit-list-repositories)))

;; rename the current file
;; https://github.com/purcell/emacs.d/blob/master/lisp/init-utils.el
(defun p-rename-this-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (unless filename
      (error "Buffer '%s' is not visiting a file!" name))
    (progn
      (when (file-exists-p filename)
        (rename-file filename new-name 1))
      (set-visited-file-name new-name)
      (rename-buffer new-name))))

;; find file in my config
(defun p-find-file-in-config ()
  (interactive)
  (let ((default-directory (file-truename (file-name-directory user-init-file))))
    (call-interactively 'find-file)))

;; reveal file in Finder
;; https://github.com/xuchunyang/emacs.d/blob/master/lisp/chunyang-mac.el
(defun p-reveal-file-in-finder (file)
  (interactive (list (or (buffer-file-name) ".")))
  (do-applescript
   (format (concat "tell application \"Finder\"\n"
		   "	activate\n"
		   "	reveal POSIX file \"%s\"\n"
		   "end tell")
	   (expand-file-name file))))

;; google search
;; https://emacsredux.com/blog/2013/03/28/google/
(defun p-google-search ()
  (interactive)
  (browse-url
   (concat
    "http://www.google.com/search?ie=utf-8&oe=utf-8&q="
    (url-hexify-string (if mark-active
			   (buffer-substring (region-beginning) (region-end))
			 (read-string "Google: "))))))

;; youtube search
;; https://emacsredux.com/blog/2013/08/26/search-youtube/
(defun p-youtube-search ()
  (interactive)
  (browse-url
   (concat
    "http://www.youtube.com/results?search_query="
    (url-hexify-string (if mark-active
			   (buffer-substring (region-beginning) (region-end))
			 (read-string "Search YouTube: "))))))

;; keybindings
(with-eval-after-load 'evil
  (general-create-definer p-space-leader-def
    :prefix "SPC"
    :states '(normal visual))
  (p-space-leader-def
    "f"  '(:ignore t :which-key "file")
    "fp" '(p-find-file-in-config :which-key "find config file")
    "fR" '(p-rename-this-file-and-buffer :which-key "rename file")
    "s"  '(:ignore t :which-key "search")
    "sg" '(p-google-search :which-key "search on google")
    "sy" '(p-youtube-search :which-key "search on youtube")
    "t"  '(:ignore t :which-key "toggle")
    "tr" '(p-reveal-file-in-finder :which-key "reveal file in finder")
    "p"  '(:ignore t :which-key "projects and packages")
    "pS" '(p-list-installed-packages :which-key "list installed packages")))

(provide 'init-function)
;;; init-function.el ends here
