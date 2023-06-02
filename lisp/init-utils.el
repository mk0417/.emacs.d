;;;;; init-utils.el --- Functions -*- lexical-binding: t -*-

;;; Rename the current file
;; https://github.com/purcell/emacs.d/blob/master/lisp/init-utils.el
(defun p-rename-this-file-and-buffer (new-name)
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

;;; Copy current file path
(defun p-copy-file-path ()
  (interactive)
  (kill-new (buffer-file-name)))

;;; Copy current file name
(defun p-copy-file-name ()
  (interactive)
  (kill-new (buffer-name)))

;;; Find file in my config
(defun p-find-file-in-config ()
  (interactive)
  (let ((default-directory (file-truename (file-name-directory user-init-file))))
    (call-interactively 'find-file)))

;;; Open my log file
(defun p-find-file-in-log ()
  (interactive)
  (let ((default-directory (file-truename (file-name-directory (expand-file-name "~/Dropbox/peng_log/")))))
    (call-interactively 'find-file)))

;;; Functions to switch buffers
(defun p-switch-to-messages ()
  (interactive)
  (switch-to-buffer "*Messages*"))

(defun p-switch-to-previous-buffer ()
  (interactive)
  (switch-to-buffer nil))

(defun p-format-indent-in-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

;;; Make backup copy
;; http://xahlee.info/emacs/emacs/elisp_make-backup.html
(defun p-make-backup ()
  (interactive)
  (let ((xfname buffer-file-name)
        (xdateTimeFormat "%Y-%m-%d-%H%M%S"))
    (if xfname
        (let ((xbackupName
               (concat xfname "_" (format-time-string xdateTimeFormat))))
          (copy-file xfname xbackupName t)
          (message (concat "Backup saved at: " xbackupName)))
      (if (eq major-mode 'dired-mode)
          (progn
            (mapc (lambda (xx)
                    (let ((xbackupName
                           (concat xx "_" (format-time-string xdateTimeFormat))))
                      (copy-file xx xbackupName t)))
                  (dired-get-marked-files))
            (revert-buffer))
        (user-error "%s: buffer not file nor dired" real-this-command)))))

;;; Reveal file in Finder
;; https://github.com/xuchunyang/emacs.d/blob/master/lisp/chunyang-mac.el
(defun p-reveal-file-in-finder (file)
  (interactive (list (or (buffer-file-name) ".")))
  (do-applescript
   (format (concat "tell application \"Finder\"\n"
                   "	activate\n"
                   "	reveal POSIX file \"%s\"\n"
                   "end tell")
           (expand-file-name file))))

;;; Google search
;; https://emacsredux.com/blog/2013/03/28/google/
(defun p-google-search ()
  (interactive)
  (browse-url
   (concat
    "http://www.google.com/search?ie=utf-8&oe=utf-8&q="
    (url-hexify-string (if mark-active
                           (buffer-substring (region-beginning) (region-end))
                         (read-string "Google: "))))))

;;; Youtube search
;; https://emacsredux.com/blog/2013/08/26/search-youtube/
(defun p-youtube-search ()
  (interactive)
  (browse-url
   (concat
    "http://www.youtube.com/results?search_query="
    (url-hexify-string (if mark-active
                           (buffer-substring (region-beginning) (region-end))
                         (read-string "Search YouTube: "))))))

;;; Keybindings
(with-eval-after-load 'evil
  (define-key evil-normal-state-map (kbd "goi") 'p-format-indent-in-buffer)

  (general-create-definer p-space-leader-def
    :prefix "SPC"
    :states '(normal visual))
  (p-space-leader-def
    "f"  '(:ignore t :which-key "file")
    "fp" '(p-find-file-in-config :which-key "find config file")
    "fl" '(p-find-file-in-log :which-key "find log file")
    "fR" '(p-rename-this-file-and-buffer :which-key "rename file")
    "fI" '(p-copy-file-path :which-key "copy file path")
    "fi" '(p-copy-file-name :which-key "copy file name")
    "fb" '(p-make-backup :which-key "make backup file")
    "b" '(:ignore t :which-key "buffer")
    "ba" '(p-switch-to-messages :which-key "switch to messages")
    "§" '(p-switch-to-previous-buffer :which-key "switch to previous buffer")
    "`" '(p-switch-to-previous-buffer :which-key "switch to previous buffer")
    "s"  '(:ignore t :which-key "search")
    "sg" '(p-google-search :which-key "search on google")
    "st" '(p-youtube-search :which-key "search on youtube")
    "t"  '(:ignore t :which-key "toggle")
    "tr" '(p-reveal-file-in-finder :which-key "reveal file in finder")))

(provide 'init-utils)
;;;;; init-utils.el ends here
