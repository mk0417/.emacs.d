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

;;; Query replace many
;; https://tony-zorman.com/posts/query-replace/2022-08-06-query-replace-many.html
(autoload 's-join "s")

(defun p-get-queries (&optional pairs)
  (-let* (((from to delim arg)
           (query-replace-read-args
            (s-join " "
                    (-non-nil
                     (list "Query replace many"
                           (cond ((eq current-prefix-arg '-) "backward")
                                 (current-prefix-arg "word"))
                           (when (use-region-p) "in region"))))
            nil))
          (from-to (cons (regexp-quote from)
                         (s-replace "\\" "\\\\" to))))
    (if (-contains? pairs from-to)
        (list pairs delim arg)
      (p-get-queries (push from-to pairs)))))

(defun p-query-replace-many
    (pairs &optional delimited start end backward region-noncontiguous-p)
  (interactive
   (let ((common (p-get-queries)))
     (list (nth 0 common) (nth 1 common)
           (if (use-region-p) (region-beginning))
           (if (use-region-p) (region-end))
           (nth 2 common)
           (if (use-region-p) (region-noncontiguous-p)))))
  (perform-replace
   (concat "\\(?:" (mapconcat #'car pairs "\\|") "\\)")
   (cons (lambda (pairs _count)
           (cl-loop for (from . to) in pairs
                    when (string-match from (match-string 0))
                    return to))
         pairs)
   :query :regexp
   delimited nil nil start end backward region-noncontiguous-p))

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
  (define-key evil-normal-state-map (kbd "gom") 'p-query-replace-many)
  (define-key evil-visual-state-map (kbd "gom") 'p-query-replace-many)

  (general-create-definer p-space-leader-def
    :prefix "SPC"
    :states '(normal visual))
  (p-space-leader-def
    "f"  '(:ignore t :which-key "file")
    "fp" '(p-find-file-in-config :which-key "find config file")
    "fl" '(p-find-file-in-log :which-key "find log file")
    "fR" '(p-rename-this-file-and-buffer :which-key "rename file")
    "b" '(:ignore t :which-key "buffer")
    "ba" '(p-switch-to-messages :which-key "switch to messages")
    "§" '(p-switch-to-previous-buffer :which-key "switch to previous buffer")
    "`" '(p-switch-to-previous-buffer :which-key "switch to previous buffer")
    "s"  '(:ignore t :which-key "search")
    "sg" '(p-google-search :which-key "search on google")
    "sy" '(p-youtube-search :which-key "search on youtube")
    "t"  '(:ignore t :which-key "toggle")
    "tr" '(p-reveal-file-in-finder :which-key "reveal file in finder")))

(provide 'init-utils)
;;;;; init-utils.el ends here
