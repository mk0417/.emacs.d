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

;;; Insert date
;; http://xahlee.info/emacs/emacs/elisp_insert-date-time.html
(defun p-choose-and-insert-date ()
  (interactive)
  (let (($style
         (string-to-number
          (substring
           (completing-read
            "Style:"
            '("1 → yyyy-mm-dd"
              "2 → yyyy-mm-dd, A"
              "3 → dd-mm-yyyy"
              "4 → dd-mm-yyyy, A"
              "5 → yyyymmddHHMMSS"
              "6 → yyyy-mm-ddTHH:MM:SS"
              "7 → yyyy-mm-dd HH:MM:SS"
              "8 → A, dd B yyyy"
              "9 → dd B yyyy"
              )) 0 1))))
    (when (use-region-p) (delete-region (region-beginning) (region-end)))
    (insert
     (cond
      ((= $style 1)
       (format-time-string "%Y-%m-%d"))
      ((= $style 2)
       (format-time-string "%Y-%m-%d, %A"))
      ((= $style 3)
       (format-time-string "%d-%m-%Y"))
      ((= $style 4)
       (format-time-string "%d-%m-%Y, %A"))
      ((= $style 5)
       (replace-regexp-in-string ":" "" (format-time-string "%Y%m%d%T")))
      ((= $style 6)
       (format-time-string "%Y-%m-%dT%T"))
      ((= $style 7)
       (format-time-string "%Y-%m-%d %T"))
      ((= $style 8)
       (format-time-string "%A, %d %B %Y"))
      ((= $style 9)
       (format-time-string "%d %B %Y"))))))

;;; Keybindings
(with-eval-after-load 'evil
  (general-create-definer p-space-leader-def
    :prefix "SPC"
    :states '(normal visual))
  (p-space-leader-def
    "e"  '(:ignore t :which-key "editing")
    "el" '(p-choose-and-insert-date :which-key "choose and insert date")
    "f"  '(:ignore t :which-key "file")
    "fp" '(p-find-file-in-config :which-key "find config file")
    "fl" '(p-find-file-in-log :which-key "find log file")
    "fR" '(p-rename-this-file-and-buffer :which-key "rename file")
    "s"  '(:ignore t :which-key "search")
    "sg" '(p-google-search :which-key "search on google")
    "sy" '(p-youtube-search :which-key "search on youtube")
    "t"  '(:ignore t :which-key "toggle")
    "tr" '(p-reveal-file-in-finder :which-key "reveal file in finder")))

(provide 'init-utils)
;;;;; init-utils.el ends here
