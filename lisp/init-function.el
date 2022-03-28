;;;;; init-function.el --- Useful functions -*- lexical-binding: t -*-

;;; list all installed package
;; https://github.com/raxod502/straight.el/issues/262
(defun p-list-installed-packages ()
  (interactive)
  (require 'magit)
  (let ((magit-repository-directories
		 (list (cons (straight--repos-dir) 1))))
	(magit-list-repositories)))

;;; rename the current file
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

;;; find file in my config
(defun p-find-file-in-config ()
  (interactive)
  (let ((default-directory (file-truename (file-name-directory user-init-file))))
	(call-interactively 'find-file)))

;;; open my log file
(defun p-find-file-in-log ()
  (interactive)
  (let ((default-directory (file-truename (file-name-directory (expand-file-name "~/Dropbox/peng_log/")))))
	(call-interactively 'find-file)))

;;; reveal file in Finder
;; https://github.com/xuchunyang/emacs.d/blob/master/lisp/chunyang-mac.el
(defun p-reveal-file-in-finder (file)
  (interactive (list (or (buffer-file-name) ".")))
  (do-applescript
   (format (concat "tell application \"Finder\"\n"
				   "	activate\n"
				   "	reveal POSIX file \"%s\"\n"
				   "end tell")
		   (expand-file-name file))))

;;; google search
;; https://emacsredux.com/blog/2013/03/28/google/
(defun p-google-search ()
  (interactive)
  (browse-url
   (concat
	"http://www.google.com/search?ie=utf-8&oe=utf-8&q="
	(url-hexify-string (if mark-active
						   (buffer-substring (region-beginning) (region-end))
						 (read-string "Google: "))))))

;;; youtube search
;; https://emacsredux.com/blog/2013/08/26/search-youtube/
(defun p-youtube-search ()
  (interactive)
  (browse-url
   (concat
	"http://www.youtube.com/results?search_query="
	(url-hexify-string (if mark-active
						   (buffer-substring (region-beginning) (region-end))
						 (read-string "Search YouTube: "))))))

(defun p-beginning-of-line-or-block ()
  (interactive)
  (let (($p (point)))
	(if (or (equal (point) (line-beginning-position))
			(equal last-command this-command ))
		(if (re-search-backward "\n[\t\n ]*\n+" nil "NOERROR")
			(progn
			  (skip-chars-backward "\n\t ")
			  (forward-char ))
		  (goto-char (point-min)))
	  (progn
		(back-to-indentation)
		(when (eq $p (point))
		  (beginning-of-line))))))

(defun p-end-of-line-or-block ()
  (interactive)
  (if (or (equal (point) (line-end-position))
		  (equal last-command this-command ))
	  (progn
		(re-search-forward "\n[\t\n ]*\n+" nil "NOERROR" ))
	(end-of-line)))

(defun p-insert-num-list (start end format-string from)
  (interactive
   (list (region-beginning) (region-end)
		 (read-string "Number rectangle: "
					  (if (looking-back "^ *") "%d. " "%d"))
		 (read-number "From: " 1)))
  (save-excursion
	(goto-char start)
	(setq start (point-marker))
	(goto-char end)
	(setq end (point-marker))
	(goto-char start)
	(cl-loop with column = (current-column)
			 while (and (<= (point) end) (not (eobp)))
			 for i from from do
			 (move-to-column column t)
			 (insert (format format-string i))
			 (forward-line 1)))
  (goto-char start))

;; http://xahlee.info/emacs/emacs/emacs_jump_to_punctuations.html
(defun p-forward-to-equal ()
  (interactive)
  (re-search-forward "=" nil t)
  (left-char))

(defun p-backward-to-equal ()
  (interactive)
  (when (re-search-backward "=" nil t)
	(while (search-backward "=" (- (point) 1) t)
	  (left-char))))

;; http://xahlee.info/emacs/emacs/emacs_quote_lines.html
(defun xah-get-bounds-of-block ()
  (let ( $p1 $p2 ($blankRegex "\n[ \t]*\n"))
	(save-excursion
	  (setq $p1 (if (re-search-backward $blankRegex nil 1)
					(goto-char (match-end 0))
				  (point)))
	  (setq $p2 (if (re-search-forward $blankRegex nil 1)
					(match-beginning 0)
				  (point))))
	(cons $p1 $p2 )))

(defun xah-get-bounds-of-block-or-region ()
  (if (region-active-p)
	  (cons (region-beginning) (region-end))
	(xah-get-bounds-of-block)))

(defun p-quote-lines (@quoteL @quoteR @sep)
  (interactive
   (let (($brackets
		  '(
			"\"double\""
			"'single'"
			"(paren)"
			"{brace}"
			"[square]"
			"<greater>"
			"`emacs'"
			"`markdown`"
			"~tilde~"
			"=equal="
			"none"
			"other"
			)) $bktChoice $sep $sepChoice $quoteL $quoteR)
	 (setq $bktChoice (completing-read "Quote to use:" $brackets ))
	 (setq $sepChoice (completing-read "line separator:" '(  "," ";" "none" "other")))
	 (cond
	  ((string-equal $bktChoice "none")
	   (setq $quoteL "" $quoteR "" ))
	  ((string-equal $bktChoice "other")
	   (let (($x (read-string "Enter 2 chars, for begin/end quote:" )))
		 (setq $quoteL (substring-no-properties $x 0 1)
			   $quoteR (substring-no-properties $x 1 2))))
	  (t (setq $quoteL (substring-no-properties $bktChoice 0 1)
			   $quoteR (substring-no-properties $bktChoice -1))))
	 (setq $sep
		   (cond
			((string-equal $sepChoice "none") "")
			((string-equal $sepChoice "other") (read-string "Enter separator:" ))
			(t $sepChoice)))
	 (list $quoteL $quoteR $sep)))
  (let ( $p1 $p2 ($quoteL @quoteL) ($quoteR @quoteR) ($sep @sep))
	(let (($bds (xah-get-bounds-of-block-or-region))) (setq $p1 (car $bds) $p2 (cdr $bds)))
	(save-excursion
	  (save-restriction
		(narrow-to-region $p1 $p2)
		(goto-char (point-min))
		(catch 'EndReached
		  (while t
			(skip-chars-forward "\t ")
			(insert $quoteL)
			(end-of-line )
			(insert $quoteR $sep)
			(if (eq (point) (point-max))
				(throw 'EndReached t)
			  (forward-char))))))))

;; http://xahlee.info/emacs/emacs/emacs_shrink_whitespace.html
(defun p-delete-blank-lines ()
  (interactive)
  (let ($p1 $p2)
	(skip-chars-backward "\n")
	(setq $p1 (point))
	(skip-chars-forward "\n")
	(setq $p2 (point))
	(delete-region $p1 $p2))
  (insert " "))

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

;;; keybindings
(global-set-key (kbd "C-c h i") 'p-insert-num-list)

(with-eval-after-load 'evil
  (define-key evil-normal-state-map (kbd ";a") 'p-beginning-of-line-or-block)
  (define-key evil-normal-state-map (kbd ";e") 'p-end-of-line-or-block)
  (define-key evil-normal-state-map (kbd "fe") 'p-forward-to-equal)
  (define-key evil-normal-state-map (kbd "fE") 'p-backward-to-equal)

  (define-key evil-visual-state-map (kbd ";a") 'p-beginning-of-line-or-block)
  (define-key evil-visual-state-map (kbd ";e") 'p-end-of-line-or-block)
  (define-key evil-visual-state-map (kbd "fe") 'p-forward-to-equal)
  (define-key evil-visual-state-map (kbd "fE") 'p-backward-to-equal)

  (general-create-definer p-space-leader-def
    :prefix "SPC"
    :states '(normal visual))
  (p-space-leader-def
    "e"  '(:ignore t :which-key "editing")
	"eq" '(p-quote-lines :which-key "quote lines")
	"eb" '(p-delete-blank-lines :which-key "delete blank lines")
	"el" '(p-choose-and-insert-date :which-key "choose and insert date")
    "f"  '(:ignore t :which-key "file")
    "fp" '(p-find-file-in-config :which-key "find config file")
    "fl" '(p-find-file-in-log :which-key "find log file")
    "fR" '(p-rename-this-file-and-buffer :which-key "rename file")
    "s"  '(:ignore t :which-key "search")
    "sg" '(p-google-search :which-key "search on google")
    "sy" '(p-youtube-search :which-key "search on youtube")
    "t"  '(:ignore t :which-key "toggle")
    "tr" '(p-reveal-file-in-finder :which-key "reveal file in finder")
    "p"  '(:ignore t :which-key "projects and packages")
    "pS" '(p-list-installed-packages :which-key "list installed packages")))

(provide 'init-function)
;;;;; init-function.el ends here
