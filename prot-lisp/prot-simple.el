;;; prot-simple.el --- Common commands for my dotemacs -*- lexical-binding: t -*-

;; Copyright (C) 2020-2023  Protesilaos Stavrou

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; URL: https://protesilaos.com/emacs/dotemacs
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Common commands for my Emacs: <https://protesilaos.com/emacs/dotemacs/>.
;;
;; Remember that every piece of Elisp that I write is for my own
;; educational and recreational purposes.  I am not a programmer and I
;; do not recommend that you copy any of this if you are not certain of
;; what it does.

;;; Code:

(eval-when-compile
  (require 'cl-lib))
(require 'prot-common)

(defgroup prot-simple ()
  "Generic utilities for my dotemacs."
  :group 'editing)

(defcustom prot-simple-date-specifier "%F"
  "Date specifier for `format-time-string'.
Used by `prot-simple-inset-date'."
  :type 'string
  :group 'prot-simple)

(defcustom prot-simple-time-specifier "%R %z"
  "Time specifier for `format-time-string'.
Used by `prot-simple-inset-date'."
  :type 'string
  :group 'prot-simple)

(defvar-keymap prot-simple-global-repeat-map
  :doc "Repeatable prot-simple commands, per `repeat-mode'."
  :repeat t
  "+" #'prot-simple-number-increment
  "-" #'prot-simple-number-decrement)

;;; Commands

;; NOTE 2023-06-21: The code I had for scratch buffers per major mode
;; is now part of prot-scratch.el.

;;;; General commands

;;;###autoload
(defun prot-simple-keyboard-quit-dwim ()
  "Do-What-I-Mean behaviour for a general `keyboard-quit'.

The generic `keyboard-quit' does not do the expected thing when
the minibuffer is open.  Whereas we want it to close the
minibuffer, even without explicitly focusing it.

The DWIM behaviour of this command is as follows:

- When the region is active, disable it.
- When a minibuffer is open, but not focused, close the minibuffer.
- When the Completions buffer is selected, close it.
- In every other case use the regular `keyboard-quit'."
  (interactive)
  (cond
   ((region-active-p)
    (keyboard-quit))
   ((derived-mode-p 'completion-list-mode)
    (delete-completion-window))
   ((> (minibuffer-depth) 0)
    (abort-recursive-edit))
   (t
    (keyboard-quit))))

(autoload 'symbol-at-point "thingatpt")

;;;###autoload
(defun prot-simple-describe-symbol ()
  "Run `describe-symbol' for the `symbol-at-point'."
  (interactive)
  (describe-symbol (symbol-at-point)))

(autoload 'number-at-point "thingatpt")

(defun prot-simple--number-operate (number amount operation)
  "Perform OPERATION on NUMBER given AMOUNT and return the result.
OPERATION is the keyword `:increment' or `:decrement' to perform
`1+' or `1-', respectively."
  (when (and (numberp number) (numberp amount))
    (let ((fn (pcase operation
                (:increment #'+)
                (:decrement #'-)
                (_ (user-error "Unknown operation `%s' for number `%s'" operation number)))))
      (funcall fn number amount))))

(defun prot-simple--number-replace (number amount operation)
  "Perform OPERATION on NUMBER at point by AMOUNT."
  (when-let ((bounds (bounds-of-thing-at-point 'number))
             (replacement (prot-simple--number-operate number amount operation)))
    (delete-region (car bounds) (cdr bounds))
    (save-excursion
      (insert (number-to-string replacement)))))

;;;###autoload
(defun prot-simple-number-increment (number amount)
  "Increment NUMBER by AMOUNT.
When called interactively, NUMBER is the one at point, while
AMOUNT is either 1 or that of a number prefix argument."
  (interactive
   (list
    (number-at-point)
    (prefix-numeric-value current-prefix-arg)))
  (prot-simple--number-replace number amount :increment))

;;;###autoload
(defun prot-simple-number-decrement (number amount)
  "Decrement NUMBER by AMOUNT.
When called interactively, NUMBER is the one at point, while
AMOUNT is either 1 or that of a number prefix argument."
  (interactive
   (list
    (number-at-point)
    (prefix-numeric-value current-prefix-arg)))
  (prot-simple--number-replace number amount :decrement))

;;;; Commands for lines

;;;###autoload
(defun prot-simple-new-line-below (n)
  "Create N empty lines below the current one.
When called interactively without a prefix numeric argument, N is
1."
  (interactive "p")
  (goto-char (line-end-position))
  (dotimes (_ n) (insert "\n")))

;;;###autoload
(defun prot-simple-new-line-above (n)
  "Create N empty lines above the current one.
When called interactively without a prefix numeric argument, N is
1."
  (interactive "p")
  (let ((point-min (point-min)))
    (if (or (bobp)
            (eq (point) point-min)
            (eq (line-number-at-pos point-min) 1))
        (progn
          (goto-char (line-beginning-position))
          (dotimes (_ n) (insert "\n"))
          (forward-line (- n)))
      (forward-line (- n))
      (prot-simple-new-line-below n))))

;;;###autoload
(defun prot-simple-copy-line ()
  "Copy the current line to the `kill-ring'."
  (interactive)
  (copy-region-as-kill (line-beginning-position) (line-end-position)))

(make-obsolete 'prot-simple-copy-line-or-region 'prot-simple-copy-line "2023-09-26")

;;;###autoload
(defun prot-simple-yank-replace-line-or-region ()
  "Replace line or region with latest kill.
This command can then be followed by the standard
`yank-pop' (default is bound to \\[yank-pop])."
  (interactive)
  (if (use-region-p)
      (delete-region (region-beginning) (region-end))
    (delete-region (line-beginning-position) (line-end-position)))
  (yank))

;;;###autoload
(defun prot-simple-multi-line-below ()
  "Move half a screen below."
  (interactive)
  (forward-line (floor (window-height) 2))
  (setq this-command 'scroll-up-command))

;;;###autoload
(defun prot-simple-multi-line-above ()
  "Move half a screen above."
  (interactive)
  (forward-line (- (floor (window-height) 2)))
  (setq this-command 'scroll-down-command))

;;;###autoload
(defun prot-simple-kill-line-backward ()
  "Kill from point to the beginning of the line."
  (interactive)
  (kill-line 0))

;;;; Commands for text insertion or manipulation

(make-obsolete 'prot-simple-insert-pair-alist nil "2023-09-10")

(defcustom prot-simple-insert-pair-pairs
  '((?'  :description "Single quotes"           :pair (?' . ?'))
    (?\" :description "Double quotes"           :pair (?\" . ?\"))
    (?‘  :description "Single curly quotes"     :pair (?‘ . ?’))
    (?“  :description "Double curly quotes"     :pair (?“ . ?”))
    (?\> :description "Natural language quotes" :pair prot-simple-insert-pair-natural-language-quotes)
    (?\( :description "Parentheses"             :pair (?\( . ?\)))
    (?{  :description "Curly brackets"          :pair (?{ . ?}))
    (?\[ :description "Square brackets"         :pair (?\[ . ?\]))
    (?\< :description "Angled brackets"         :pair (?\< . ?\>))
    (?@  :description "At signs"                :pair (?@ . ?@))
    (?=  :description "Equals signs"            :pair (?= . ?=))
    (?+  :description "Plus signs"              :pair (?+ . ?+))
    (?`  :description "Backticks"               :pair prot-simple-insert-pair-backticks)
    (?~  :description "Tildes"                  :pair (?~ . ?~))
    (?*  :description "Asterisks"               :pair (?* . ?*))
    (?/  :description "Forward slashes"         :pair (?/ . ?/))
    (?_  :description "Underscores"             :pair (?_ . ?_)))
  "Alist of pairs for use with `prot-simple-insert-pair'.
Each element in the list is a list whose `car' is a character and
the `cdr' is a plist with a `:description' and `:pair' keys.  The
`:description' is a string used to describe the character/pair in
interactive use, while `:pair' is a cons cell referencing the
opening and closing characters.

The value of `:pair' can also be the unquoted symbol of a
function.  The function is called with no arguments and must
return a cons cell of two characters.  Examples of such functions
are `prot-simple-insert-pair-natural-language-quotes' and
`prot-simple-insert-pair-backticks'"
  :type '(alist
          :key-type character
          :value-type (plist :options (((const :tag "Pair description" :description) string)
                                       ((const :tag "Characters" :pair)
                                        (choice (cons character character) function)))))
  :group 'prot-simple)

(defun prot-simple-insert-pair-backticks ()
  "Return pair of backticks for `prot-simple-insert-pair-pairs'.
When the major mode is derived from `lisp-mode', return a pair of
backtick and single quote, else two backticks."
  (if (derived-mode-p 'lisp-mode 'lisp-data-mode)
      (cons ?` ?')
    (cons ?` ?`)))

(defun prot-simple-insert-pair-natural-language-quotes ()
  "Return pair of quotes for `prot-simple-insert-pair-pairs', per natural language."
  ;; There are more here: <https://en.wikipedia.org/wiki/Quotation_mark>.
  ;; I cover the languages I might type in.
  (cond
   ((and current-input-method
         (string-match-p "\\(greek\\|french\\|spanish\\)" current-input-method))
    (cons ?« ?»))
   (t (cons ?\" ?\"))))

(defun prot-simple--insert-pair-prompt ()
  "Prompt for initial character among `prot-simple-insert-pair-pairs'."
  (car
   (read-multiple-choice
    "Select pair: "
    (mapcar
     (lambda (properties)
       (list
        (car properties)
        (plist-get (cdr properties) :description)))
     prot-simple-insert-pair-pairs))))

(defun prot-simple--insert-pair-bounds ()
  "Return boundaries of symbol at point or active region."
  (if (region-active-p)
      (cons (region-beginning) (region-end))
    (bounds-of-thing-at-point 'symbol)))

(defun prot-simple--insert-pair-characters (character)
  "Return pair corresponding to CHARACTER in `prot-simple-insert-pair-pairs'."
  (plist-get (alist-get character prot-simple-insert-pair-pairs) :pair))

;;;###autoload
(defun prot-simple-insert-pair (pair &optional count)
  "Insert PAIR among `prot-simple-insert-pair-pairs' around object at point.
The object at point is either a symbol or the boundaries of the
active region.

With optional COUNT as a numeric value, do the aforementioned
COUNTth times.  Without a COUNT, the operation is performed once."
  (interactive
   (list
    (prot-simple--insert-pair-characters (prot-simple--insert-pair-prompt))
    (prefix-numeric-value current-prefix-arg)))
  (let* ((bounds (prot-simple--insert-pair-bounds))
         (beg (car bounds))
         (end (1+ (cdr bounds))) ; 1+ because we want the character after it
         (characters (if (functionp pair) (funcall pair) pair)))
    (dotimes (_ (or count 1))
      (save-excursion
        (goto-char beg)
        (insert (car characters))
        (goto-char end)
        (setq end (1+ end))
        (insert (cdr characters))))))

;;;###autoload
(defun prot-simple-delete-pair-dwim ()
  "Delete pair following or preceding point.
For Emacs version 28 or higher, the feedback's delay is
controlled by `delete-pair-blink-delay'."
  (interactive)
  (if (eq (point) (cdr (bounds-of-thing-at-point 'sexp)))
      (delete-pair -1)
    (delete-pair 1)))

;;;###autoload
(defun prot-simple-insert-date (&optional arg)
  "Insert the current date as `prot-simple-date-specifier'.

With optional prefix ARG (\\[universal-argument]) also append the
current time understood as `prot-simple-time-specifier'.

When region is active, delete the highlighted text and replace it
with the specified date."
  (interactive "P")
  (let* ((date prot-simple-date-specifier)
         (time prot-simple-time-specifier)
         (format (if arg (format "%s %s" date time) date)))
    (when (use-region-p)
      (delete-region (region-beginning) (region-end)))
    (insert (format-time-string format))))

(defun prot-simple--pos-url-on-line (&optional char)
  "Return position of `prot-common-url-regexp' on line or at CHAR."
  (save-excursion
    (goto-char (or char (line-beginning-position)))
    (re-search-forward prot-common-url-regexp (line-end-position) :noerror)))

;;;###autoload
(defun prot-simple-escape-url-line (&optional char)
  "Escape all URLs or email addresses on the current line.
By default, start operating from `line-beginning-position' to the
end of the current line.  With optional CHAR as a buffer
position, operate from CHAR to the end of the line."
  (interactive)
  (when-let ((regexp-end (prot-simple--pos-url-on-line char)))
    (save-excursion
      (goto-char regexp-end)
      (unless (looking-at ">")
        (insert ">")
        (search-backward "\s")
        (forward-char 1)
        (insert "<")))
    (prot-simple-escape-url-line (1+ regexp-end))))

;; Thanks to Bruno Boal for `prot-simple-escape-url-region'.  I am
;; just renaming it for consistency with the rest of prot-simple.el.
;; Check Bruno's Emacs config: <https://github.com/BBoal/emacs-config>.

;;;###autoload
(defun prot-simple-escape-url-region (&optional beg end)
  "Apply `prot-simple-escape-url-line' on region lines between BEG and END."
  (interactive
   (if (region-active-p)
       (list (region-beginning) (region-end))
     (error "There is no region!")))
  (unless (> end beg)
    (cl-rotatef end beg))
  (save-excursion
    (goto-char beg)
    (setq beg (line-beginning-position))
    (while (<= beg end)
      (prot-simple-escape-url-line beg)
      (beginning-of-line 2)
      (setq beg (point)))))

;;;###autoload
(defun prot-simple-escape-url-dwim ()
  "Escape URL on the current line or lines implied by the active region.
Call the commands `prot-simple-escape-url-line' and
`prot-simple-escape-url-region' ."
  (interactive)
  (call-interactively
   (if (region-active-p)
       #'prot-simple-escape-url-region
     #'prot-simple-escape-url-line)))

;;;###autoload
(defun prot-simple-zap-to-char-backward (char &optional arg)
  "Backward `zap-to-char' for CHAR.
Optional ARG is a numeric prefix to match ARGth occurance of
CHAR."
  (interactive
   (list
    (read-char-from-minibuffer "Zap to char: " nil 'read-char-history)
    (prefix-numeric-value current-prefix-arg)))
  (zap-to-char (- arg) char t))

(defvar prot-simple-flush-and-diff-history nil
  "Minibuffer history for `prot-simple-flush-and-diff'.")

;;;###autoload
(defun prot-simple-flush-and-diff (regexp beg end)
  "Call `flush-lines' for REGEXP and produce diff if file is modified.
When region is active, operate between the region boundaries
demarcated by BEG and END."
  (interactive
   (let ((regionp (region-active-p)))
     (list
      (read-regexp "Flush lines using REGEXP: " nil 'prot-simple-flush-and-diff-history)
      (and regionp (region-beginning))
      (and regionp (region-end)))))
  (flush-lines regexp (or beg (point-min)) (or end (point-max)) :no-message)
  (when (and (buffer-modified-p) buffer-file-name)
    (diff-buffer-with-file (current-buffer))))

(defcustom prot-simple-line-prefix-strings '(">" "+" "-")
  "List of strings used as line prefixes.
The command which serves as the point of entry is
`prot-simple-insert-line-prefix'."
  :type '(repeat string)
  :group 'prot-simple)

(defun prot-simple--line-prefix-regexp (&optional string)
  "Format regular expression for `prot-simple--line-prefix-p'.
With optional STRING use it directly.  Else format the regexp by
concatenating `prot-simple-line-prefix-strings'."
  (if string
      (format "^%s " string)
    (format "^[%s] " (mapconcat #'identity prot-simple-line-prefix-strings))))

(defun prot-simple--line-prefix-p (&optional string)
  "Return non-nil if line beginning has an appropriate string prefix.
With optional STRING test that it is at the beginning of the line."
  (save-excursion
    (goto-char (line-beginning-position))
    (looking-at (prot-simple--line-prefix-regexp string))))

(defun prot-simple--line-prefix-insert (string)
  "Insert STRING at the beginning of the line, followed by a space."
  (save-excursion
    (goto-char (line-beginning-position))
    (insert string)
    (insert " ")))

(defun prot-simple--line-prefix-infer-string ()
  "Return line prefix string if it matches `prot-simple--line-prefix-p'."
  (when (prot-simple--line-prefix-p)
    (string-trim
     (buffer-substring-no-properties (match-beginning 0) (match-end 0)))))

(defun prot-simple--line-prefix-toggle (string)
  "Insert or remove STRING at the beginning of the line."
  (if (prot-simple--line-prefix-p string)
      (delete-region (match-beginning 0) (match-end 0))
    (prot-simple--line-prefix-insert string)))

(defvar prot-simple--line-prefix-history nil
  "Minibuffer history of `prot-simple--line-prefix-prompt'.")

(defun prot-simple--line-prefix-prompt ()
  "Prompt for string to use as line prefix.
Provide `prot-simple-line-prefix-strings' as completion
candidates, though accept arbitrary input."
  (let ((default (car prot-simple--line-prefix-history)))
    (completing-read
     (format-prompt "Select line prefix" default)
     prot-simple-line-prefix-strings
     nil nil nil
     'prot-simple--line-prefix-history default)))

(defun prot-simple-line-prefix-infer-or-prompt ()
  "Infer string for line prefix or prompt for one."
  (or (prot-simple--line-prefix-infer-string)
      (prot-simple--line-prefix-prompt)))

;;;###autoload
(defun prot-simple-insert-line-prefix-dwim (string)
  "Toggle presence of STRING at the beginning of the line.

When called interactively try to infer STRING based on the line
prefix.  If one is found among `prot-simple-line-prefix-strings',
perform a removal outright.

If no string can be inferred, prompt for STRING among
`prot-simple-line-prefix-strings'.  Accept arbitrary strings at
the prompt.

When the region is active, toggle the presence of STRING for each
line in the region."
  (interactive (list (prot-simple-line-prefix-infer-or-prompt)))
  (if-let ((region-p (region-active-p))
           (beg (region-beginning))
           (end (line-number-at-pos (region-end))))
      (progn
        (goto-char beg)
        (push-mark (point))
        (while (<= (line-number-at-pos (point)) end)
          (prot-simple--line-prefix-toggle string)
          (forward-line 1)))
    (prot-simple--line-prefix-toggle string)))

;;;; Commands for object transposition

;; The "move" functions all the way to `prot-simple-move-below-dwim'
;; are courtesy of Bruno Boal: <https://git.sr.ht/~bboal>.  With minor
;; tweaks by me.
(defun prot-simple--move-line (count dir)
  "Move line or region COUNTth times in DIR direction."
  (let* ((start (pos-bol))
         (end (pos-eol))
         diff-eol-point
         diff-eol-mark)
    (when-let (((use-region-p))
               (pos (point))
               (mrk (mark))
               (line-diff-mark-point (1+ (- (line-number-at-pos mrk)
                                            (line-number-at-pos pos)))))
      (if (> pos mrk)
          (setq start (pos-bol line-diff-mark-point)) ; pos-bol of where the mark is
        (setq end (pos-eol line-diff-mark-point)))    ; pos-eol of the line where the mark is
      (setq diff-eol-mark (1+ (- end mrk))))          ; 1+ to get the \n
    ;; this is valid for region or a single line
    (setq diff-eol-point (1+ (- end (point))))
    (let* ((max (point-max))
           (end (1+ end))
           (end (if (> end max) max end))
           (deactivate-mark)
           (lines (delete-and-extract-region start end)))
      (forward-line (* count dir))
      ;; Handle the special case when there isn't a newline as the eob.
      (when (and (eq (point) max)
                 (/= (current-column) 0))
        (insert "\n"))
      (insert lines)
      ;; if user provided a region
      (when diff-eol-mark
        (set-mark (- (point) diff-eol-mark)))
      ;; either way go to same point location reference initial motion
      (goto-char (- (point) diff-eol-point)))))

(defun prot-simple--move-line-user-error (boundary)
  "Return `user-error' with message accounting for BOUNDARY.
BOUNDARY is a buffer position, expected to be `point-min' or `point-max'."
  (when-let ((bound (line-number-at-pos boundary))
             (scope (cond
                     ((and (use-region-p)
                           (or (= (line-number-at-pos (point)) bound)
                               (= (line-number-at-pos (mark)) bound)))
                      "region is ")
                     ((= (line-number-at-pos (point)) bound)
                      "")
                     (t nil))))
    (user-error (format "Warning: %salready in the last line!" scope))))

(defun prot-simple-move-above-dwim (arg)
  "Move line or region ARGth times up.
If ARG is nil, do it one time."
  (interactive "p")
  (unless (prot-simple--move-line-user-error (point-min))
    (prot-simple--move-line arg -1)))

(defun prot-simple-move-below-dwim (arg)
  "Move line or region ARGth times down.
If ARG is nil, do it one time."
  (interactive "p")
  (unless (prot-simple--move-line-user-error (point-max))
    (prot-simple--move-line arg 1)))

(defmacro prot-simple-transpose (name scope &optional doc)
  "Macro to produce transposition functions.
NAME is the function's symbol.  SCOPE is the text object to
operate on.  Optional DOC is the function's docstring.

Transposition over an active region will swap the object at
mark (region beginning) with the one at point (region end)"
  `(defun ,name (arg)
     ,doc
     (interactive "p")
     (let ((x (format "%s-%s" "transpose" ,scope)))
       (if (use-region-p)
           (funcall (intern x) 0)
         (funcall (intern x) arg)))))

(prot-simple-transpose
 prot-simple-transpose-lines
 "lines"
 "Transpose lines or swap over active region.")

(prot-simple-transpose
 prot-simple-transpose-paragraphs
 "paragraphs"
 "Transpose paragraphs or swap over active region.")

(prot-simple-transpose
 prot-simple-transpose-sentences
 "sentences"
 "Transpose sentences or swap over active region.")

(prot-simple-transpose
 prot-simple-transpose-sexps
 "sexps"
 "Transpose balanced expressions or swap over active region.")

;;;###autoload
(defun prot-simple-transpose-chars ()
  "Always transposes the two characters before point.
There is no dragging the character forward.  This is the
behaviour of `transpose-chars' when point is at the end of the
line."
  (interactive)
  (transpose-chars -1)
  (forward-char))

;;;###autoload
(defun prot-simple-transpose-words (arg)
  "Like `transpose-words' but treat ARG as 0 when the region is active."
  (interactive "*p")
  (transpose-words (if (region-active-p) 0 arg)))

;;;; Commands for marking syntactic constructs

(defmacro prot-simple-mark (name object &optional docstring)
  "Produce function for marking small syntactic constructs.
NAME is how the function should be called.  OBJECT is its scope.
Optional DOCSTRING describes the resulting function.

This is a slightly modified version of the built-in `mark-word'."
  `(defun ,name (&optional arg allow-extend)
     ,docstring
     (interactive "P\np")
     (let ((x (format "%s-%s" "forward" ,object)))
       (cond ((and allow-extend
                   (or (and (eq last-command this-command) (mark t))
                       (region-active-p)))
              (setq arg (if arg (prefix-numeric-value arg)
                          (if (< (mark) (point)) -1 1)))
              (set-mark
               (save-excursion
                 (goto-char (mark))
                 (funcall (intern x) arg)
                 (point))))
             (t
              (let ((bounds (bounds-of-thing-at-point (intern ,object))))
                (unless (consp bounds)
                  (user-error "No %s at point" ,object))
                (if (>= (prefix-numeric-value arg) 0)
                    (goto-char (car bounds))
                  (goto-char (cdr bounds)))
                (push-mark
                 (save-excursion
                   (funcall (intern x) (prefix-numeric-value arg))
                   (point)))
                (activate-mark)))))))

(prot-simple-mark
 prot-simple-mark-word
 "word"
 "Mark the whole word at point.
This function is a slightly modified version of the built-in
`mark-word', that I intend to use only in special circumstances,
such as when recording a keyboard macro where precision is
required.  For a general purpose utility, use `prot-simple-mark-symbol'
instead.")

(prot-simple-mark
 prot-simple-mark-symbol
 "symbol"
 "Mark the whole symbol at point.
With optional ARG, mark the current symbol and any remaining
ARGth symbols away from point.  A negative argument moves
backward. Repeated invocations of this command mark the next
symbol in the direction originally specified.

In the absence of a symbol and if a word is present at point,
this command will operate on it as described above.")

;;;###autoload
(defun prot-simple-mark-sexp-backward (&optional arg)
  "Mark previous or ARGth balanced expression[s].
Just a convenient backward-looking `mark-sexp'."
  (interactive "P")
  (if arg
      (mark-sexp (- arg) t)
    (mark-sexp (- 1) t)))

;;;###autoload
(defun prot-simple-mark-construct-dwim (&optional arg)
  "Mark symbol or balanced expression at point.
A do-what-I-mean wrapper for `prot-simple-mark-sexp-backward',
`mark-sexp', and `prot-simple-mark-symbol'.

When point is over a symbol, mark the entirety of it.  Regular
words are interpreted as symbols when an actual symbol is not
present.

For balanced expressions, a backward match will happen when point
is to the right of the closing delimiter.  A forward match is the
fallback condition and should work when point is before a
balanced expression, with or without whitespace in between it an
the opening delimiter.

Optional ARG will mark a total of ARGth objects while counting
the current one (so 3 would be 1+2 more).  A negative count moves
the mark backward (though that would invert the backward-moving
sexp matching of `prot-simple-mark-sexp-backward', so be mindful of
where the point is).  Repeated invocations of this command
incrementally mark objects in the direction originally
specified."
  (interactive "P")
  (cond
   ((symbol-at-point)
    (prot-simple-mark-symbol arg t))
   ((eq (point) (cdr (bounds-of-thing-at-point 'sexp)))
    (prot-simple-mark-sexp-backward arg))
   (t
    (mark-sexp arg t))))

;;;; Commands for code navigation (work in progress)

;;;###autoload
(defun prot-simple-downward-list (arg)
  "Like `backward-up-list' but defaults to a forward motion.
With numeric prefix ARG, move that many times in the given
direction (negative is forward due to this being a
backward-facing command)."
  (interactive "p")
  (backward-up-list (or (- arg) -1)))

(make-obsolete 'prot-simple-downward-list 'up-list "2023-09-20")

;;;; Commands for paragraphs

;;;###autoload
(defun prot-simple-unfill-region-or-paragraph (&optional beg end)
  "Unfill paragraph or, when active, the region.
Join all lines in region delimited by BEG and END, if active,
while respecting any empty lines (so multiple paragraphs are not
joined, just unfilled).  If no region is active, operate on the
paragraph.  The idea is to produce the opposite effect of both
`fill-paragraph' and `fill-region'."
  (interactive "r")
  (let ((fill-column most-positive-fixnum))
    (if (use-region-p)
        (fill-region beg end)
      (fill-paragraph))))

;;;; Commands for windows and pages

;;;###autoload
(defun prot-simple-narrow-visible-window ()
  "Narrow buffer to wisible window area.
Also check `prot-simple-narrow-dwim'."
  (interactive)
  (let* ((bounds (prot-common-window-bounds))
         (window-area (- (cdr bounds) (car bounds)))
         (buffer-area (- (point-max) (point-min))))
    (if (/= buffer-area window-area)
        (narrow-to-region (car bounds) (cdr bounds))
      (user-error "Buffer fits in the window; won't narrow"))))

;;;###autoload
(defun prot-simple-narrow-dwim ()
  "Do-what-I-mean narrowing.
If region is active, narrow the buffer to the region's
boundaries.

If pages are defined by virtue of `prot-common-page-p', narrow to
the current page boundaries.

If no region is active and no pages exist, narrow to the visible
portion of the window.

If narrowing is in effect, widen the view."
  (interactive)
  (unless mark-ring                  ; needed when entering a new buffer
    (push-mark (point) t nil))
  (cond
   ((and (use-region-p)
         (null (buffer-narrowed-p)))
    (narrow-to-region (region-beginning) (region-end)))
   ((prot-common-page-p)
    (narrow-to-page))
   ((null (buffer-narrowed-p))
    (prot-simple-narrow-visible-window))
   ((widen))))

(defun prot-simple--narrow-to-page (count &optional back)
  "Narrow to COUNTth page with optional BACK motion."
  (if back
      (narrow-to-page (or (- count) -1))
    (narrow-to-page (or (abs count) 1)))
  ;; Avoids the problem of skipping pages while cycling back and forth.
  (goto-char (point-min)))

;;;###autoload
(defun prot-simple-forward-page-dwim (&optional count)
  "Move to next or COUNTth page forward.
If buffer is narrowed to the page, keep the effect while
performing the motion.  Always move point to the beginning of the
narrowed page."
  (interactive "p")
  (if (buffer-narrowed-p)
      (prot-simple--narrow-to-page count)
    (forward-page count)
    (setq this-command 'forward-page)))

;;;###autoload
(defun prot-simple-backward-page-dwim (&optional count)
  "Move to previous or COUNTth page backward.
If buffer is narrowed to the page, keep the effect while
performing the motion.  Always move point to the beginning of the
narrowed page."
  (interactive "p")
  (if (buffer-narrowed-p)
      (prot-simple--narrow-to-page count t)
    (backward-page count)
    (setq this-command 'backward-page)))

;;;###autoload
(defun prot-simple-delete-page-delimiters (&optional beg end)
  "Delete lines with just page delimiters in the current buffer.
When region is active, only operate on the region between BEG and
END, representing the point and mark."
  (interactive "r")
  (let (b e)
    (if (use-region-p)
        (setq b beg
              e end)
      (setq b (point-min)
            e (point-max)))
    (widen)
    (flush-lines (format "%s$" page-delimiter) b e)
    (setq this-command 'flush-lines)))

;; NOTE 2023-06-18: The idea of narrowing to a defun in an indirect
;; buffer is still experimental.
(defun prot-simple-narrow--guess-defun-symbol ()
  "Try to return symbol of current defun as a string."
  (save-excursion
    (beginning-of-defun)
    (search-forward " ")
    (thing-at-point 'symbol :no-properties)))

;;;###autoload
(defun prot-simple-narrow-to-cloned-buffer ()
  "Narrow to defun in cloned buffer.
Name the buffer after the defun's symbol."
  (interactive)
  (clone-indirect-buffer-other-window
   (format "%s -- %s"
           (buffer-name)
           (prot-simple-narrow--guess-defun-symbol))
   :display)
  (narrow-to-defun))

;;;; Commands for buffers

;;;###autoload
(defun prot-simple-kill-buffer (buffer)
  "Kill current BUFFER without confirmation.
When called interactively, prompt for BUFFER."
  (interactive (list (read-buffer "Select buffer: ")))
  (let ((kill-buffer-query-functions nil))
    (kill-buffer (or buffer (current-buffer)))))

;;;###autoload
(defun prot-simple-kill-buffer-current (&optional arg)
  "Kill current buffer.
With optional prefix ARG (\\[universal-argument]) delete the
buffer's window as well.  Kill the window regardless of ARG if it
satisfies `prot-common-window-small-p' and it has no previous
buffers in its history."
  (interactive "P")
  (let ((kill-buffer-query-functions nil))
    (if (or (and (prot-common-window-small-p)
                 (null (window-prev-buffers)))
            (and arg (not (one-window-p))))
        (kill-buffer-and-window)
      (kill-buffer))))

;;;###autoload
(defun prot-simple-rename-file-and-buffer (name)
  "Apply NAME to current file and rename its buffer.
Do not try to make a new directory or anything fancy."
  (interactive
   (list (read-string "Rename current file: " (buffer-file-name))))
  (let ((file (buffer-file-name)))
    (if (vc-registered file)
        (vc-rename-file file name)
      (rename-file file name))
    (set-visited-file-name name t t)))

(defun prot-simple--buffer-major-mode-prompt ()
  "Prompt of `prot-simple-buffers-major-mode'.
Limit list of buffers to those matching the current
`major-mode' or its derivatives."
  (let ((read-buffer-function nil)
        (current-major-mode major-mode))
    (read-buffer
     (format "Buffer for %s: " major-mode)
     nil
     :require-match
     (lambda (pair) ; pair is (name-string . buffer-object)
       (with-current-buffer (cdr pair)
         (derived-mode-p current-major-mode))))))

;;;###autoload
(defun prot-simple-buffers-major-mode ()
  "Select BUFFER matching the current one's major mode."
  (interactive)
  (switch-to-buffer (prot-simple--buffer-major-mode-prompt)))

(defun prot-simple--buffer-vc-root-prompt ()
  "Prompt of `prot-simple-buffers-vc-root'."
  (let ((root (or (vc-root-dir)
                  (locate-dominating-file "." ".git")))
        (read-buffer-function nil))
    (read-buffer
     (format "Buffers in %s: " root)
     nil t
     (lambda (pair) ; pair is (name-string . buffer-object)
       (with-current-buffer (cdr pair) (string-match-p root default-directory))))))

;;;###autoload
(defun prot-simple-buffers-vc-root ()
  "Select buffer matching the current one's VC root."
  (interactive)
  (switch-to-buffer (prot-simple--buffer-vc-root-prompt)))

;;;###autoload
(defun prot-simple-swap-window-buffers (counter)
  "Swap states of live buffers.
With two windows, transpose their buffers.  With more windows,
perform a clockwise rotation.  Do not alter the window layout.
Just move the buffers around.

With COUNTER as a prefix argument, do the rotation
counter-clockwise."
  (interactive "P")
  (when-let* ((winlist (if counter (reverse (window-list)) (window-list)))
              (wincount (count-windows))
              ((> wincount 1)))
    (dotimes (i (- wincount 1))
      (window-swap-states (elt winlist i) (elt winlist (+ i 1))))))

;;;; Commands of a general nature

(autoload 'color-rgb-to-hex "color")
(autoload 'color-name-to-rgb "color")

(defun prot-simple-accessible-colors (variant)
  "Return list of accessible `defined-colors'.
VARIANT is either `dark' or `light'."
  (let ((variant-color (if (eq variant 'dark) "#000000" "#ffffff")))
    (seq-filter
     (lambda (c)
       (let* ((rgb (color-name-to-rgb c))
              (r (nth 0 rgb))
              (g (nth 1 rgb))
              (b (nth 2 rgb))
              (hex (color-rgb-to-hex r g b 2)))
         (when (>= (prot-common-contrast variant-color hex) 4.5)
           c)))
     (defined-colors))))

(defun prot-simple--list-accessible-colors-prompt ()
  "Use `read-multiple-choice' to return `dark' or `light' variant."
  (intern
   (cadr
    (read-multiple-choice
     "Variant"
     '((?d "dark" "Load a random dark theme")
       (?l "light" "Load a random light theme"))
     "Limit to the dark or light subset of the Ef themes collection."))))

(defun prot-simple-list-accessible-colors (variant)
  "Return buffer with list of accessible `defined-colors'.
VARIANT is either `dark' or `light'."
  (interactive (list (prot-simple--list-accessible-colors-prompt)))
  (list-colors-display (prot-simple-accessible-colors variant)))

(provide 'prot-simple)
;;; prot-simple.el ends here
