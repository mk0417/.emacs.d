;;;;; init-keychord.el --- Key chord -*- lexical-binding: t -*-

;; My modified simultaneous keybindings based on Jonas Bernoulli's key-chord
;; https://github.com/emacsorphanage/key-chord/blob/master/key-chord.el

(defgroup key-chord nil
  "Map pairs of simultaneously pressed keys to commands."
  :group 'bindings)

(defcustom key-chord-two-keys-delay 0.2
  "Max time delay between two key press to be considered a key chord."
  :type 'float)

(defcustom key-chord-one-key-delay 0.2
  "Max time delay between two press of the same key to be considered a key chord.
This should normally be a little longer than `key-chord-two-keys-delay'."
  :type 'float)

(defcustom key-chord-safety-interval-backward 0
  "Min time to distinguish a key chord and preceding inputs."
  :type 'float)

(defcustom key-chord-safety-interval-forward 0
  "Min time delay to distinguish a key chord and following inputs."
  :type 'float)

(defvar key-chord-mode nil)

;; Shortcut for key-chord-input-method: no need to test a key again if it
;; didn't matched a chord the last time. Improves feedback during autorepeat.
(defvar key-chord-last-unmatched nil)

;; Macro heuristics: Keep track of which chords was used when the last macro
;; was defined. Or rather, only the first-char of the chords. Only expand
;; matching chords during macro execution.
(defvar key-chord-in-last-kbd-macro nil)
(defvar key-chord-defining-kbd-macro nil)

;; Internal vars for
;; `key-chord-safety-interval-backward'. `key-chord-idle-state' is
;; non-nil iff at least `key-chord-safety-interval-backward' past
;; after the last (non-keychord) input.
(defvar key-chord-idle-state t)
(defvar key-chord-timer-object nil)

;;;###autoload
(define-minor-mode key-chord-mode
  "Map pairs of simultaneously pressed keys to commands.

See `key-chord-define' and variables `key-chord-two-keys-delay'
and `key-chord-one-key-delay'."
  :global t
  (if key-chord-mode
      (progn
        (setq input-method-function 'key-chord-input-method)
        (setq key-chord-timer-object
              (run-with-idle-timer
               key-chord-safety-interval-backward 'repeat
               (lambda () (setq key-chord-idle-state t)))))
    (cancel-timer key-chord-timer-object)
    (setq input-method-function nil)
    (setq key-chord-timer-object nil)))

;;;###autoload
(defun key-chord-define (keymap keys command)
  (if (/= 2 (length keys))
      (error "Key-chord keys must have two elements"))
  (let ((key1 (logand 255 (aref keys 0)))
        (key2 (logand 255 (aref keys 1))))
    (if (eq key1 key2)
        (define-key keymap (vector 'key-chord key1 key2) command)
      (define-key keymap (vector 'key-chord key1 key2) command))))

(defun key-chord-lookup-key1 (keymap key)
  (let ((res (lookup-key keymap key)))
    (and (not (numberp res))
         res)))

(defun key-chord-lookup-key (key)
  (let ((maps (current-minor-mode-maps))
        res)
    (while (and maps (not res))
      (setq res (key-chord-lookup-key1 (car maps) key))
      (setq maps (cdr maps)))
    (or res
        (and (current-local-map)
             (key-chord-lookup-key1 (current-local-map) key))
        (key-chord-lookup-key1 (current-global-map) key))))

(defun key-chord-input-method (first-char)
  (cond
   ((and key-chord-idle-state
         (not (eq first-char key-chord-last-unmatched))
         (key-chord-lookup-key (vector 'key-chord first-char)))
    (let ((delay (if (key-chord-lookup-key
                      (vector 'key-chord first-char first-char))
                     key-chord-one-key-delay
                   key-chord-two-keys-delay)))
      (cond ((if executing-kbd-macro
                 (not (memq first-char key-chord-in-last-kbd-macro))
               (when (bound-and-true-p eldoc-mode)
                 (eldoc-pre-command-refresh-echo-area))
               (sit-for delay 'no-redisplay))
             (setq key-chord-last-unmatched nil)
             (setq key-chord-idle-state nil)
             (list first-char))
            (t ; input-pending-p
             (let* ((input-method-function nil)
                    (next-char (read-event))
                    (res (vector 'key-chord first-char next-char)))
               (cond ((and (key-chord-lookup-key res)
                           (sit-for key-chord-safety-interval-forward 'no-redisplay))
                      (setq key-chord-defining-kbd-macro
                            (cons first-char key-chord-defining-kbd-macro))
                      (list 'key-chord first-char next-char))
                     (t ;put back next-char and return first-char
                      (setq unread-command-events
                            (cons next-char unread-command-events))
                      (when (eq first-char next-char)
                        (setq key-chord-last-unmatched first-char))
                      (setq key-chord-idle-state nil)
                      (list first-char))))))))
   (t ; no key-chord keymap
    (setq key-chord-last-unmatched first-char)
    (setq key-chord-idle-state nil)
    (list first-char))))

(key-chord-mode 1)

(with-eval-after-load 'evil
  (dolist (mode (list evil-insert-state-map evil-ex-completion-map minibuffer-local-map))
    (key-chord-define mode "kk" "()\C-b")
    (key-chord-define mode ",," "[]\C-b")
    (key-chord-define mode "hh" "{}\C-b")
    (key-chord-define mode "aa" "&")
    (key-chord-define mode "qq" "@")
    (key-chord-define mode "vv" "#")
    (key-chord-define mode "gt" "!")
    (key-chord-define mode "zz" "$")
    (key-chord-define mode "zl" "|")
    (key-chord-define mode "fh" "%")
    (key-chord-define mode "uu" "_")
    (key-chord-define mode "jj" "+")
    (key-chord-define mode "ji" "-")
    (key-chord-define mode "cj" "*")
    (key-chord-define mode "dh" "=")
    (key-chord-define mode "ww" "?")
    (key-chord-define mode "bb" "~")
    (key-chord-define mode ",d" "==")
    (key-chord-define mode ";c" "^")
    (key-chord-define mode ";x" "<")
    (key-chord-define mode ";d" ">")
    (key-chord-define mode ";h" "<>\C-b")
    (key-chord-define mode ";a" "<-")
    (key-chord-define mode ";j" "%>%")
    (key-chord-define mode ";m" "%*%")
    (key-chord-define mode ";p" "%%\C-b")
    (key-chord-define mode ";f" "5")
    (key-chord-define mode ";i" "6")
    (key-chord-define mode ";s" "7")
    (key-chord-define mode ";e" "8")
    (key-chord-define mode ";n" "9")
    (key-chord-define mode ";t" "0")))

(provide 'init-keychord)
