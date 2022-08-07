;;;;; init-keychord.el --- Key chord -*- lexical-binding: t -*-

;;; Install packages
(straight-use-package 'key-chord)

;;; Keychord
(require 'key-chord)
(key-chord-mode 1)
(setq key-chord-two-keys-delay 0.3)

(with-eval-after-load 'evil
  (dolist (mode (list evil-insert-state-map evil-ex-completion-map minibuffer-local-map))
    (key-chord-define mode "kk" "()\C-b")
    (key-chord-define mode ",," "[]\C-b")
    (key-chord-define mode "hh" "{}\C-b")
    (key-chord-define mode "aa" "&")
    (key-chord-define mode "qq" "@")
    (key-chord-define mode "vv" "#")
    (key-chord-define mode "uu" "_")
    (key-chord-define mode "jj" "+")
    (key-chord-define mode "ji" "-")
    (key-chord-define mode "cj" "*")
    (key-chord-define mode "dh" "=")
    (key-chord-define mode "ww" "?")
    (key-chord-define mode "bb" "~")
    (key-chord-define mode ",r" "<-")
    (key-chord-define mode ",j" "%>%")
    (key-chord-define mode ";f" "5")
    (key-chord-define mode ";i" "6")
    (key-chord-define mode ";s" "7")
    (key-chord-define mode ";e" "8")
    (key-chord-define mode ";n" "9")
    (key-chord-define mode ";t" "0")))

(with-eval-after-load 'general
  (general-evil-setup t)

  ;; exclamation
  (defun p-insert-exc ()
    (interactive)
    (insert "!"))
  (general-imap "g"
                (general-key-dispatch 'self-insert-command
                  :timeout 0.25
                  "t" 'p-insert-exc))
  ;; british pound
  (defun p-insert-pound ()
    (interactive)
    (insert "£"))
  (general-imap "y"
                (general-key-dispatch 'self-insert-command
                  :timeout 0.25
                  "b" 'p-insert-pound))
  ;; dollar
  (defun p-insert-dollar ()
    (interactive)
    (insert "$"))
  (general-imap "m"
                (general-key-dispatch 'self-insert-command
                  :timeout 0.25
                  "j" 'p-insert-dollar))
  ;; percentage
  (defun p-insert-percent ()
    (interactive)
    (insert "%"))
  (general-imap "f"
                (general-key-dispatch 'self-insert-command
                  :timeout 0.25
                  "h" 'p-insert-percent))
  ;; carat
  (defun p-insert-carat ()
    (interactive)
    (insert "^"))
  (general-imap "s"
                (general-key-dispatch 'self-insert-command
                  :timeout 0.25
                  "j" 'p-insert-carat))
  ;; pipe
  (defun p-insert-pipe ()
    (interactive)
    (insert "|"))
  (general-imap "z"
                (general-key-dispatch 'self-insert-command
                  :timeout 0.25
                  "l" 'p-insert-pipe))
  ;; less than
  (defun p-insert-less ()
    (interactive)
    (insert "<"))
  (general-imap "x"
                (general-key-dispatch 'self-insert-command
                  :timeout 0.25
                  "y" 'p-insert-less))
  ;; greater than
  (defun p-insert-greater ()
    (interactive)
    (insert ">"))
  (general-imap "d"
                (general-key-dispatch 'self-insert-command
                  :timeout 0.25
                  "y" 'p-insert-greater))
  ;; R assign
  (defun p-insert-r-assign ()
    (interactive)
    (insert "<-"))
  (general-imap "e"
                (general-key-dispatch 'self-insert-command
                  :timeout 0.25
                  "j" 'p-insert-r-assign))
  ;; R connect
  (defun p-insert-r-connect ()
    (interactive)
    (insert "%>%"))
  (general-imap "r"
                (general-key-dispatch 'self-insert-command
                  :timeout 0.25
                  "j" 'p-insert-r-connect))
  ;; double equal
  (defun p-insert-double-equal ()
    (interactive)
    (insert "=="))
  (general-imap ","
                (general-key-dispatch 'self-insert-command
                  :timeout 0.25
                  "d" 'p-insert-double-equal)))

(provide 'init-keychord)
;;;;; init-keychord.el ends here
