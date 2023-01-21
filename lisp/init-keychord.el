;;;;; init-keychord.el --- Key chord -*- lexical-binding: t -*-

;;; Install packages
(straight-use-package 'key-chord)

;;; Keychord
(require 'key-chord)
(key-chord-mode 1)
(setq key-chord-two-keys-delay 0.2)

(with-eval-after-load 'evil
  (dolist (mode (list evil-insert-state-map evil-ex-completion-map minibuffer-local-map))
    (key-chord-define mode "kk" "()\C-b")
    (key-chord-define mode ",," "[]\C-b")
    (key-chord-define mode "hh" "{}\C-b")
    (key-chord-define mode "aa" "&")
    (key-chord-define mode "qq" "@")
    (key-chord-define mode "vv" "#")
    (key-chord-define mode "ii" "!")
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
    (key-chord-define mode ";c" "^")
    (key-chord-define mode ";x" "<")
    (key-chord-define mode ";d" ">")
    (key-chord-define mode ";a" "<-")
    (key-chord-define mode ";j" "%>%")
    (key-chord-define mode ";m" "%*%")
    (key-chord-define mode ";p" "%%")
    (key-chord-define mode ";f" "5")
    (key-chord-define mode ";i" "6")
    (key-chord-define mode ";s" "7")
    (key-chord-define mode ";e" "8")
    (key-chord-define mode ";n" "9")
    (key-chord-define mode ";t" "0")))

(with-eval-after-load 'general
  (general-evil-setup t)

  ;; british pound
  (defun p-insert-pound ()
    (interactive)
    (insert "£"))
  (general-imap "y"
                (general-key-dispatch 'self-insert-command
                  :timeout 0.2
                  "b" 'p-insert-pound))
  ;; double equal
  (defun p-insert-double-equal ()
    (interactive)
    (insert "=="))
  (general-imap ","
                (general-key-dispatch 'self-insert-command
                  :timeout 0.2
                  "d" 'p-insert-double-equal)))

(provide 'init-keychord)
;;;;; init-keychord.el ends here
