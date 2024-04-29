;;; init-keybinding.el --- Keybinding -*- lexical-binding: t -*-

(use-package meow
  :ensure t
  :config
  (meow-global-mode 1)
  (meow-thing-register 'angle
                       '(pair ("<") (">"))
                       '(pair ("<") (">")))

  (setq meow-char-thing-table
        '((?b . round)
          (?f . square)
          (?h . curly)
          (?a . angle)
          (?d . defun)
          (?s . string)
          (?p . paragraph)
          (?l . line)
          (?x . buffer)))

  (meow-leader-define-key
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument)
   '("/" . meow-keypad-describe-key)
   '("?" . meow-cheatsheet))

  (meow-normal-define-key
   '("a" . meow-append)
   '("b" . meow-back-word)
   '("c" . meow-search)
   '("d" . meow-kill)
   '("e" . meow-block)
   '("h" . meow-left)
   '("i" . meow-insert)
   '("j" . meow-next)
   '("k" . meow-prev)
   '("l" . meow-right)
   '("m" . meow-line)
   '("o" . meow-open-below)
   '("p" . meow-yank)
   '("q" . meow-join)
   '("r" . meow-replace)
   '("s" . meow-change)
   '("u" . undo-only)
   '("w" . meow-next-word)
   '("x" . meow-delete)
   '("y" . meow-clipboard-save)
   '("B" . meow-back-symbol)
   '("E" . meow-to-block)
   '("H" . meow-left-expand)
   '("J" . meow-next-expand)
   '("K" . meow-prev-expand)
   '("L" . meow-right-expand)
   '("O" . meow-open-above)
   '("P" . meow-yank-pop)
   '("R" . undo-redo)
   '("S" . meow-goto-line)
   '("W" . meow-next-symbol)
   '("0" . meow-expand-0)
   '("1" . meow-expand-1)
   '("2" . meow-expand-2)
   '("3" . meow-expand-3)
   '("4" . meow-expand-4)
   '("5" . meow-expand-5)
   '("6" . meow-expand-6)
   '("7" . meow-expand-7)
   '("8" . meow-expand-8)
   '("9" . meow-expand-9)
   '("," . meow-beginning-of-thing)
   '("." . meow-end-of-thing)
   '("'" . meow-reverse)
   '("[" . indent-rigidly-left-to-tab-stop)
   '("]" . indent-rigidly-right-to-tab-stop)
   
   ;; prefix v
   '("va" . meow-bounds-of-thing)
   '("vc" . meow-save-char)
   '("ve" . meow-swap-grab)
   '("vf" . meow-visit)
   '("vg" . meow-grab)
   '("vh" . "H")
   '("vi" . meow-inner-of-thing)
   '("vj" . "J")
   '("vk" . "K")
   '("vl" . "L")
   '("vr" . meow-query-replace-regexp)
   '("vs" . meow-mark-symbol)
   '("vw" . meow-mark-word)

   ;; prefix n
   '("nd" . (lambda () (interactive) (prot-pair-insert '(?\" . ?\") 1)))
   '("nf" . (lambda () (interactive) (prot-pair-insert '(?\[ . ?\]) 1)))
   '("nh" . (lambda () (interactive) (prot-pair-insert '(?\{ . ?\}) 1)))
   '("nk" . (lambda () (interactive) (prot-pair-insert '(?\( . ?\)) 1)))
   '("nn" . prot-pair-delete)
   '("ns" . (lambda () (interactive) (prot-pair-insert '(?\' . ?\') 1)))
   
   ;; prefix f
   '("fi" . indent-region)
   '("fx" . execute-extended-command)
   '("f'" . meow-last-buffer)
   
   '("<escape>" . ignore))

  (define-key meow-insert-state-keymap (kbd "C-g") 'meow-insert-exit)
  (define-key meow-keypad-state-keymap (kbd "C-g") 'meow-keypad-quit))

(use-package prot-prefix
  :ensure nil
  :bind-keymap
  ((";" . prot-prefix)))

(provide 'init-keybinding)
