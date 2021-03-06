;;;;; init-windows.el --- Windows config -*- lexical-binding: t -*-

;;; Install packages
(straight-use-package 'winum)

;;; winum
(add-hook 'after-init-hook 'winum-mode)

;;; Split new buffer on the right by default
(setq split-height-threshold nil)

;;; Keybindings
(with-eval-after-load 'evil
  (general-create-definer p-space-leader-def
    :prefix "SPC"
    :states '(normal visual))
  (p-space-leader-def
    "SPC" 'execute-extended-command
    "1" '(winum-select-window-1 :which-key "select window 1")
    "2" '(winum-select-window-2 :which-key "select window 2")
    "3" '(winum-select-window-3 :which-key "select window 3")
    "w" '(:ignore t :which-key "window")
    "wl" '(evil-window-right :which-key "move to right window")
    "wh" '(evil-window-left :which-key "move to left window")
    "wj" '(evil-window-down :which-key "move to down window")
    "wk" '(evil-window-up :which-key "move to up window")
    "wd" '(delete-window :which-key "delete window")
    "wv" '(evil-window-vsplit :which-key "split window right")
    "ws" '(evil-window-split :which-key "split window below")
    "wo" '(delete-other-windows :which-key "delete other windows")))

(provide 'init-windows)
;;;;; init-windows.el ends here
