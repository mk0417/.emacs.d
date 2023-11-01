;;; prot-prefix.el --- Prefix keymap for my dotemacs -*- lexical-binding: t -*-

;; Copyright (C) 2023  Protesilaos Stavrou

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
;; Prefix keymap for my custom keymaps.
;;
;; Remember that every piece of Elisp that I write is for my own
;; educational and recreational purposes.  I am not a programmer and I
;; do not recommend that you copy any of this if you are not certain of
;; what it does.

;;; Code:

(declare-function prot-simple-kill-buffer-current "prot-simple" (&optional arg))
(declare-function prot-simple-rename-file-and-buffer "prot-simple" (name))

(defvar-keymap prot-prefix-buffer-map
  :doc "Prefix keymap for buffers."
  :name "Buffer"
  "i" 'ibuffer
  "d" 'prot-simple-kill-buffer-current
  "D" 'kill-buffer-and-window
  "r" 'revert-buffer
  "h" 'mark-whole-buffer
  "s" 'prot-scratch-buffer
  "m" 'p-switch-to-messages
  "b" #'consult-buffer
  "c" #'clone-indirect-buffer-other-window
  "f" #'fit-window-to-buffer
  "k" #'prot-simple-kill-buffer-current
  "g" #'revert-buffer-quick
  "n" #'next-buffer
  "p" #'previous-buffer)

(defvar-keymap prot-prefix-file-map
  :doc "Prefix keymaps for files."
  :name "File"
  "p" 'p-find-file-in-config
  "n" 'p-find-file-in-notes
  "j" 'p-create-scratch-file
  "r" #'prot-simple-rename-file-and-buffer
  "s" #'save-buffer
  "f" #'find-file
  "F" #'find-file-other-window
  "b" #'bookmark-jump
  "d" #'dired
  "l" #'find-library
  "m" #'man)

(defvar-keymap prot-prefix-insert-map
  :doc "Prefix keymap for character insertion."
  :name "Insert"
  "i" #'insert-char
  "e" #'emoji-search
  "q" #'quoted-insert
  "s" #'emoji-search
  "l" #'emoji-list)

(declare-function keycast-mode "keycast")
(declare-function prot-modeline-subtle-mode "prot-modeline")
(declare-function rainbow-mode "rainbow")
(declare-function spacious-padding-mode "spacious-padding")

(defvar-keymap prot-prefix-toggle-map
  :doc "Prefix keymap for minor mode toggles."
  :name "Toggle"
  "w" 'count-words
  "c" 'count-lines-page
  "m" 'toggle-frame-maximized
  "h" #'hl-line-mode
  "k" #'keycast-mode
  "n" #'display-line-numbers-mode
  "t" #'toggle-truncate-lines
  "p" #'spacious-padding-mode ; "padding" mnemonic
  "r" #'rainbow-mode
  "s" #'prot-modeline-subtle-mode ; "subtle" mnemonic
  "v" #'variable-pitch-mode)

(defvar-keymap prot-prefix-window-map
  :doc "Prefix keymap for windows."
  :name "Window"
  "d" 'delete-window
  "o" 'delete-other-windows
  "v" 'split-window-right
  "s" 'split-window-below
  "b" #'balance-windows-area
  "0" #'delete-window
  "1" #'delete-other-windows
  "!" #'delete-other-windows-vertically
  "2" #'split-window-below
  "@" #'split-root-window-below
  "3" #'split-window-right
  "#" #'split-root-window-right
  "w" #'other-window
  "^" #'tear-off-window
  "h" #'windmove-left
  "j" #'windmove-down
  "k" #'windmove-up
  "l" #'windmove-right
  "H" #'windmove-swap-states-left
  "J" #'windmove-swap-states-down
  "K" #'windmove-swap-states-up
  "L" #'windmove-swap-states-right)

(defvar-keymap prot-prefix-expression-map
  :doc "Prefix keymap for s-expression motions."
  :name "S-EXP"
  :repeat t
  "a" #'beginning-of-defun
  "e" #'end-of-defun
  "f" #'forward-sexp
  "b" #'backward-sexp
  "n" #'forward-list
  "p" #'backward-list
  "d" #'up-list ; confusing name for what looks "out and down" to me
  "t" #'transpose-sexps
  "u" #'backward-up-list ; the actual "up"
  "k" #'kill-sexp
  "SPC" #'prot/expreg-expand-dwim
  "DEL" #'backward-kill-sexp)

(defvar-keymap prot-prefix-search-map
  :doc "Prefix keymap for search."
  :name "Search"
  :repeat t
  "s" #'consult-line
  "k" #'consult-yank-pop
  "p" #'consult-ripgrep
  "i" #'consult-imenu
  "l" #'consult-outline)

(defvar-keymap prot-prefix-git-map
  :doc "Prefix keymap for git."
  :name "Git"
  :repeat t
  "g" #'project-vc-dir
  "d" #'color-rg-search-symbol
  "p" #'color-rg-search-symbol-in-project)

(defvar-keymap prot-prefix-jupyter-map
  :doc "Prefix keymap for jupyter."
  :name "Jupyter"
  :repeat t
  "j" #'jupyter-run-repl
  "r" #'jupyter-eval-line-or-region
  "f" #'jupyter-eval-defun
  "e" #'p-jupyter-eval-region-dwim
  "K" #'jupyter-repl-clear-cells
  "I" #'jupyter-repl-interrupt-kernel
  "i" #'jupyter-inspect-at-point
  "C" #'jupyter-eval-remove-overlays
  "c" #'p-jupyter-remove-line-overlay
  "w" #'jupyter-repl-pop-to-buffer)

(defvar-keymap prot-prefix-latex-map
  :doc "Prefix keymap for latex."
  :name "Latex"
  :repeat t
  "m" #'TeX-insert-macro
  "e" #'LaTeX-environment
  "f" #'LaTeX-fill-buffer
  "r" #'p-run-latex
  "a" #'TeX-command-run-all
  "p" #'p-select-beamer-frame
  "c" #'p-clear-latex-temp-files
  "v" #'TeX-view)

(defvar-keymap prot-prefix-map
  :doc "Prefix keymap with multiple subkeymaps."
  :name "Prot Prefix"
  "," #'execute-extended-command
  "`" #'p-switch-to-previous-buffer
  "0" #'delete-window
  "1" #'delete-other-windows
  "!" #'delete-other-windows-vertically
  "2" #'split-window-below
  "@" #'split-root-window-below
  "3" #'split-window-right
  "#" #'split-root-window-right
  "o" #'other-window
  "Q" #'save-buffers-kill-emacs
  "b" prot-prefix-buffer-map
  "c" #'world-clock
  "f" prot-prefix-file-map
  "h" help-map
  "i" prot-prefix-insert-map
  "d" #'dired-jump
  "n" narrow-map
  "p" project-prefix-map
  "r" ctl-x-r-map
  "t" prot-prefix-toggle-map
  "u" #'universal-argument
  "v" vc-prefix-map
  "w" prot-prefix-window-map
  "x" prot-prefix-expression-map
  "s" prot-prefix-search-map
  "g" prot-prefix-git-map
  "j" prot-prefix-jupyter-map
  "l" prot-prefix-latex-map)

(with-eval-after-load 'which-key
  (which-key-add-keymap-based-replacements prot-prefix-map
    "b" `("Buffer" . ,prot-prefix-buffer-map)
    "f" `("File" . ,prot-prefix-file-map)
    "h" `("Help" . ,help-map)
    "i" `("Insert" . ,prot-prefix-insert-map)
    "n" `("Narrow" . ,narrow-map)
    "p" `("Project" . ,project-prefix-map)
    "r" `("C-x r" . ,ctl-x-r-map)
    "t" `("Toggle" . ,prot-prefix-toggle-map)
    "v" `("C-x v" . ,vc-prefix-map)
    "w" `("Window" . ,prot-prefix-window-map)
    "x" `("S-EXP" . ,prot-prefix-expression-map)
    "s" `("Search" . ,prot-prefix-search-map)
    "g" `("Git" . ,prot-prefix-git-map)
    "j" `("Jupyter" . ,prot-prefix-jupyter-map)
    "l" `("Latex" . ,prot-prefix-latex-map)))

;; What follows is an older experiment with transient.  I like its
;; visuals, though find it hard to extend.  Keymaps are easier for me,
;; as I can add commands to one of the subkeymaps and they are readily
;; available without evaluating anything else.  Probably transient can
;; do this, though it is not obvious to me as to how.

;; (require 'transient)
;;
;; (transient-define-prefix prot-prefix-file nil
;;   "Transient with file commands."
;;   [["File or directory"
;;     ("f" "find-file" find-file)
;;     ("F" "find-file-other-window" find-file-other-window)]
;;    ["Directory only"
;;     ("d" "dired" dired)
;;     ("D" "dired-other-window" dired-other-window)]
;;    ["Documentation"
;;     ("l" "find-library" find-library)
;;     ("m" "man" man)]])
;;
;; (transient-define-prefix prot-prefix-buffer nil
;;   "Transient with buffer commands."
;;   [["Switch"
;;     ("b" "switch buffer" switch-to-buffer)
;;     ("B" "switch buf other window" switch-to-buffer-other-window)
;;     ("n" "next-buffer" next-buffer)
;;     ("p" "previous-buffer" previous-buffer)
;;     ("m" "buffer-menu" buffer-menu)
;;     ("q" "bury-buffer" bury-buffer)]
;;    ["Persist"
;;     ("c" "clone buffer" clone-indirect-buffer)
;;     ("C" "clone buf other window" clone-indirect-buffer-other-window)
;;     ("r" "rename-buffer" rename-buffer)
;;     ("R" "rename-uniquely" rename-uniquely)
;;     ("s" "save-buffer" save-buffer)
;;     ("w" "write-file" write-file)]
;;    ["Destroy"
;;     ("k" "kill-current-buffer" kill-current-buffer)
;;     ("K" "kill-buffer-and-window" kill-buffer-and-window)
;;     ("r" "revert-buffer" revert-buffer)]])
;;
;; (transient-define-prefix prot-prefix-search nil
;;   "Transient with search commands."
;;   [["Search"
;;     ("s" "isearch-forward" isearch-forward)
;;     ("S" "isearch-forward-regexp" isearch-forward-regexp)
;;     ("r" "isearch-backward" isearch-backward)
;;     ("R" "isearch-backward-regexp" isearch-backward-regexp)
;;     ("o" "occur" occur)]
;;    ["Edit"
;;     ("f" "flush-lines" flush-lines)
;;     ("k" "keep-lines" keep-lines)
;;     ("q" "query-replace" query-replace)
;;     ("Q" "query-replace-regexp" query-replace-regexp)]])
;;
;; (transient-define-prefix prot-prefix-window nil
;;   "Transient with window commands."
;;   [["Manage"
;;     ("b" "balance-windows" balance-windows)
;;     ("f" "fit-window-to-buffer" fit-window-to-buffer)
;;     ("t" "tear-off-window" tear-off-window)]
;;    ["Popup"
;;     ("c" "calc" calc)
;;     ("f" "list-faces-display" list-faces-display)
;;     ("r" "re-builder" re-builder)
;;     ("w" "world-clock" world-clock)]])
;;
;; ;; This is independent of the transient, though still useful.
;; (defvar-keymap prot-prefix-repeat-map
;;   :doc "Global prefix map for repeatable keybindings (per `repeat-mode')."
;;   :name "Repeat"
;;   :repeat t
;;   "n" #'next-buffer
;;   "p" #'previous-buffer
;;   "<down>" #'enlarge-window
;;   "<right>" #'enlarge-window-horizontally
;;   "<up>" #'shrink-window
;;   "<left>" #'shrink-window-horizontally)
;;
;; (transient-define-prefix prot-prefix-toggle nil
;;   "Transient with minor mode toggles."
;;   [["Interface"
;;     ("c" "context-menu-mode" context-menu-mode)
;;     ("m" "menu-bar-mode" menu-bar-mode)
;;     ("s" "scroll-bar-mode" scroll-bar-mode)
;;     ("C-t" "tool-bar-mode" tool-bar-mode)]
;;    ["Tools"
;;     ("d" "toggle-debug-on-error" toggle-debug-on-error)
;;     ("f" "follow-mode" follow-mode)
;;     ("l" "visual-line-mode" visual-line-mode)
;;     ("v" "variable-pitch-mode" variable-pitch-mode)
;;     ("t" "toggle-truncate-lines" toggle-truncate-lines)
;;     ("C-s" "window-toggle-side-windows" window-toggle-side-windows)]])
;;
;; (transient-define-prefix prot-prefix nil
;;   "Transient with common commands.
;; Commands that bring up transients have ... in their description."
;;   [["Common"
;;     ("b" "Buffer..." prot-prefix-buffer)
;;     ("f" "File..." prot-prefix-file)
;;     ("s" "Search..." prot-prefix-search)
;;     ("w" "Window..." prot-prefix-window)
;;     ("t" "Toggle..." prot-prefix-toggle)]
;;    ["Resize"
;;     ("   <up>" "Shrink vertically" shrink-window)
;;     (" <down>" "Enlarge vertically" enlarge-window)
;;     (" <left>" "Shrink horizontally" shrink-window-horizontally)
;;     ("<right>" "Enlarge horizontally" enlarge-window-horizontally)]
;;    ["Misc"
;;     ("e" "Emoji transient..." emoji-insert)
;;     ("E" "Emoji search" emoji-search)
;;     ("C-e" "Emoji buffer" emoji-list)
;;     ("RET" "Insert unicode" insert-char)
;;     ("\\" "toggle-input-method" toggle-input-method)]])

(provide 'prot-prefix)
;;; prot-prefix.el ends here
