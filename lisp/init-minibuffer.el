;;;;; init-minibuffer.el --- Minibuffer -*- lexical-binding: t -*-

;;; Install packages
(straight-use-package 'marginalia)
(straight-use-package 'orderless)
(straight-use-package 'consult)
(straight-use-package 'embark)
(straight-use-package 'embark-consult)
(straight-use-package 'wgrep)

;;; Marginalia
(require 'marginalia)
(setq marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
(marginalia-mode 1)

;;; Orderless
;; Set up orderless for better matching
(require 'orderless)
(setq completion-styles '(orderless))
(setq completion-category-overrides '((file (styles . (partial-completion)))))

(defun without-if-bang (pattern _index _total)
  (cond
   ((equal "!" pattern)
    '(orderless-literal . ""))
   ((string-prefix-p "!" pattern)
    `(orderless-without-literal . ,(substring pattern 1)))))
(add-to-list 'orderless-style-dispatchers #'without-if-bang)

;;; Embark
(require 'embark)
(require 'embark-consult)
(global-set-key [remap describe-bindings] #'embark-bindings)
(global-set-key (kbd "C-;") 'embark-act)
(global-set-key (kbd "C-c C-o") 'embark-export)

;; Use Embark to show bindings in a key prefix with `C-h`
(setq prefix-help-command #'embark-prefix-help-command)

(with-eval-after-load 'embark-consult
  (add-hook 'embark-collect-mode-hook #'consult-preview-at-point-mode))

(setq embark-verbose-indicator-display-action
      '((display-buffer-at-bottom)))

(define-key embark-identifier-map "R" #'consult-ripgrep)
(define-key embark-identifier-map "J" #'consult-line)

;;; Consult
(setq consult-line-start-from-top t)
(setq consult-async-input-debounce 0.5)
(setq consult-async-input-throttle 0.8)

(setq completion-in-region-function #'consult-completion-in-region)

(defun p-consult-line-at-point ()
  (interactive)
  (consult-line (thing-at-point 'symbol)))

(defun p-consult-ripgrep-at-point ()
  (interactive)
  (consult-ripgrep nil (thing-at-point 'symbol)))

(defun p-consult-ripgrep-dir ()
  (interactive)
  (consult-ripgrep (universal-argument)))

(consult-customize
 consult-ripgrep
 p-consult-ripgrep-at-point
 consult-git-grep
 consult-grep
 consult-bookmark
 consult-recent-file
 consult-xref
 consult-yank-pop
 consult--source-bookmark
 :preview-key "M-v")

(global-set-key [remap switch-to-buffer] 'consult-buffer)
(global-set-key [remap switch-to-buffer-other-window] 'consult-buffer-other-window)

;; Export to do editing
;; https://github.com/zilongshanren/emacs.d/blob/develop/lisp/init-funcs.el
(defun p-embark-export-write ()
  (interactive)
  (require 'embark)
  (require 'wgrep)
  (pcase-let ((`(,type . ,candidates)
               (run-hook-with-args-until-success 'embark-candidate-collectors)))
    (pcase type
      ('consult-grep (let ((embark-after-export-hook #'wgrep-change-to-wgrep-mode))
                       (embark-export)))
      ('file (let ((embark-after-export-hook #'wdired-change-to-wdired-mode))
               (embark-export)))
      ('consult-location (let ((embark-after-export-hook #'occur-edit-mode))
                           (embark-export)))
      (x (user-error "embark category %S doesn't support writable export" x)))))

(global-set-key (kbd "C-s") 'consult-line)
(define-key minibuffer-local-map (kbd "C-r") 'consult-history)
(global-set-key (kbd "C-c C-.") 'p-embark-export-write)

;;; Keybindings
(with-eval-after-load 'evil
  (general-create-definer p-space-leader-def
    :prefix "SPC"
    :states '(normal visual))
  (p-space-leader-def
    "f"  '(:ignore t :which-key "file")
    "fr" '(consult-recent-file :which-key "recent file")
    "b"  '(:ignore t :which-key "buffer")
    "bb" '(consult-buffer :which-key "consult switch buffer")
    "bm" '(consult-bookmark :which-key "consult bookmark")
    "bo" '(consult-buffer-other-window :which-key "open file in another window")
    "bp" '(consult-project-buffer :which-key "enhanced consult switch buffer")
    "s"  '(:ignore t :which-key "search")
    "ss" '(consult-line :which-key "consult line")
    "sb" '(p-consult-line-at-point :which-key "consult line at point")
    "sk" '(consult-yank-pop :which-key "consult yank")
    "sp" '(consult-ripgrep :which-key "consult-rg project")
    "sd" '(p-consult-ripgrep-at-point :which-key "consult-rg at point")
    "sr" '(p-consult-ripgrep-dir :which-key "consult-rg dir")
    "si" '(consult-imenu :which-key "consult imenu")
    "sl" '(consult-outline :which-key "consult outline")))

(provide 'init-minibuffer)
;;;;; init-minibuffer.el ends here
