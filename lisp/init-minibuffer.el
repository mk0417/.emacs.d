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
;; set up Orderless for better fuzzy matching
(require 'orderless)
(setq completion-styles '(orderless))
(setq completion-category-overrides '((file (styles . (partial-completion)))))

;;; Embark
(require 'embark)
(require 'embark-consult)
(global-set-key [remap describe-bindings] #'embark-bindings)
(global-set-key (kbd "C-;") 'embark-act)
(global-set-key (kbd "C-c C-o") 'embark-export)
;; use Embark to show bindings in a key prefix with `C-h`
(setq prefix-help-command #'embark-prefix-help-command)
(with-eval-after-load 'embark-consult
  (add-hook 'embark-collect-mode-hook #'consult-preview-at-point-mode))
;; embark action integration with which-key
(defun embark-which-key-indicator ()
  (lambda (&optional keymap targets prefix)
    (if (null keymap)
        (which-key--hide-popup-ignore-command)
      (which-key--show-keymap
       (if (eq (plist-get (car targets) :type) 'embark-become)
           "Become"
         (format "Act on %s '%s'%s"
                 (plist-get (car targets) :type)
                 (embark--truncate-target (plist-get (car targets) :target))
                 (if (cdr targets) "…" "")))
       (if prefix
           (pcase (lookup-key keymap prefix 'accept-default)
             ((and (pred keymapp) km) km)
             (_ (key-binding prefix 'accept-default)))
         keymap)
       nil nil t (lambda (binding)
                   (not (string-suffix-p "-argument" (cdr binding))))))))

(setq embark-indicators '(embark-which-key-indicator embark-highlight-indicator embark-isearch-highlight-indicator))

;;; Consult
;; set some consult bindings
(setq completion-in-region-function #'consult-completion-in-region)

(defmacro p-no-consult-preview (&rest cmds)
  `(with-eval-after-load 'consult
     (consult-customize ,@cmds :preview-key (kbd "M-v"))))
(p-no-consult-preview consult-ripgrep
                      consult-git-grep
                      consult-grep
                      consult-bookmark
                      consult-recent-file
                      consult-xref
                      consult-yank-pop
                      consult--source-bookmark)
(global-set-key [remap switch-to-buffer] 'consult-buffer)
(global-set-key [remap switch-to-buffer-other-window] 'consult-buffer-other-window)

;; export to do editing
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
    "sr" '(consult-yank-pop :which-key "consult yank")
    "sp" '(consult-ripgrep :which-key "consult-rg project")
    "si" '(consult-imenu :which-key "consult imenu")
    "sl" '(consult-outline :which-key "consult outline")))

(provide 'init-minibuffer)
;;;;; init-minibuffer.el ends here
