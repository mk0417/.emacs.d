;;;;; init-minibuffer.el --- Config for minibuffer completion -*- lexical-binding: t; -*-

;;; package
(straight-use-package 'marginalia)
(straight-use-package 'orderless)
(straight-use-package 'consult)
(straight-use-package 'embark)
(straight-use-package 'embark-consult)
(straight-use-package 'consult-dir)
;; (straight-use-package '(mct :type git :host gitlab :repo "protesilaos/mct" :files ("*.el" "extensions/*.el")))
(straight-use-package '(vertico :files ("*.el" "extensions/*.el")))
(straight-use-package 'wgrep)

;;; completion
(setq completion-ignore-case t)
(setq completion-cycle-threshold 2)

(add-hook 'completion-list-mode-hook (lambda () (setq-local global-hl-line-mode nil)))

;;; mct
;; https://gitlab.com/protesilaos/mct
;; (setq mct-live-update-delay 0.5)
;; (setq mct-live-completion t)
;; (setq mct-persist-dynamic-completion t)
;; (setq mct-completion-passlist '(embark-prefix-help-command Info-goto-node Info-index Info-menu vc-retrieve-tag))

;; (mct-minibuffer-mode 1)

;; (dolist (map (list mct-minibuffer-local-completion-map
;; 		   mct-minibuffer-completion-list-map))
;;   (define-key map (kbd "C-.") #'mct-avy-choose-completion-exit))

;;; vertico
;; Prot will not continue to develop mct
;; https://protesilaos.com/codelog/2022-04-14-emacs-discontinue-mct/
(setq vertico-count 20)
(setq vertico-cycle t)
(setq vertico-resize t)
(setq vertico-scroll-margin 0)

(vertico-mode)
(vertico-multiform-mode)

(setq vertico-multiform-commands
      '((p-consult-rg-current-dir buffer)
        (p-consult-rg-at-point-project buffer)
        (p-consult-rg-other-dir buffer)
        (p-consult-rg-at-point-current-dir buffer)
        (consult-ripgrep buffer)))

;; current item indicator
;; https://github.com/minad/vertico/wiki#prefix-current-candidate-with-arrow
(advice-add #'vertico--format-candidate :around
            (lambda (orig cand prefix suffix index _start)
              (setq cand (funcall orig cand prefix suffix index _start))
              (concat
               (if (= vertico--index index)
                   (propertize "» " 'face 'vertico-current)
                 "  ")
               cand)))

;;; orderless
(setq completion-styles '(orderless basic))
(setq completion-category-defaults nil)
(setq completion-category-overrides '((file (styles partial-completion))))

;;; marginalia
(setq marginalia-max-relative-age 0)
(add-hook 'after-init-hook 'marginalia-mode)

;;; consult
(setq consult-line-start-from-top t)
(setq consult-line-numbers-widen t)
(setq consult-async-min-input 2)
(setq consult-async-refresh-delay  0.15)
(setq consult-async-input-throttle 0.2)
(setq consult-async-input-debounce 0.1)
(setq consult-preview-key 'any)

(with-eval-after-load 'consult
  (add-hook 'completion-list-mode-hook #'consult-preview-at-point-mode)
  (defmacro p-no-consult-preview (&rest cmds)
    `(with-eval-after-load 'consult
       (consult-customize ,@cmds :preview-key (kbd "M-v"))))
  (p-no-consult-preview consult-ripgrep
                        consult-git-grep
                        consult-grep
                        consult-bookmark
                        consult-recent-file
                        consult-xref
                        consult--source-bookmark
                        p-consult-rg-at-point-project
                        p-consult-rg-current-dir
                        p-consult-rg-other-dir
                        p-consult-rg-at-point-current-dir)

  (global-set-key [remap switch-to-buffer] 'consult-buffer)
  (global-set-key [remap switch-to-buffer-other-window] 'consult-buffer-other-window)
  (global-set-key [remap switch-to-buffer-other-frame] 'consult-buffer-other-frame)
  (global-set-key [remap goto-line] 'consult-goto-line)
  (global-set-key (kbd "C-x l") 'consult-line)
  (define-key minibuffer-local-map (kbd "C-r") 'consult-history))

(autoload 'consult--grep "consult")

(defun p-consult-at-point-line (&optional initial)
  (interactive)
  (consult-line (thing-at-point 'symbol)))

(defun p-consult-rg-at-point-project (&optional dir)
  (interactive)
  (consult--grep "Ripgrep" #'consult--ripgrep-builder dir (thing-at-point 'symbol)))

(defun p-consult-rg-current-dir (&optional initial)
  (interactive "P")
  (if (equal buffer-file-name nil)
      (consult--grep "Ripgrep current dir" #'consult--ripgrep-builder "/Users/ml/" initial)
    (consult--grep "Ripgrep current dir" #'consult--ripgrep-builder (file-name-directory buffer-file-name) initial)))

(defun p-consult-rg-other-dir (&optional initial)
  (interactive "P")
  (consult--grep "Ripgrep current dir" #'consult--ripgrep-builder (read-directory-name "consult-rg directory:") initial))

(defun p-consult-rg-at-point-current-dir ()
  (interactive)
  (consult--grep "Ripgrep current dir" #'consult--ripgrep-builder (file-name-directory buffer-file-name) (thing-at-point 'symbol)))

(defun p-consult-fd-local (&optional dir initial)
  (interactive "P")
  (if (equal buffer-file-name nil)
      (consult-find "~/" initial)
    (consult-find dir initial)))

(defun p-consult-fd-global (&optional initial)
  (interactive "P")
  (consult-find (read-directory-name "consult-find directory:") initial))

;;; embark
(autoload 'embark-act "embark")
(autoload 'embark-export "embark")

(global-set-key (kbd "C-;") 'embark-act)
(global-set-key (kbd "C-c C-o") 'embark-export)

;;; embark action integration with which-key
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

(defun embark-hide-which-key-indicator (fn &rest args)
  (which-key--hide-popup-ignore-command)
  (let ((embark-indicators
         (remq #'embark-which-key-indicator embark-indicators)))
    (apply fn args)))

(advice-add #'embark-completing-read-prompter :around #'embark-hide-which-key-indicator)

;;; open folder in Finder
;; https://book.emacs-china.org/#org404700d
(defun p-open-directory-externally (file)
  (interactive "fOpen externally: ")
  (shell-command-to-string (encode-coding-string (format "open %s" (file-name-directory (expand-file-name file))) 'gbk)))

(with-eval-after-load 'embark
  (setq embark-keymap-prompter-key ",")
  (setq embark-indicators '(embark-which-key-indicator embark-highlight-indicator embark-isearch-highlight-indicator))
  (add-to-list 'embark-indicators #'embark-vertico-indicator)
  (require 'embark-consult)
  (add-hook 'embark-collect-mode-hook 'embark-consult-preview-minor-mode)
  (setq embark-indicators
        '(embark-which-key-indicator
          embark-highlight-indicator
          embark-isearch-highlight-indicator))
  (define-key embark-file-map (kbd "O") #'p-open-directory-externally))

;;; export to do editing
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

;;; keystrokes feedback interval
(setq echo-keystrokes 0.02)

;;; keybindings
(global-set-key (kbd "C-c C-d") 'consult-dir)
(global-set-key (kbd "C-c C-j") 'consult-dir-jump-file)
(global-set-key (kbd "C-c C-.") 'p-embark-export-write)

(with-eval-after-load 'evil
  (general-create-definer p-space-leader-def
    :prefix "SPC"
    :states '(normal visual))
  (p-space-leader-def
    "f"  '(:ignore t :which-key "file")
    "fr" '(consult-recent-file :which-key "recent file")
    "fd" '(consult-dir :which-key "find directory")
    "b"  '(:ignore t :which-key "buffer")
    "bb" '(consult-buffer :which-key "consult switch buffer")
    "bo" '(consult-buffer-other-window :which-key "open file in another window")
    "s"  '(:ignore t :which-key "search")
    "ss" '(consult-line :which-key "consult line")
    "sS" '(p-consult-at-point-line :which-key "consult at-point line")
    "sr" '(consult-yank-pop :which-key "consult yank")
    "sm" '(consult-multi-occur :which-key "consult multi occur")
    "sp" '(consult-ripgrep :which-key "consult-rg project")
    "sP" '(p-consult-rg-at-point-project :which-key "consult-rg at-point project")
    "sd" '(p-consult-rg-current-dir :which-key "consult-rg current dir")
    "sD" '(p-consult-rg-at-point-current-dir :which-key "consult-rg at-point current dir")
    "so" '(p-consult-rg-other-dir :which-key "consult-rg other dir")
    "sf" '(p-consult-fd-global :which-key "consult-fd global files")
    "sF" '(p-consult-fd-local :which-key "consult-fd local files")
    "si" '(consult-imenu :which-key "consult imenu")
    "sl" '(consult-outline :which-key "consult outline")
    "t"  '(:ignore t :which-key "toggle")
    "to" '(p-open-directory-externally :which-key "open directory externally")))

(provide 'init-minibuffer)
;;;;; init-minibuffer.el ends here
