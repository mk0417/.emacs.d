;;;;; init.el --- Init file -*- lexical-binding: t -*-

;;; Produce backtraces when errors occur: can be helpful to diagnose startup issues
(setq debug-on-error t)

;;; Speed up
;; https://github.com/seagle0128/.emacs.d/blob/master/init.el
(setq auto-mode-case-fold nil)

;;; Initial scratch buffer message
(setq initial-scratch-message "")

(with-eval-after-load 'project-vc
  (setq package-vc-register-as-project nil)) ; Emacs 30

;;; Setup straight as package manager
;; https://github.com/doomemacs/doomemacs/issues/5682
(defvar native-comp-deferred-compilation-deny-list nil)
(setq straight-repository-branch "develop")
;; (setq straight-vc-git-default-clone-depth 1)
(setq straight-vc-git-default-clone-depth '(1 single-branch))
;; quickier init time
;; https://emacs.stackexchange.com/questions/71302/reducing-straight-el-bloat
(setq straight-check-for-modifications '(check-on-save find-when-checking))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(defmacro prot-emacs-keybind (keymap &rest definitions)
  "Expand key binding DEFINITIONS for the given KEYMAP.
DEFINITIONS is a sequence of string and command pairs."
  (declare (indent 1))
  (unless (zerop (% (length definitions) 2))
    (error "Uneven number of key+command pairs"))
  (let ((keys (seq-filter #'stringp definitions))
        ;; We do accept nil as a definition: it unsets the given key.
        (commands (seq-remove #'stringp definitions)))
    `(when-let (((keymapp ,keymap))
                (map ,keymap))
       ,@(mapcar
          (lambda (pair)
            (let* ((key (car pair))
                   (command (cdr pair))
                   (command-symbol (if command `(function ,command) command)))
              (unless (and (null key) (null command))
                `(define-key map (kbd ,key) ,command-symbol))))
          (cl-mapcar #'cons keys commands)))))

;; Load path
(dolist (path '("lisp" "prot-lisp" "prot-emacs-modules"))
  (add-to-list 'load-path (locate-user-emacs-file path)))

(require 'init-env)
(require 'init-theme)
(require 'prot-emacs-font)
(require 'init-modeline)
(require 'init-defaults)
(require 'init-evil)
(require 'init-completion)
(require 'prot-emacs-completion-vertico)
(require 'init-windows)
(require 'init-func)
(require 'init-keybindings)

(defun p-config-after-startup ()
  (cl-dolist (mod (list
                   'prot-emacs-search
                   'prot-emacs-langs
                   'prot-emacs-dired
                   'prot-emacs-window
                   ;; 'prot-emacs-org
                   'prot-emacs-write
                   'prot-emacs-git
                   'prot-emacs-theme-extras
                   'init-tree-sitter
                   'init-programming
                   'init-keychord
                   'init-git
                   'init-avy
                   'init-snippet
                   'init-latex
                   'init-org
                   'init-xah
                   'init-launcher
                   'init-lsp-bridge))
    (require mod nil t)))

(add-hook 'emacs-startup-hook #'p-config-after-startup)

;;; Profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s seconds."
                     (emacs-init-time "%.2f"))))

;;;;; init.el ends here
