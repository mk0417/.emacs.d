;;;;; init.el --- Init file -*- lexical-binding: t -*-

;;; Produce backtraces when errors occur: can be helpful to diagnose startup issues
(setq debug-on-error t)

;;; Speed up
;; https://github.com/seagle0128/.emacs.d/blob/master/init.el
(setq auto-mode-case-fold nil)

;;; Initial scratch buffer message
(setq initial-scratch-message
      (concat ";; Hello Peng, welcome to Emacs and happy hacking\n"
              (format ";; Emacs version: %s\n" (car (split-string emacs-version)))))

;;; Inherit variables from .zshrc
;; http://xahlee.info/emacs/emacs/emacs_env_var_paths.html
(let ((emacs-init-env-path
       (list (expand-file-name "~/anaconda3/bin")
             (expand-file-name "~/anaconda3/bin/jupyter")
             (expand-file-name "~/.emacs.d/bin")
             (expand-file-name "~/.cargo/bin")
             "/opt/homebrew/Caskroom/miniforge/base/bin"
             "/opt/homebrew/Caskroom/miniforge/base/bin/jupyter"
             "/opt/homebrew/Caskroom/mambaforge/base/bin"
             "/opt/homebrew/Caskroom/mambaforge/base/bin/jupyter"
             "/opt/homebrew/bin/"
             "/usr/local/bin"
             "/usr/local/sbin"
             "/usr/bin"
             "/bin"
             "/usr/sbin"
             "/sbin"
             "/Applications/Stata/StataMP.app/Contents/MacOS/"
             "/Applications/Stata/StataMP.app/Contents/MacOS/stata"
             "/Library/TeX/texbin"
             "/Applications/Emacs.app/Contents/MacOS/bin")))
  (setenv "PATH" (mapconcat 'identity emacs-init-env-path ":"))
  (setq exec-path (append emacs-init-env-path (list "." exec-directory))))

;;; Load config
(add-to-list 'load-path (expand-file-name "lisp/" user-emacs-directory) t)

(require 'init-ui)
(require 'init-defaults)
(require 'init-evil)
(require 'init-editing)
(require 'init-osx)
(require 'init-minibuffer)
(require 'init-mct)
;; (require 'init-vertico)
(require 'init-utils)
(require 'init-windows)
;; (require 'init-programming)
;; (require 'init-keychord)
;; (require 'init-git)
;; (require 'init-project)
;; (require 'init-avy)
;; (require 'init-org)
;; (require 'init-template)
;; (require 'init-notes)
;; (require 'init-latex)
;; (require 'init-dired)
;; (require 'init-modeline)
;; (require 'init-xah)
;; (require 'init-lsp-bridge)
;; (require 'init-vterm)

;;; Disable corfu and eglot to favor lsp-bridge
;; (require 'init-completion)
;; (require 'init-eglot)

(defun p-config-after-startup ()
  (cl-dolist (mod (list
                   'init-programming
                   'init-keychord
                   'init-git
                   'init-project
                   'init-avy
                   'init-org
                   'init-snippet
                   'init-notes
                   'init-latex
                   'init-dired
                   'init-modeline
                   'init-xah
                   'init-lsp-bridge))
    (require mod nil t)))

(add-hook 'emacs-startup-hook #'p-config-after-startup)

;;; Profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s seconds."
                     (emacs-init-time "%.2f"))))

;;; Make GC pauses faster by decreasing the threshold
(setq gc-cons-threshold (* 8 1024 1024))
(setq gc-cons-percentage 0.1)

;;;;; init.el ends here
