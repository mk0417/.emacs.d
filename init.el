;;;;; init.el -*- lexical-binding: t -*-

;;; Produce backtraces when errors occur: can be helpful to diagnose startup issues
(setq debug-on-error t)

;;; Initial scratch buffer message
(setq initial-scratch-message
      (concat ";; Hello Peng, welcome to Emacs and happy hacking\n"
              (format ";; Emacs version: %s\n" (car (split-string emacs-version)))))

;;; Profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s seconds."
                     (emacs-init-time "%.2f"))))

;;; Inherit variables from .zshrc
;; http://xahlee.info/emacs/emacs/emacs_env_var_paths.html
(let ((emacs-init-env-path
       (list (expand-file-name "~/anaconda3/bin")
             (expand-file-name "~/anaconda3/bin/jupyter")
             (expand-file-name "~/.emacs.d/bin")
             (expand-file-name "~/.cargo/bin")
             "/opt/homebrew/Caskroom/miniforge/base/bin"
             "/opt/homebrew/Caskroom/miniforge/base/bin/jupyter"
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
(add-to-list 'load-path (expand-file-name "lisp/" user-emacs-directory))

(require 'init-ui)
(require 'init-defaults)
(require 'init-evil)
(require 'init-editing)
(require 'init-osx)
(require 'init-minibuffer)
(require 'init-completion)
(require 'init-windows)
(require 'init-git)
(require 'init-utils)
(require 'init-keychord)
(require 'init-programming)
(require 'init-project)
(require 'init-avy)
(require 'init-org)
(require 'init-template)
(require 'init-notes)

;;; Make GC pauses faster by decreasing the threshold
(setq gc-cons-threshold (* 2 1000 1000))
