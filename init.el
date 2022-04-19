;;;;; init.el --- Load configuration -*- lexical-binding: t -*-

;;; produce backtraces when errors occur: can be helpful to diagnose startup issues
(setq debug-on-error t)

;;; initial scratch buffer message
(setq initial-scratch-message
      (concat ";; Hello Peng, welcome to EMACS and happy hacking\n"
              (format ";; Emacs version: %s\n" (car (split-string emacs-version)))))

;;; startup time
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s seconds." (emacs-init-time "%.2f"))))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;;; inherit variables from .zshrc
;; http://xahlee.info/emacs/emacs/emacs_env_var_paths.html
(let ((p-env-path
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
  (setenv "PATH" (mapconcat 'identity p-env-path ":"))
  (setq exec-path (append p-env-path (list "." exec-directory))))

;;; init
;; use straight to manage packages
(require 'init-straight)
(require 'init-default)
(require 'init-ui)
(require 'init-theme)
(require 'init-evil)
(require 'init-git)
(require 'init-org)
(require 'init-dired)
(require 'init-function)
(require 'init-minibuffer)
;; (require 'init-company)
(require 'init-completion)
(require 'init-text)
(require 'init-avy)
(require 'init-programming)
(require 'init-note)
(require 'init-template)
;; (require 'init-tree-sitter)

;;; Make GC pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))

;;;;; init.el ends here
