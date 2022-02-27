;;; init.el --- Load configuration -*- lexical-binding: t -*-

;; produce backtraces when errors occur: can be helpful to diagnose startup issues
(setq debug-on-error t)

;; initial scratch buffer message
(setq initial-scratch-message
      (concat ";; Hello Peng, welcome to EMACS and happy hacking\n"
	      (format ";; Emacs version: %s\n" (car (split-string emacs-version)))))

;; startup time
(add-hook 'emacs-startup-hook
	  (lambda ()
	    (message "*** Emacs loaded in %s."
		     (format "%.2f seconds"
			     (float-time
			      (time-subtract after-init-time before-init-time))))))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; inherit variables from .zshrc
;; http://xahlee.info/emacs/emacs/emacs_env_var_paths.html
(let ((p-env-path
       '("/Users/ml/anaconda3/bin"
	 "/Users/ml/anaconda3/bin/jupyter"
	 "/usr/local/bin"
	 "/usr/local/sbin"
	 "/usr/bin"
	 "/bin"
	 "/usr/sbin"
	 "/sbin"
	 "/Applications/Stata/StataMP.app/Contents/MacOS/"
	 "/Applications/Stata/StataMP.app/Contents/MacOS/stata"
	 "/Library/TeX/texbin"
	 "/Users/ml/.emacs.d/bin"
	 "/Users/ml/.cargo/bin"
	 "/Applications/Emacs.app/Contents/MacOS/bin")))
  (setenv "PATH" (mapconcat 'identity p-env-path ":") )
  (setq exec-path (append p-env-path (list "." exec-directory))))

;; init
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

;; Make GC pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))
