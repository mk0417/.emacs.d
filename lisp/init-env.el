;;; init-env.el --- Eenvironment for my dotemacs -*- lexical-binding: t -*-

;; Inherit variables from .zshrc
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

;; Fix emacs-jupyter issue for Python 3.11.*
;; https://github.com/nnicandro/emacs-jupyter/issues/439
(setenv "PYDEVD_DISABLE_FILE_VALIDATION" "1")

;; https://github.com/d12frosted/homebrew-emacs-plus/issues/383
;; https://github.com/d12frosted/homebrew-emacs-plus/issues/383#issuecomment-1075742794
(when (eq system-type 'darwin)
  (setq insert-directory-program "/opt/homebrew/bin/gls"))

(provide 'init-env)
;;; init-env.el ends here
