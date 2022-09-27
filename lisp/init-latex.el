;;;;; init-latex.el --- Latex -*- lexical-binding: t -*-

;;; Install packages
(straight-use-package 'auctex)

;;; Latex
(with-eval-after-load 'latex
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil)
  (setq TeX-clean-confirm t)

  ;; Compile to pdf
  (tex-pdf-mode)

  ;; Correlate the source and the output
  (TeX-source-correlate-mode)

  ;; Set a correct indentation in a few additional environments
  (add-to-list 'LaTeX-indent-environment-list '("lstlisting" current-indentation))
  (add-to-list 'LaTeX-indent-environment-list '("tikzcd" LaTeX-indent-tabular))
  (add-to-list 'LaTeX-indent-environment-list '("tikzpicture" current-indentation))

  ;; Add a few macros and environment as verbatim
  (add-to-list 'LaTeX-verbatim-environments "lstlisting")
  (add-to-list 'LaTeX-verbatim-environments "Verbatim")
  (add-to-list 'LaTeX-verbatim-macros-with-braces "lstinline")
  (add-to-list 'LaTeX-verbatim-macros-with-delims "lstinline")

  ;; Electric pairs in auctex
  (setq TeX-electric-sub-and-superscript t)
  (setq LaTeX-electric-left-right-brace t)
  (setq TeX-electric-math (cons "$" "$"))

  (defun p-latex-enable-modes ()
    (setq fill-column 100)
    (setq reftex-plug-into-AUCTeX t)
    (auto-fill-mode)
    (turn-on-reftex)
    (LaTeX-math-mode))

  (add-hook 'LaTeX-mode-hook #'p-latex-enable-modes)

  ;; Open all buffers with the math mode and auto-fill mode
  ;; (add-hook 'LaTeX-mode-hook #'auto-fill-mode)
  ;; (add-hook 'LaTeX-mode-hook #'LaTeX-math-mode)

  ;; Add support for references
  ;; (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  ;; (setq reftex-plug-into-AUCTeX t)

  ;; To have the buffer refresh after compilation
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer))

;; Message the user if the latex executable is not found
(add-hook 'tex-mode-hook
          (lambda () (unless crafted-latex-latexp (message "latex executable not found"))))

(dolist (dir '("/Applications/Skim.app/Contents/SharedSupport"))
  (add-to-list 'exec-path dir))

(setq TeX-view-program-list '(("Skim" "open -a Skim.app %o")))
(setq TeX-view-program-selection '((output-pdf "Skim")))

(provide 'init-latex)
;;;;; init-latex.el ends here
