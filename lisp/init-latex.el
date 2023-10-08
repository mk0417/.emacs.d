;;;;; init-latex.el --- Latex -*- lexical-binding: t -*-

;; https://github.com/SystemCrafters/crafted-emacs/blob/master/modules/crafted-latex.el
(prot-emacs-configure 
  (:delay 10)
  (with-eval-after-load 'latex
    (setq TeX-auto-save t)
    (setq TeX-parse-self t)
    (setq-default TeX-master nil)

    (tex-pdf-mode)

    (TeX-source-correlate-mode)

    (add-to-list 'LaTeX-indent-environment-list '("lstlisting" current-indentation))
    (add-to-list 'LaTeX-indent-environment-list '("tikzcd" LaTeX-indent-tabular))
    (add-to-list 'LaTeX-indent-environment-list '("tikzpicture" current-indentation))
    (add-to-list 'LaTeX-verbatim-environments "lstlisting")
    (add-to-list 'LaTeX-verbatim-environments "Verbatim")
    (add-to-list 'LaTeX-verbatim-macros-with-braces "lstinline")
    (add-to-list 'LaTeX-verbatim-macros-with-delims "lstinline")

    (setq TeX-view-program-selection '((output-pdf "PDF Tools")))
    (setq TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view)))
    (setq TeX-source-correlate-start-server t)
    (setq TeX-electric-sub-and-superscript t)
    (setq LaTeX-electric-left-right-brace t)
    (setq TeX-electric-math (cons "$" "$"))

    (add-hook 'LaTeX-mode-hook #'auto-fill-mode)
    (add-hook 'LaTeX-mode-hook #'LaTeX-math-mode)
  
    ;; to have the buffer refresh after compilation
    (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)

    (dolist (dir '("/Applications/Skim.app/Contents/SharedSupport"))
      (add-to-list 'exec-path dir))
    (setq TeX-view-program-list '(("Skim" "open -a Skim.app %o")))
    (setq TeX-view-program-selection '((output-pdf "Skim")))

    ;; Select beamer frame block
    (defun p-select-beamer-frame ()
      (interactive)
      (search-backward "\\begin{frame}")
      (set-mark (line-beginning-position))
      (search-forward "\\end{frame}")
      (end-of-line))

    ;; Clear temp files
    (defun p-clear-latex-temp-files ()
      (interactive)
      (let ((default-directory default-directory)
            (command
             (concat "find . -maxdepth 1 \\( -name \"*.aux\" -o -name \"*.log\" -o -name \"*.gz\" -o -name \"*.listing\""
                     " -o -name \"*.out\" -o -name \"*.fls\" -o -name \"*.nav\" -o -name \"*.snm\" -o -name \"*.vrb\""
                     " -o -name \"*.toc\" -o -name \"*.fdb_latexmk\" -o -name \"auto\""
                     " -o -name \"_minted-*\" -type d \\) -exec rm -rf {} +")))
        (shell-command command)
        (message "Files and directories deleted.")))

    (defun p-run-latex ()
      (interactive)
      (TeX-save-document (TeX-master-file))
      (TeX-command "LaTeX" 'TeX-master-file))))

(provide 'init-latex)
