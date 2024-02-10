;;; init-latex.el --- Latex -*- lexical-binding: t -*-

(prot-emacs-configure
  (:delay 5)
  ;; https://emacs.stackexchange.com/questions/41321/when-to-specify-a-package-name-in-use-packages-ensure-tag/41324#41324
  ;; prot-emacs-package will require auctex. This seems not to work with this package
  ;; So I just install it without require
  (prot-emacs-package-install 'auctex t)

  (prot-emacs-package engrave-faces
    (:install t)
    (:delay 2))

  (with-eval-after-load 'latex
    (setq TeX-auto-save t)
    (setq TeX-parse-self t)
    (setq-default TeX-master t)
    (setq LaTeX-indent-level 4)
    (setq LaTeX-item-indent 0)
    (setq TeX-command-extra-options "-shell-escape")
    (setq TeX-electric-sub-and-superscript t)
    (setq LaTeX-electric-left-right-brace t)
    (setq TeX-electric-math (cons "$" "$"))

    ;; compile to pdf
    (tex-pdf-mode)

    ;; correlate the source and the output
    (TeX-source-correlate-mode)

    (add-to-list 'LaTeX-indent-environment-list '("lstlisting" current-indentation))
    (add-to-list 'LaTeX-indent-environment-list '("tikzcd" LaTeX-indent-tabular))
    (add-to-list 'LaTeX-indent-environment-list '("tikzpicture" current-indentation))
    (add-to-list 'LaTeX-verbatim-environments "lstlisting")
    (add-to-list 'LaTeX-verbatim-environments "Verbatim")
    (add-to-list 'LaTeX-verbatim-macros-with-braces "lstinline")
    (add-to-list 'LaTeX-verbatim-macros-with-delims "lstinline")

    ;; open all buffers with the math mode and auto-fill mode
    (add-hook 'LaTeX-mode-hook #'auto-fill-mode)
    (add-hook 'LaTeX-mode-hook #'LaTeX-math-mode)

    ;; line number mode
    (add-hook 'LaTeX-mode-hook #'display-line-numbers-mode)

    ;; add support for references
    (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
    (setq reftex-plug-into-AUCTeX t)

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
                     " -o -name \"*.toc\" -o -name \"*.fdb_latexmk\" -o -name \"*.bcf\" -o -name \"*.xml\" -o -name \"auto\""
                     " -o -name \"_minted-*\" -type d \\) -exec rm -rf {} +")))
        (shell-command command)
        (message "Files and directories deleted.")))

    (defun p-run-latex ()
      (interactive)
      (TeX-save-document (TeX-master-file))
      (TeX-command "LaTeX" 'TeX-master-file))))

(provide 'init-latex)
