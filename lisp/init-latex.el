;;;;; init-latex.el --- Latex -*- lexical-binding: t -*-

;;; Install packages
(straight-use-package 'auctex)
(straight-use-package 'evil-tex)

;;; Latex
(setq TeX-auto-save t)
(setq TeX-parse-self t)
;; Do not prompt for a master file.
(setq-default TeX-master t)
(setq TeX-clean-confirm t)
;; Electric pairs in auctex
(setq TeX-electric-sub-and-superscript t)
(setq LaTeX-electric-left-right-brace t)
(setq TeX-electric-math (cons "$" "$"))
;; Correlate the source and the output
(setq TeX-source-correlate-mode t)
(setq TeX-source-correlate-method 'synctex)
(setq TeX-source-correlate-start-server nil)
(setq TeX-electric-sub-and-superscript t)
(setq TeX-save-query nil)
(setq LaTeX-fill-break-at-separators nil)
(setq LaTeX-item-indent 0)
(setq LaTeX-indent-level 4)

(with-eval-after-load 'latex
  ;; Compile to pdf
  (tex-pdf-mode)
  ;; Set a correct indentation in a few additional environments
  (add-to-list 'LaTeX-indent-environment-list '("lstlisting" current-indentation))
  (add-to-list 'LaTeX-indent-environment-list '("tikzcd" LaTeX-indent-tabular))
  (add-to-list 'LaTeX-indent-environment-list '("tikzpicture" current-indentation))
  ;; Add a few macros and environment as verbatim
  (add-to-list 'LaTeX-verbatim-environments "lstlisting")
  (add-to-list 'LaTeX-verbatim-environments "Verbatim")
  (add-to-list 'LaTeX-verbatim-macros-with-braces "lstinline")
  (add-to-list 'LaTeX-verbatim-macros-with-delims "lstinline")

  (defun p-latex-enable-modes ()
    (setq fill-column 100)
    (setq reftex-plug-into-AUCTeX t)
    (auto-fill-mode)
    (turn-on-reftex)
    (LaTeX-math-mode))

  (add-hook 'LaTeX-mode-hook #'p-latex-enable-modes)
  (add-hook 'LaTeX-mode-hook #'evil-tex-mode)
  (add-hook 'LaTeX-mode-hook #'adaptive-wrap-prefix-mode)

  ;; Open all buffers with the math mode and auto-fill mode
  ;; (add-hook 'LaTeX-mode-hook #'auto-fill-mode)
  ;; (add-hook 'LaTeX-mode-hook #'LaTeX-math-mode)

  ;; Add support for references
  ;; (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  ;; (setq reftex-plug-into-AUCTeX t)

  ;; To have the buffer refresh after compilation
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer))

(dolist (dir '("/Applications/Skim.app/Contents/SharedSupport"))
  (add-to-list 'exec-path dir))

(setq TeX-view-program-list '(("Skim" "open -a Skim.app %o")))
(setq TeX-view-program-selection '((output-pdf "Skim")))

;;; Clear temp files
(defun p-clear-latex-temp-files ()
  (interactive)
  (let ((default-directory default-directory)
        (command "find . -maxdepth 1 \\( -name \"*.aux\" -o -name \"*.log\" -o -name \"*.gz\" -o -name \"*.out\" -o -name \"*.fls\" -o -name \"*.nav\" -o -name \"*.snm\" -o -name \"*.toc\" -o -name \"*.fdb_latexmk\" -o -name \"auto\" \\) -exec rm -rf {} +"))
    (shell-command command)
    (message "Files and directory deleted.")))

(defun p-run-latex ()
  (interactive)
  (TeX-save-document (TeX-master-file))
  (TeX-command "LaTeX" 'TeX-master-file))

;;; Keybindings
(with-eval-after-load 'evil
  (general-create-definer p-latex-leader-def
    :prefix ";"
    :states '(normal visual)
    :keymaps '(TeX-mode-map))
  (p-latex-leader-def
    "j"  '(:ignore t :which-key "latex")
    "jm" '(TeX-insert-macro :which-key "insert latex macro")
    "je" '(LaTeX-environment :which-key "insert latex environment")
    "jf" '(LaTeX-fill-buffer :which-key "format latex file")
    "jr" '(p-run-latex :which-key "run tex")
    "ja" '(TeX-command-run-all :which-key "run all")
    "jc" '(p-clear-latex-temp-files :which-key "clear temp files")
    "jv" '(TeX-view :which-key "view pdf")))

(provide 'init-latex)
;;;;; init-latex.el ends here
