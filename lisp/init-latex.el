;;; init-latex.el --- Latex -*- lexical-binding: t -*-

;; https://github.com/karthink/.emacs.d/blob/master/lisp/setup-latex.el
(use-package latex
  :after tex
  :ensure auctex
  :hook ((LaTeX-mode . electric-pair-mode)
         (LaTeX-mode . my/latex-with-outline))
  :mode ("\\.tex\\'" . latex-mode)
  :defines (TeX-auto-save
            TeX-parse-self
            TeX-electric-escape
            TeX-PDF-mode
            TeX-source-correlate-method
            TeX-newline-function
            TeX-view-program-list
            TeX-view-program-selection
            TeX-mode-map)
  :bind
  (:map LaTeX-mode-map
        ("M-RET" . LaTeX-insert-item)
        :map TeX-source-correlate-map
        ([C-down-mouse-1] . TeX-view-mouse))
  :config
  (defun my/latex-with-outline ()
    (add-to-list 'minor-mode-overriding-map-alist
                 `(outline-minor-mode . ,outline-minor-mode-map))
    (outline-minor-mode 1))

  (use-package embrace
    :bind (:map TeX-mode-map
                ("M-s a" . embrace-add)
                ("M-s c" . embrace-change)
                ("M-s d" . embrace-delete)))

  (defun TeX-insert-smallmatrix () (interactive)
         (insert "[\\begin{smallmatrix}  \\end{smallmatrix}]")
         (backward-char 19))

  (defun TeX-insert-bmatrix () (interactive)
         (insert "\\begin{bmatrix}  \\end{bmatrix}")
         (backward-char 14))

  (dolist (dir '("/Applications/Skim.app/Contents/SharedSupport")) (add-to-list 'exec-path dir))
  (setq TeX-view-program-list '(("Skim" "open -a Skim.app %o")))
  (setq TeX-view-program-selection '((output-pdf "Skim")))

  (setq TeX-auto-save t
        TeX-parse-self t
        TeX-electric-escape nil
        ;; Setting this to t messes up previews
        ;; If previews still don't show disable the hyperref package
        TeX-PDF-mode nil
        TeX-error-overview-open-after-TeX-run nil)
  (setq LaTeX-command "latex")
  (setq-default TeX-source-correlate-mode t)
  (setq TeX-source-correlate-method 'synctex)
  (setq-default TeX-source-correlate-start-server t)
  (setq TeX-newline-function 'reindent-then-newline-and-indent)
  ;; (setq TeX-PDF-from-DVI "Dvips") ; Set to nil to call pdflatex directly
  ;; Update PDF buffers after successful LaTeX runs
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer))

;; Some structural navigation tweaks for Latex mode.
(use-package latex
  :defer
  :bind (:map LaTeX-mode-map
              ("C-M-u" . LaTeX-backward-up-list)
              ("C-M-e" . LaTeX-forward-environment)
              ("C-M-a" . LaTeX-backward-environment))
  :config
  ;; Monkey patching: Stop this from marking to the end of the line at the end
  ;; of the env.
  (defun LaTeX-mark-environment (&optional count)
    "Set mark to end of current environment and point to the matching begin.
If prefix argument COUNT is given, mark the respective number of
enclosing environments.  The command will not work properly if
there are unbalanced begin-end pairs in comments and verbatim
environments."
    (interactive "p")
    (setq count (if count (abs count) 1))
    (let ((cur (point)) beg end)
      ;; Only change point and mark after beginning and end were found.
      ;; Point should not end up in the middle of nowhere if the search fails.
      (save-excursion
        (dotimes (_ count) (LaTeX-find-matching-end))
        (setq end (point))
        (goto-char cur)
        (dotimes (_ count) (LaTeX-find-matching-begin))
        (setq beg (point)))
      (push-mark end)
      (goto-char beg)
      (TeX-activate-region)))
  (defun LaTeX-backward-up-list (&optional arg)
    (interactive "p")
    (let ((total (or arg 1)))
      (condition-case at-top-level
          (dotimes (_ arg)
            (up-list -1 t t)
            (setq total (1- total)))
        ('user-error
         (dotimes (_ (max 0 total))
           (LaTeX-find-matching-begin))))))

  (defun LaTeX-forward-environment (&optional N do-push-mark)
    "Move to the \\end of the next \\begin,
or to the \\end of the current environment
(whichever comes first) N times.

Never goes into deeper environments.

DO-PUSH-MARK defaults to t when interactive,
but mark is only pushed if region isn't active."
    (interactive "p")
    (unless (region-active-p)
      (when do-push-mark (push-mark)))
    (let ((start (point))
          (count (abs N))
          (direction (if (< N 0) -1 1)))
      (while (and (> count 0)
                  (re-search-forward "\\\\\\(begin\\|end\\)\\b"
                                     nil t direction))
        (cl-decf count)
        (if (or (and (> direction 0) (looking-back "begin" (- (point) 7)))
                (looking-at "\\\\end"))
            (unless (funcall (if (> direction 0)
                                 #'LaTeX-find-matching-end
                               #'LaTeX-find-matching-begin))
              (error "Unmatched \\begin?"))
          (when (looking-at "\\[") (forward-sexp 1))
          (when (looking-at "{") (forward-sexp 1))))))

  (defun LaTeX-backward-environment (&optional N do-push-mark)
    "Move to the \\begin of the next \\end,
or to the \\begin of the current environment
(whichever comes first) N times.

Never goes into deeper environments.

DO-PUSH-MARK defaults to t when interactive,
but mark is only pushed if region isn't active."
    (interactive "p")
    (LaTeX-forward-environment (- N) do-push-mark)))

(use-package latex
  :defer
  :if (version<= "28.0" emacs-version)
  :config
  (defvar my/TeX-error-map
    (let ((map (make-sparse-keymap)))
      (define-key map "n" 'TeX-next-error)
      (define-key map "p" 'TeX-previous-error)
      map))
  (put 'TeX-next-error 'repeat-map 'my/TeX-error-map)
  (put 'TeX-previous-error 'repeat-map 'my/TeX-error-map))

(use-package preview
  :after latex
  :hook (LaTeX-mode . my/preview-scale-larger)
  :config
  (define-key LaTeX-mode-map (kbd "C-c C-x") preview-map)
  (defun my/preview-scale-larger ()
    "Increase the size of `preview-latex' images"
    (setq preview-scale-function
          (lambda nil (* 1.25 (funcall (preview-scale-from-face)))))))

;; (setq-default TeX-master nil)
(use-package cdlatex
  :after latex
  :ensure t
  ;; :commands turn-on-cdlatex
  :hook ((LaTeX-mode . cdlatex-mode)
         (LaTeX-mode . cdlatex-electricindex-mode))
  :bind (:map cdlatex-mode-map
              ("[" . nil) ("(" . nil) ("{" . nil)
              ("<tab>" . cdlatex-tab))
  :defines (cdlatex-math-symbol-prefix cdlatex-command-alist)
  :config
  ;; (setq cdlatex-math-symbol-prefix ?\;)
  (define-key cdlatex-mode-map
              (cdlatex-get-kbd-vector cdlatex-math-symbol-prefix)
              #'cdlatex-math-symbol)
  (dolist (cmd '(("vc" "Insert \\vect{}" "\\vect{?}"
                  cdlatex-position-cursor nil nil t)
                 ("tfr" "Insert \\tfrac{}{}" "\\tfrac{?}{}"
                  cdlatex-position-cursor nil nil t)
                 ("sfr" "Insert \\sfrac{}{}" "\\sfrac{?}{}"
                  cdlatex-position-cursor nil nil t)
                 ("abs" "Insert \\abs{}" "\\abs{?}"
                  cdlatex-position-cursor nil nil t)
                 ("equ*" "Insert equation* env"
                  "\\begin{equation*}\n?\n\\end{equation*}"
                  cdlatex-position-cursor nil t nil)
                 ("sn*" "Insert section* env"
                  "\\section*{?}"
                  cdlatex-position-cursor nil t nil)
                 ("ss*" "Insert subsection* env"
                  "\\subsection*{?}"
                  cdlatex-position-cursor nil t nil)
                 ("sss*" "Insert subsubsection* env"
                  "\\subsubsection*{?}"
                  cdlatex-position-cursor nil t nil)))
    (push cmd cdlatex-command-alist))

  (setq cdlatex-env-alist
        '(("align" "\\begin{align}
?
\\end{align}" "\\\\AUTOLABEL
?")
          ("equation" "\\begin{equation}
?
\\end{equation}" nil)))

  (setq cdlatex-math-symbol-alist '((?F ("\\Phi"))
                                    (?o ("\\omega" "\\mho" "\\mathcal{O}"))
                                    (?. ("\\cdot" "\\circ"))
                                    (?6 ("\\partial"))
                                    (?v ("\\vee" "\\forall"))
                                    (?^ ("\\uparrow" "\\Updownarrow" "\\updownarrow"))))
  (setq cdlatex-math-modify-alist '((?k "\\mathfrak" "\\textfrak" t nil nil)
                                    (?b "\\mathbf" "\\textbf" t nil nil)
                                    (?B "\\mathbb" "\\textbf" t nil nil)
                                    (?t "\\text" nil t nil nil)))
  (setq cdlatex-paired-parens "$[{(")
  (cdlatex-reset-mode))

(provide 'init-latex)
