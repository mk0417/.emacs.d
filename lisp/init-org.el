;;; init-org.el --- Org-mode -*- lexical-binding: t -*-

;; package
(straight-use-package 'org)
(straight-use-package 'org-superstar)
(straight-use-package 'org-tree-slide)
(straight-use-package 'olivetti)
(straight-use-package '(org-appear :type git :host github :repo "awth13/org-appear"))

;; org
(setq org-directory "~/Dropbox/org"
      org-agenda-files '("~/Dropbox/org/todo.org")
      org-log-done t
      org-startup-indented t
      org-src-fontify-natively t
      org-src-tab-acts-natively t
      org-edit-src-content-indentation 0
      org-export-use-babel nil
      org-confirm-babel-evaluate nil
      org-fontify-quote-and-verse-blocks t
      org-hide-emphasis-markers t
      org-catch-invisible-edits 'show
      org-export-coding-system 'utf-8
      org-fast-tag-selection-single-key 'expert
      org-html-validation-link nil
      org-export-kill-product-buffer-when-displayed t
      org-tags-column 80)

(setq org-todo-keywords
      '((sequence "TODO(t)" "WORKING(w)" "|" "DONE(d)" "CANCEL(c)")))

;; fix void function issue
(autoload 'org-element-keyword-parser "org")

(with-eval-after-load 'org
  (require 'org-tempo)
  (require 'ob)
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.8))
  (add-to-list 'org-structure-template-alist '("b" . "src shell"))
  (add-to-list 'org-structure-template-alist '("p" . "src elisp")))

;; org-superstar
(add-hook 'org-mode-hook 'org-superstar-mode)
(setq org-superstar-remove-leading-stars t)
(setq org-superstar-headline-bullets-list '("◉" "▷" "○"))
(setq org-superstar-item-bullet-alist
      '((?+ . ?•)
	(?* . ?➤)
	(?- . ?–)))

;; org appear
;; https://github.com/willbchang/ward-emacs/blob/master/config.org#org-appear
(setq org-appear-delay 0)
(setq org-appear-autolinks t)
(setq org-appear-autoentities t)
(setq org-appear-autokeywords t)
(setq org-appear-autosubmarkers t)

(add-hook 'evil-insert-state-entry-hook (lambda() (setq org-appear-delay 0)))
(add-hook 'evil-normal-state-entry-hook (lambda() (setq org-appear-delay 1)))
(add-hook 'org-mode-hook 'org-appear-mode)

;; capturing
(setq org-capture-templates
      `(("t" "todo" entry (file ,(concat org-directory "/todo.org"))
	 "* TODO %^{Title}\nSCHEDULED: %^t\n")
	("n" "note" entry (file ,(concat org-directory "/note.org"))
	 "* %U\n")
	("i" "idea" entry (file ,(concat org-directory "/idea.org"))
	 "* %^{Title}\n%U\n")))

;; re-align tags when window shape changes
(with-eval-after-load 'org-agenda
  (add-hook 'org-agenda-mode-hook
            (lambda () (add-hook 'window-configuration-change-hook 'org-agenda-align-tags nil t))))

;; ignore heading with no_heading tag when exporting
;; https://emacs.stackexchange.com/questions/9492/is-it-possible-to-export-content-of-subtrees-without-their-headings/17677
(defun p-org-export-no-heading (backend)
  (org-map-entries (lambda () (delete-region (point-at-bol) (point-at-eol)))
                   "no_heading"))
(add-hook 'org-export-before-processing-hook 'p-org-export-no-heading)

;; latex
;; https://github.com/GeneKao/orgmode-latex-templates
(setq org-latex-pdf-process
      '("latexmk -pdflatex='pdflatex -interaction nonstopmode' -pdf -bibtex -f %f"))

(unless (boundp 'org-latex-classes)
  (setq org-latex-classes nil))

(add-to-list 'org-latex-classes
             '("ethz"
               "\\documentclass[a4paper,11pt,titlepage]{memoir}
\\usepackage[utf8]{inputenc}
\\usepackage[T1]{fontenc}
\\usepackage{fixltx2e}
\\usepackage{graphicx}
\\usepackage{longtable}
\\usepackage{float}
\\usepackage{wrapfig}
\\usepackage{rotating}
\\usepackage[normalem]{ulem}
\\usepackage{amsmath}
\\usepackage{textcomp}
\\usepackage{marvosym}
\\usepackage{wasysym}
\\usepackage{amssymb}
\\usepackage{hyperref}
\\usepackage{mathpazo}
\\usepackage{color}
\\usepackage{enumerate}
\\definecolor{bg}{rgb}{0.95,0.95,0.95}
\\tolerance=1000
      [NO-DEFAULT-PACKAGES]
      [PACKAGES]
      [EXTRA]
\\linespread{1.1}
\\hypersetup{pdfborder=0 0 0}"
               ("\\chapter{%s}" . "\\chapter*{%s}")
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(add-to-list 'org-latex-classes
             '("article"
               "\\documentclass[11pt,a4paper]{article}
\\usepackage[utf8]{inputenc}
\\usepackage[T1]{fontenc}
\\usepackage{fixltx2e}
\\usepackage{graphicx}
\\usepackage{longtable}
\\usepackage{float}
\\usepackage{wrapfig}
\\usepackage{rotating}
\\usepackage[normalem]{ulem}
\\usepackage{amsmath}
\\usepackage{textcomp}
\\usepackage{marvosym}
\\usepackage{wasysym}
\\usepackage{amssymb}
\\usepackage{hyperref}
\\usepackage{mathpazo}
\\usepackage{color}
\\usepackage{enumerate}
\\definecolor{bg}{rgb}{0.95,0.95,0.95}
\\tolerance=1000
      [NO-DEFAULT-PACKAGES]
      [PACKAGES]
      [EXTRA]
\\linespread{1.1}
\\hypersetup{pdfborder=0 0 0}"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")))

(add-to-list 'org-latex-classes '("ebook"
                                  "\\documentclass[11pt, oneside]{memoir}
\\setstocksize{9in}{6in}
\\settrimmedsize{\\stockheight}{\\stockwidth}{*}
\\setlrmarginsandblock{2cm}{2cm}{*} % Left and right margin
\\setulmarginsandblock{2cm}{2cm}{*} % Upper and lower margin
\\checkandfixthelayout
% Much more laTeX code omitted
"
                                  ("\\chapter{%s}" . "\\chapter*{%s}")
                                  ("\\section{%s}" . "\\section*{%s}")
                                  ("\\subsection{%s}" . "\\subsection*{%s}")))

;; https://github.com/mclear-tools/dotemacs/blob/master/setup-config/setup-teaching.el
(setq org-latex-classes '(("beamer" "\\documentclass[presentation]{beamer}"
                           ("\\section{%s}" . "\\section*{%s}")
                           ("\\subsection{%s}" . "\\subsection*{%s}")
                           ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
                          ("article" "\\documentclass[11pt]{article}"
                           ("\\section{%s}" . "\\section*{%s}")
                           ("\\subsection{%s}" . "\\subsection*{%s}")
                           ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                           ("\\paragraph{%s}" . "\\paragraph*{%s}")
                           ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
                          ("report" "\\documentclass[11pt]{report}"
                           ("\\part{%s}" . "\\part*{%s}")
                           ("\\chapter{%s}" . "\\chapter*{%s}")
                           ("\\section{%s}" . "\\section*{%s}")
                           ("\\subsection{%s}" . "\\subsection*{%s}")
                           ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
                          ("book" "\\documentclass[11pt]{book}"
                           ("\\part{%s}" . "\\part*{%s}")
                           ("\\chapter{%s}" . "\\chapter*{%s}")
                           ("\\section{%s}" . "\\section*{%s}")
                           ("\\subsection{%s}" . "\\subsection*{%s}")
                           ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
                          ;; notes
                          ("org-notes" "\\documentclass[12pt]{article}
                           [NO-DEFAULT-PACKAGES]
                           [EXTRA]
                           \\input{/Users/roambot/.emacs.d/.local/custom-org-latex-classes/notes-setup-file.tex}"
                           ("\\section{%s}" . "\\section*{%s}")
                           ("\\subsection{%s}" . "\\subsection*{%s}")
                           ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                           ("\\paragraph{%s}" . "\\paragraph*{%s}")
                           ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
                          ;; beamer handout
                          ("beamer-handout" "\\documentclass[12pt]{article}
                           [NO-DEFAULT-PACKAGES]
                           [EXTRA]
                           \\input{/Users/roambot/.emacs.d/.local/custom-org-latex-classes/handout-setup-file.tex}"
                           ("\\section{%s}" . "\\section*{%s}")
                           ("\\subsection{%s}" . "\\subsection*{%s}")
                           ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                           ("\\paragraph{%s}" . "\\paragraph*{%s}")
                           ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
                          ;; beamer presentation
                          ("beamer-presentation" "\\documentclass[presentation]{beamer}
                           [NO-DEFAULT-PACKAGES]
                           [PACKAGES]
                           \\usepackage{pgfpages}
                           [EXTRA]
                           \\setbeameroption{show notes on second screen=right}
                           \\setbeamertemplate{note page}{\\pagecolor{yellow!5}\\insertnote}
                           \\input{/Users/roambot/.emacs.d/.local/custom-org-latex-classes/unl-beamer-preamble.tex}"
                           ("\\section{%s}" . "\\section*{%s}")
                           ("\\subsection{%s}" . "\\subsection*{%s}")
                           ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
                          ;; beamer slides only
                          ("beamer-slides-no-notes" "\\documentclass[handout]{beamer}
                           [NO-DEFAULT-PACKAGES]
                           [EXTRA]
                           \\setbeameroption{hidenotes}
                           \\input{/Users/roambot/.emacs.d/.local/custom-org-latex-classes/unl-beamer-preamble.tex}
                           [PACKAGES]"
                           ("\\section{%s}" . "\\section*{%s}")
                           ("\\subsection{%s}" . "\\subsection*{%s}")
                           ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))))

(defun cpm/org-export-beamer-presentation ()
  (interactive)
  (let ((org-export-exclude-tags '("handout")))
    (save-excursion
      (goto-char (point-min))
      (org-beamer-export-to-pdf nil t nil nil '(:latex-class "beamer-presentation")))))

;; https://kitchingroup.cheme.cmu.edu/blog/2013/12/08/Selectively-exporting-headlines-in-org-mode/
(defun cpm/org-export--file-beamer-presentation ()
  (interactive)
  (let ((org-export-exclude-tags '("handout")))
    (save-excursion
      (goto-char (point-min))
      (org-beamer-export-to-pdf t nil nil nil '(:latex-class "beamer-presentation")))))

;;;; Org export to slides w/o notes
(defun cpm/org-export-beamer-no-notes ()
  "Export org subtree slide content to useful custom style handout (PDF) form"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (org-open-file (org-beamer-export-to-pdf nil t nil nil '(:latex-class "beamer-slides-no-notes")))))

(defun cpm/org-export--file-beamer-no-notes ()
  "Export org file slide content to useful custom style handout (PDF) form"
  (interactive)
  (let ((org-export-exclude-tags '("slides")))
    (save-excursion
      (goto-char (point-min))
      (org-beamer-export-to-pdf t nil nil nil '(:latex-class "beamer-slides-no-notes")))))

;; Handouts
(defun cpm/org-export-beamer-handout ()
  "Export subtree content to PDF handout. Handout uses a distinctive quote style."
  (interactive)
  (let ((org-latex-default-quote-environment "quote-b")
        (org-export-exclude-tags '("slides")))
    (org-narrow-to-subtree)
    (save-excursion
      (goto-char (point-min))
      (org-latex-export-to-pdf t t nil nil '(:latex-class "beamer-handout")))
    (widen)))

(defun cpm/org-export--file-beamer-handout ()
  "Export file content to PDF handout. Handout uses a distinctive quote style."
  (interactive)
  (let ((org-latex-default-quote-environment "quote-b")
        (org-export-exclude-tags '("slides")))
    (save-excursion
      (goto-char (point-min))
      (org-latex-export-to-pdf t nil nil nil '(:latex-class "beamer-handout")))))

;; Notes
;; Org to PDF Notes
(defun cpm/org-export-pdf-notes ()
  "Export subtree of notes to PDF file. Note uses a distinctive quote style."
  (interactive)
  (let ((org-latex-default-quote-environment "quote-b"))
    (org-narrow-to-subtree)
    (save-excursion
      (goto-char (point-min))
      (org-latex-export-to-pdf t t nil nil '(:latex-class "org-notes")))
    (widen)))

(defun cpm/org-export--file-pdf-notes ()
  "Export file notes to PDF file. Note uses a distinctive quote style."
  (interactive)
  (let ((org-latex-default-quote-environment "quote-b"))
    (save-excursion
      (goto-char (point-min))
      (org-latex-export-to-pdf t nil nil nil '(:latex-class "org-notes")))))

(defun cpm/cleanup-pdf-notes()
  "Move notes to static directory & cleanup other files"
  (interactive)
  (async-shell-command-no-window "trash *.tex *.bbl && mv *.pdf static/materials/handouts"))

;; focus mode and presentation mode
(setq olivetti-body-width 0.7)
(setq olivetti-minimum-body-width 80)
(setq olivetti-recall-visual-line-mode-entry-state t)

(setq org-tree-slide-breadcrumbs nil)
(setq org-tree-slide-header nil)
(setq org-tree-slide-slide-in-effect nil)
(setq org-tree-slide-heading-emphasis nil)
(setq org-tree-slide-cursor-init t)
(setq org-tree-slide-modeline-display nil)
(setq org-tree-slide-skip-done nil)
(setq org-tree-slide-skip-comments t)
(setq org-tree-slide-fold-subtrees-skipped t)
(setq org-tree-slide-skip-outline-level 8)
(setq org-tree-slide-never-touch-face t)
(setq org-tree-slide-activate-message
      (format "Presentation %s" (propertize "ON" 'face 'success)))
(setq org-tree-slide-deactivate-message
      (format "Presentation %s" (propertize "OFF" 'face 'error)))

(with-eval-after-load 'olivetti
  (dolist (m '(org-indent-mode visual-line-mode olivetti-mode text-scale-mode))
    (diminish m)))

(autoload 'olivetti-set-width "olivetti")

(add-hook 'org-tree-slide-play-hook (lambda ()
                                      (text-scale-increase 4)
                                      (olivetti-set-width 0.1)
                                      (olivetti-mode 1)))
(add-hook 'org-tree-slide-stop-hook (lambda ()
                                      (text-scale-adjust 0)
                                      (olivetti-mode -1)))

;; keybindings
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(with-eval-after-load 'org
  (define-key org-mode-map (kbd "<tab>") 'org-cycle)
  (define-key org-mode-map (kbd "C-c l") 'org-store-link))

(with-eval-after-load 'evil
  (general-create-definer p-space-leader-def
    :prefix "SPC"
    :states 'normal)
  (p-space-leader-def
    "n"  '(:ignore t :which-key "note")
    "na" '(org-agenda :which-key "org agenda")
    "nc" '(org-capture :which-key "org capture"))
  (general-create-definer p-org-leader-def
    :prefix ";"
    :states 'normal
    :keymaps 'org-mode-map)
  (p-org-leader-def
   "." '(org-toggle-narrow-to-subtree :which-key "narrow to substree")
   "," '(org-toggle-latex-fragment :which-key "latex preview")
   "i" '(org-toggle-inline-images :which-key "toggle inline image")
   ";" '(org-tree-slide-mode :which-key "presentation mode")
   "n" '(org-tree-slide-move-next-tree :which-key "next slide")
   "p" '(org-tree-slide-move-previous-tree :which-key "previous slide")
   "h" '(org-tree-slide-display-header-toggle :which-key "toggle slide header")
   "t"  '(:ignore t :which-key "table")
   "tk" '(org-table-move-row-up :which-key "move row up")
   "tj" '(org-table-move-row-down :which-key "move row down")
   "tl" '(org-table-move-column-right :which-key "move column right")
   "th" '(org-table-move-column-left :which-key "move column left")
   "tc" '(org-table-convert-region :which-key "convert region to table")))

(provide 'init-org)
;;; init-org.el ends here
