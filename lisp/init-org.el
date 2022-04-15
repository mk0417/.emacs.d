;;;;; init-org.el --- Org-mode -*- lexical-binding: t -*-

;;; package
(straight-use-package 'org-modern)
(straight-use-package 'org-tree-slide)
(straight-use-package 'olivetti)
;; (straight-use-package '(org-appear :type git :host github :repo "awth13/org-appear"))

;;; org
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
      org-export-coding-system 'utf-8
      org-fast-tag-selection-single-key 'expert
      org-html-validation-link nil
      org-export-kill-product-buffer-when-displayed t
      org-catch-invisible-edits 'show-and-error
      org-special-ctrl-a/e t
      org-insert-heading-respect-content t
      org-pretty-entities t
      org-ellipsis "…"
      org-agenda-block-separator ?─
      org-agenda-time-grid
      '((daily today require-timed)
	(800 1000 1200 1400 1600 1800 2000)
	" ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
      org-agenda-current-time-string "⭠ now ─────────────────────────────────────────────────"
      org-tags-column 0)

(setq org-todo-keywords
      '((sequence "TODO(t)" "WORKING(w)" "|" "DONE(d)" "CANCEL(c)")))

;; prevent evil-mode from inserting unnecessary indentation
(add-hook 'org-mode-hook (lambda () (setq evil-auto-indent nil)))

;;; capturing
(setq org-capture-templates
      `(("t" "todo" entry (file ,(concat org-directory "/todo.org"))
	 "* TODO %^{Title}\nSCHEDULED: %^t\n")
	("n" "note" entry (file ,(concat org-directory "/note.org"))
	 "* %U\n")
	("i" "idea" entry (file ,(concat org-directory "/idea.org"))
	 "* %^{Title}\n%U\n")))

;;; re-align tags when window shape changes
(with-eval-after-load 'org-agenda
  (add-hook 'org-agenda-mode-hook
            (lambda () (add-hook 'window-configuration-change-hook 'org-agenda-align-tags nil t))))

;;; ignore heading with no_heading tag when exporting
;; https://emacs.stackexchange.com/questions/9492/is-it-possible-to-export-content-of-subtrees-without-their-headings/17677
(defun p-org-export-no-heading (backend)
  (org-map-entries (lambda () (delete-region (point-at-bol) (point-at-eol)))
                   "no_heading"))
(add-hook 'org-export-before-processing-hook 'p-org-export-no-heading)

(with-eval-after-load 'org
  ;; (require 'org-tempo)
  ;; (require 'ob)
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.8))
  (add-hook 'org-mode-hook 'p-text-mode-auto-fill)
  (add-to-list 'org-structure-template-alist '("b" . "src shell"))
  (add-to-list 'org-structure-template-alist '("p" . "src elisp")))

;;; org-modern-mode
(add-hook 'org-mode-hook #'org-modern-mode)
(add-hook 'org-agenda-finalize-hook #'org-modern-agenda)

;;; org appear
;; https://github.com/willbchang/ward-emacs/blob/master/config.org#org-appear
;; (setq org-appear-delay 0)
;; (setq org-appear-autolinks t)
;; (setq org-appear-autoentities t)
;; (setq org-appear-autokeywords t)
;; (setq org-appear-autosubmarkers t)

;; (add-hook 'evil-insert-state-entry-hook (lambda() (setq org-appear-delay 0)))
;; (add-hook 'evil-normal-state-entry-hook (lambda() (setq org-appear-delay 1)))
;; (add-hook 'org-mode-hook 'org-appear-mode)

;;; focus mode and presentation mode
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

;;; keybindings
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
	"d" '(org-set-tags-command :which-key "set tag"))
  (general-create-definer p-org-leader-def
	:prefix ";"
	:states '(normal visual)
	:keymaps 'org-mode-map)
  (p-org-leader-def
	"t"  '(:ignore t :which-key "table")
	"tk" '(org-table-move-row-up :which-key "move row up")
	"tj" '(org-table-move-row-down :which-key "move row down")
	"tl" '(org-table-move-column-right :which-key "move column right")
	"th" '(org-table-move-column-left :which-key "move column left")
	"tc" '(org-table-convert-region :which-key "convert region to table")))

(provide 'init-org)
;;;;; init-org.el ends here
