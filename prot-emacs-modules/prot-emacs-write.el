;;; General configurations for prose/writing

;;; Install package
(straight-use-package 'denote)
(straight-use-package 'olivetti)

(setq outline-minor-mode-highlight nil) ; emacs28
(setq outline-minor-mode-cycle t)             ; emacs28
(setq outline-minor-mode-use-buttons nil) ; emacs29---bless you for the nil option!
(setq outline-minor-mode-use-margins nil) ; as above
(define-key global-map (kbd "<f10>") #'outline-minor-mode)

;;;; `docview' (simple PDF viewer)
;; The "mupdf" is a reference to the Arch Linux system packages
;; `mupdf', `mupdf-tools', `libmupdf'.
(setq doc-view-pdf->png-converter-function #'doc-view-pdf->png-converter-mupdf)
(setq doc-view-mupdf-use-svg (image-type-available-p 'svg)) ; Emacs 30
(setq doc-view-resolution 300) ; (doc-view-clear-cache)

;;;; `dictionary'
(setq dictionary-server "dict.org"
      dictionary-default-popup-strategy "lev" ; read doc string
      dictionary-create-buttons nil
      dictionary-use-single-buffer t)
(define-key global-map (kbd "C-c d") #'dictionary-search)

;;; Denote (simple note-taking and file-naming)
;; Read the manual: <https://protesilaos.com/emacs/denote>.
;; Remember to check the doc strings of those variables.
(setq denote-directory (expand-file-name "~/Dropbox/peng_notes/"))
(setq denote-known-keywords '("emacs" "economics"))
(setq denote-infer-keywords t)
(setq denote-sort-keywords t)
;; (setq denote-file-type 'text) ; Org is the default, set others here like I do
(setq denote-excluded-directories-regexp nil)
(setq denote-allow-multi-word-keywords nil)
(setq denote-date-format nil) ; read its doc string

;; By default, we fontify backlinks in their bespoke buffer.
(setq denote-link-fontify-backlinks t)

(denote-rename-buffer-mode 1)

;; Also see `denote-link-backlinks-display-buffer-action' which is a bit
;; advanced.

;; If you use Markdown or plain text files you want to buttonise
;; existing buttons upon visiting the file (Org renders links as
;; buttons right away).
(add-hook 'find-file-hook #'denote-link-buttonize-buffer)

;; Generic (great if you rename files Denote-style in lots of places):
(add-hook 'dired-mode-hook #'denote-dired-mode)
;;
;; OR if only want it in `denote-dired-directories':
;; (add-hook 'dired-mode-hook #'denote-dired-mode-in-directories)

;; Here is a custom, user-level command from one of the examples we
;; show in this manual.  We define it here and add it to a key binding
;; below.  The manual: <https://protesilaos.com/emacs/denote>.

;; NOTE 2023-06-16: I modified it to save journal files to my journal directory
(defun prot/denote-journal ()
  "Create an entry tagged 'journal' with the date as its title and save it to a pre-defined path.
If a journal for the current day exists, visit it. If multiple
entries exist, prompt with completion for a choice between them.
Else create a new file."
  (interactive)
  (let* ((today (format-time-string "%A %e %B %Y"))
         (string (denote-sluggify today))
         (files (denote-directory-files-matching-regexp string))
         (pre-defined-path "~/Dropbox/peng_notes/journal"))
    ;; Check if the pre-defined path exists, create it if it doesn't
    (unless (file-exists-p pre-defined-path)
      (make-directory pre-defined-path t))
    (cond
     ((> (length files) 1)
      (find-file (completing-read "Select file: " files nil :require-match)))
     (files
      (find-file (car files)))
     (t
      (denote
       today
       '("journal")
       nil
       pre-defined-path)))))

;; Denote DOES NOT define any key bindings.  This is for the user to
;; decide.  For example:
(prot-emacs-keybind global-map
  "C-c n j" prot/denote-journal
  "C-c n n" denote
  "C-c n N" denote-type
  "C-c n d" denote-date
  "C-c n z" denote-signature ; "zettelkasten" mnemonic
  "C-c n s" denote-subdirectory
  ;; If you intend to use Denote with a variety of file types, it is
  ;; easier to bind the link-related commands to the `global-map', as
  ;; shown here.  Otherwise follow the same pattern for `org-mode-map',
  ;; `markdown-mode-map', and/or `text-mode-map'.
  "C-c n i" denote-link ; "insert" mnemonic
  "C-c n I" denote-add-links
  "C-c n b" denote-backlinks
  "C-c n f f" denote-find-link
  "C-c n f b" denote-find-backlink
  ;; Note that `denote-rename-file' can work from any context, not
  ;; just Dired buffers.  That is why we bind it here to the
  ;; `global-map'.
  ;;
  ;; Also see `denote-rename-file-using-front-matter' further below.
  "C-c n r" denote-rename-file)

;; Key bindings specifically for Dired.
(prot-emacs-keybind dired-mode-map
  "C-c C-d C-i" denote-link-dired-marked-notes
  "C-c C-d C-r" denote-dired-rename-marked-files)

;; Also see `denote-rename-file' further above.
(define-key text-mode-map (kbd "C-c n R") #'denote-rename-file-using-front-matter)

(with-eval-after-load 'org-capture
  (setq denote-org-capture-specifiers "%l\n%i\n%?")
  (add-to-list 'org-capture-templates
               '("n" "New note (with denote.el)" plain
                 (file denote-last-path)
                 #'denote-org-capture
                 :no-save t
                 :immediate-finish nil
                 :kill-buffer t
                 :jump-to-captured t)))

(setq olivetti-body-width 0.7)
(setq olivetti-minimum-body-width 80)
(setq olivetti-recall-visual-line-mode-entry-state t)

(provide 'prot-emacs-write)
