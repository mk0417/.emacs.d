;;; The Standard themes

;; The themes are customisable.  Read the manual:
;; <https://protesilaos.com/emacs/standard-themes>.

(prot-emacs-package standard-themes
  (:install t)
  (:delay 1)
  (setq standard-themes-bold-constructs t
        standard-themes-italic-constructs t
        standard-themes-mixed-fonts t
        standard-themes-variable-pitch-ui t
        standard-themes-mode-line-accented nil

        ;; Accepts a symbol value
        standard-themes-fringes 'subtle

        ;; The following accept lists of properties
        standard-themes-links nil
        standard-themes-region nil
        standard-themes-prompts nil

        ;; more complex alist to set weight, height, and optional
        ;; `variable-pitch' per heading level (t is for any level not
        ;; specified)
        standard-themes-headings
        '((0 . (variable-pitch light 1.9))
          (1 . (variable-pitch light 1.8))
          (2 . (variable-pitch light 1.7))
          (3 . (variable-pitch semilight 1.6))
          (4 . (variable-pitch semilight 1.5))
          (5 . (variable-pitch 1.4))
          (6 . (variable-pitch 1.3))
          (7 . (variable-pitch 1.2))
          (agenda-date . (1.3))
          (agenda-structure . (variable-pitch light 1.8))
          (t . (variable-pitch 1.1))))

  ;; Load a theme that is consistent with my session's theme.  Those
  ;; functions are defined in my init.el.
  (standard-themes-load-dark)

  (define-key global-map (kbd "<f5>") #'standard-themes-toggle))

;; NOTE: For testing purposes
(prot-emacs-comment
 (progn
   (mapc #'disable-theme custom-enabled-themes)

   (add-to-list 'load-path "/home/prot/Git/Projects/standard-themes/")

   (require 'standard-themes)
   (load-theme 'standard-dark t t)
   (load-theme 'standard-light t t)

   (setq standard-themes-bold-constructs t
         standard-themes-italic-constructs t
         standard-themes-disable-other-themes t
         standard-themes-mixed-fonts t
         standard-themes-variable-pitch-ui t
         standard-themes-mode-line-accented nil

         ;; Accepts a symbol value
         standard-themes-fringes 'subtle

         ;; The following accept lists of properties
         standard-themes-links nil
         standard-themes-region nil
         standard-themes-prompts nil

         ;; more complex alist to set weight, height, and optional
         ;; `variable-pitch' per heading level (t is for any level not
         ;; specified)
         standard-themes-headings
         '((0 . (variable-pitch light 1.9))
           (1 . (variable-pitch light 1.8))
           (2 . (variable-pitch light 1.7))
           (3 . (variable-pitch semilight 1.6))
           (4 . (variable-pitch semilight 1.5))
           (5 . (variable-pitch 1.4))
           (6 . (variable-pitch 1.3))
           (7 . (variable-pitch 1.2))
           (agenda-date . (1.3))
           (agenda-structure . (variable-pitch light 1.8))
           (t . (variable-pitch 1.1))))

   (mapcar (lambda (theme)
             (add-to-list
              'custom-theme-load-path
              (concat "/home/prot/Git/Projects/standard-themes/" (symbol-name theme) "-theme.el")))
           (standard-themes--list-enabled-themes))

   (if (prot-emacs-theme-environment-dark-p)
       (load-theme 'standard-dark :no-confirm)
     (load-theme 'standard-light :no-confirm))

   (define-key global-map (kbd "<f5>") #'standard-themes-toggle)))

(provide 'prot-emacs-standard-themes)
