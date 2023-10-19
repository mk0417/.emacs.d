;;; Fontaine (font configurations)
;; Read the manual: <https://protesilaos.com/emacs/fontaine>
(prot-emacs-package fontaine
  (:install t)
  ;; (:delay 5)
  ;; This is defined in Emacs C code: it belongs to font settings.
  (setq x-underline-at-descent-line nil)

  ;; And this is for Emacs28.
  (setq-default text-scale-remap-header-line t)

  ;; This is the default value.  Just including it here for
  ;; completeness.
  (setq fontaine-latest-state-file (locate-user-emacs-file "fontaine-latest-state.eld"))

  ;; Iosevka Comfy is my highly customised build of Iosevka with
  ;; monospaced and duospaced (quasi-proportional) variants as well as
  ;; support or no support for ligatures:
  ;; <https://git.sr.ht/~protesilaos/iosevka-comfy>.
  ;;
  ;; | Family                          | Shapes | Spacing | Style      | Ligatures |
  ;; |---------------------------------+--------+---------+------------+-----------|
  ;; | Iosevka Comfy                   | Sans   | Compact | Monospaced | Yes       |
  ;; | Iosevka Comfy Fixed             | Sans   | Compact | Monospaced | No        |
  ;; | Iosevka Comfy Duo               | Sans   | Compact | Duospaced  | Yes       |
  ;; |---------------------------------+--------+---------+------------+-----------|
  ;; | Iosevka Comfy Motion            | Slab   | Compact | Monospaced | Yes       |
  ;; | Iosevka Comfy Motion Fixed      | Slab   | Compact | Monospaced | No        |
  ;; | Iosevka Comfy Motion Duo        | Slab   | Compact | Duospaced  | Yes       |
  ;; |---------------------------------+--------+---------+------------+-----------|
  ;; | Iosevka Comfy Wide              | Sans   | Wide    | Monospaced | Yes       |
  ;; | Iosevka Comfy Wide Fixed        | Sans   | Wide    | Monospaced | No        |
  ;; | Iosevka Comfy Wide Duo          | Sans   | Wide    | Duospaced  | Yes       |
  ;; |---------------------------------+--------+---------+------------+-----------|
  ;; | Iosevka Comfy Wide Motion       | Slab   | Wide    | Monospaced | Yes       |
  ;; | Iosevka Comfy Wide Motion Fixed | Slab   | Wide    | Monospaced | No        |
  ;; | Iosevka Comfy Wide Motion Duo   | Slab   | Wide    | Duospaced  | Yes       |
  (setq fontaine-presets
        '((small
           :default-height 80)
          (regular) ; like this it uses all the fallback values and is named `regular'
          (medium
           :default-weight semilight
           :default-height 115
           :bold-weight extrabold)
          (large
           :inherit medium
           :default-height 150)
          (presentation
           :inherit medium
           :default-weight light
           :default-height 180)
          (t
           ;; I keep all properties for didactic purposes, but most can be
           ;; omitted.  See the fontaine manual for the technicalities:
           ;; <https://protesilaos.com/emacs/fontaine>.
           :default-family "Iosevka Comfy"
           :default-weight regular
           :default-height 110
           :fixed-pitch-family nil ; falls back to :default-family
           :fixed-pitch-weight nil ; falls back to :default-weight
           :fixed-pitch-height 1.0
           :fixed-pitch-serif-family nil ; falls back to :default-family
           :fixed-pitch-serif-weight nil ; falls back to :default-weight
           :fixed-pitch-serif-height 1.0
           :variable-pitch-family "Iosevka Comfy Duo"
           :variable-pitch-weight nil
           :variable-pitch-height 1.0
           :bold-family nil ; use whatever the underlying face has
           :bold-weight bold
           :italic-family nil
           :italic-slant italic
           :line-spacing nil)))

  ;; Set last preset or fall back to desired style from `fontaine-presets'.
  (fontaine-set-preset (or (fontaine-restore-latest-preset) 'regular))

  ;; The other side of `fontaine-restore-latest-preset'.
  (add-hook 'kill-emacs-hook #'fontaine-store-latest-preset)

  ;; Persist font configurations while switching themes.  The
  ;; `enable-theme-functions' is from Emacs 29.
  (add-hook 'enable-theme-functions #'fontaine-apply-current-preset)

  (define-key global-map (kbd "C-c f") #'fontaine-set-preset)
  (define-key global-map (kbd "C-c F") #'fontaine-set-face-font))

;;; `variable-pitch-mode' setup

(prot-emacs-configure
  (:delay 5)
  (define-key ctl-x-x-map (kbd "v") #'variable-pitch-mode)

  ;; NOTE 2022-11-20: This may not cover every case, though it works
  ;; fine in my workflow.  I am still undecided by EWW.
  (defun prot/enable-variable-pitch ()
    (unless (or (derived-mode-p 'mhtml-mode 'nxml-mode 'yaml-mode)
                (member (buffer-name) '("*Colors*" "*Faces*" "*Quick Help*")))
      (variable-pitch-mode 1)))

  (defvar prot/enable-variable-pitch-in-hooks
    '(text-mode-hook
      help-mode-hook)
    "List of hook symbols to add `prot/enable-variable-pitch' to.")

  (dolist (hook prot/enable-variable-pitch-in-hooks)
    (add-hook hook #'prot/enable-variable-pitch))

  (with-eval-after-load 'cursory
    (defvar prot/cursory-variable-pitch--preferred-presets '(bar underscore)
      "List of preferred `cursory-presets' in order of preference.")

    (defun prot/cursory-variable-pitch--return-preset ()
      "Return preferred `cursory-presets' for `variable-pitch-mode'."
      (catch 'preset
        (dolist (element prot/cursory-variable-pitch--preferred-presets)
          (memq element cursory-presets)
          (throw 'preset element))))

    (defun prot/cursory-variable-pitch-set-preset ()
      "Set preferred Cursory preset or fall back to a reasonable default"
      (cursory-set-preset
       (if buffer-face-mode
           (prot/cursory-variable-pitch--return-preset)
         (or (cursory-restore-latest-preset) t))
       :local))

    (defvar prot/variable-pitch-mode-hook nil
      "Normal hook that runs after `variable-pitch-mode'.")

    (defun prot/variable-pitch-run-hook (&rest _)
      "Run `prot/variable-pitch-mode-hook'.
Use this as :after advice to the `variable-pitch-mode' function."
      (run-hooks 'prot/variable-pitch-mode-hook))

    (advice-add #'variable-pitch-mode :after #'prot/variable-pitch-run-hook)

    (add-hook 'prot/variable-pitch-mode-hook #'prot/cursory-variable-pitch-set-preset)))

(provide 'prot-emacs-font)
