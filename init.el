;;; init.el --- Personal configuration file -*- lexical-binding: t -*-

;; Copyright (c) 2019-2023  Protesilaos Stavrou <info@protesilaos.com>

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; URL: https://protesilaos.com/emacs/dotemacs
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1"))

;; This file is NOT part of GNU Emacs.

;; This file is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; See my dotfiles: <https://git.sr.ht/~protesilaos/dotfiles>

;;; Code:

(defgroup prot-emacs nil
  "User options for my dotemacs."
  :group 'file)

;; For those who use my dotfiles and need an easy way to write their
;; own extras on top of what I already load: search below for the files
;; prot-emacs-pre-custom.el and prot-emacs-post-custom.el

(defcustom prot-emacs-load-theme-family 'modus
  "Set of themes to load.
Valid values are the symbols `ef', `modus', and `standard', which
reference the `ef-themes', `modus-themes', and `standard-themes',
respectively.

A nil value does not load any of the above (use Emacs without a
theme).

This user option must be set in the `prot-emacs-pre-custom.el'
file.  If that file exists in the Emacs directory, it is loaded
before all other modules of my setup."
  :group 'prot-emacs
  :type '(choice :tag "Set of themes to load" :value modus
                 (const :tag "The `ef-themes' module" ef)
                 (const :tag "The `modus-themes' module" modus)
                 (const :tag "The `standard-themes' module" standard)
                 (const :tag "Do not load a theme module" nil)))

(defcustom prot-emacs-completion-ui 'vertico
  "Choose minibuffer completion UI between `mct' or `vertico'."
  :group 'prot-emacs
  :type '(choice :tag "Minibuffer user interface"
                 (const :tag "The `mct' module" mct)
                 (const :tag "The `vertico' module" vertico)))

(defcustom prot-emacs-load-evil nil
  "When non-nil, load Vim style key bindings."
  :group 'prot-emacs
  :type 'boolean)

(defcustom prot-emacs-load-which-key nil
  "When non-nil, display key binding hints after a short delay."
  :group 'prot-emacs
  :type 'boolean)

(defcustom prot-emacs-load-icons nil
  "When non-nil, enable iconography in various contexts.
This installs and uses the `nerd-icons' package and its variants."
  :group 'prot-emacs
  :type 'boolean)

(defcustom prot-emacs-omit-packages nil
  "List of package names to not load.
This instructs the relevant macros to not `require' the given
package.  In the case of `prot-emacs-elpa-package', the package
will not be installed if it is not already available on the
system.

This user option must be set in the `prot-emacs-pre-custom.el'
file.  If that file exists in the Emacs directory, it is loaded
before all other modules of my setup."
  :group 'prot-emacs
  :type '(repeat symbol))

;; Some basic settings
(setq frame-title-format '("%b"))
(setq ring-bell-function 'ignore)
(setq use-short-answers t)
(setq native-comp-async-report-warnings-errors 'silent) ; Emacs 28 with native compilation
(setq native-compile-prune-cache t) ; Emacs 29
(setq make-backup-files nil)
(setq backup-inhibited nil) ; Not sure if needed, given `make-backup-files'
(setq create-lockfiles nil)

;; Disable the damn thing by making it disposable.
(setq custom-file (make-temp-file "emacs-custom-"))

;; There is also the greek-postfix style.  This is for inserting
;; accents.  I am used to the standard use of a prefix.
(setq default-input-method "greek")

;; Enable these
(dolist (c '(narrow-to-region narrow-to-page upcase-region downcase-region))
  (put c 'disabled nil))

;; And disable these
(dolist (c '(eshell project-eshell overwrite-mode iconify-frame diary))
  (put c 'disabled t))

;; Always start with *scratch*
(setq initial-buffer-choice t)

;;;; Packages

(dolist (path '("prot-lisp" "prot-emacs-modules" "lisp"))
  (add-to-list 'load-path (locate-user-emacs-file path)))

(require 'package)

(with-eval-after-load 'project-vc
  (setq package-vc-register-as-project nil)) ; Emacs 30

(add-hook 'package-menu-mode-hook #'hl-line-mode)

;; Also read: <https://protesilaos.com/codelog/2022-05-13-emacs-elpa-devel/>
(setq package-archives
      '(("gnu-elpa" . "https://elpa.gnu.org/packages/")
        ("gnu-elpa-devel" . "https://elpa.gnu.org/devel/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa" . "https://melpa.org/packages/")))

;; Highest number gets priority (what is not mentioned has priority 0)
(setq package-archive-priorities
      '(("gnu-elpa" . 3)
        ("melpa" . 2)
        ("nongnu" . 1)))

;; I want to use my own packages from specific repositories.  All
;; others will rely on `package-archive-priorities'.  I do this to
;; test that the packaged version works as intended.
(defvar prot-emacs-my-packages
  '(agitate
    beframe
    cursory
    denote
    ef-themes
    fontaine
    lin
    mct
    modus-themes
    pulsar
    spacious-padding
    standard-themes
    substitute
    ;; altcaps
    ;; dired-preview
    ;; logos
    ;; notmuch-indicator
    ;; sxhkdrc-mode
    ;; tmr
    )
  "List of symbols representing the packages I develop/maintain.")

(setq package-pinned-packages
      `(,@(mapcar
           (lambda (package)
             (cons package "gnu-elpa-devel"))
           prot-emacs-my-packages)))

;; NOTE 2023-08-21: I build Emacs from source, so I always get the
;; latest version of built-in packages.  However, this is a good
;; solution to set to non-nil if I ever switch to a stable release.
(setq package-install-upgrade-built-in nil)

(setq custom-safe-themes t)

(defun prot-emacs-package-install (package &optional method)
  "Install PACKAGE with optional METHOD.

If METHOD is nil or the `builtin' symbol, PACKAGE is not
installed as it is considered part of Emacs.

If METHOD is a string, it must be a URL pointing to the version
controlled repository of PACKAGE.  Installation is done with
`package-vc-install'.

If METHOD is a quoted list, it must have a form accepted by
`package-vc-install' such as:

\\='(denote :url \"https://git.sr.ht/~protesilaos/denote\" :branch \"main\")

If METHOD is any other non-nil value, install PACKAGE using
`package-install'."
  (unless (or (eq method 'builtin) (null method))
    (unless (package-installed-p package)
      (when (or (stringp method) (listp method))
        (package-vc-install method))
      (unless package-archive-contents
        (package-refresh-contents))
      (package-install package))))

(defvar prot-emacs-loaded-packages nil)

(defmacro prot-emacs-package (package &rest body)
  "Require PACKAGE with BODY configurations.

PACKAGE is an unquoted symbol that is passed to `require'.  It
thus conforms with `featurep'.

BODY consists of ordinary Lisp expressions.  There are,
nevertheless, two unquoted plists that are treated specially:

1. (:install METHOD)
2. (:delay NUMBER)

These plists can be anywhere in BODY and are not part of its
final expansion.

The :install property is the argument passed to
`prot-emacs-package-install' and has the meaning of METHOD
described therein.

The :delay property makes the evaluation of PACKAGE with the
expanded BODY happen with `run-with-timer'.

Also see `prot-emacs-configure'."
  (declare (indent 1))
  (unless (memq package prot-emacs-omit-packages)
    (let (install delay)
      (dolist (element body)
        (when (plistp element)
          (pcase (car element)
            (:install (setq install (cdr element)
                            body (delq element body)))
            (:delay (setq delay (cadr element)
                          body (delq element body))))))
      (let ((common `(,(when install
                         `(prot-emacs-package-install ',package ,@install))
                      (require ',package)
                      (add-to-list 'prot-emacs-loaded-packages ',package)
                      ,@body
                      ;; (message "Prot Emacs loaded package: %s" ',package)
                      )))
        (cond
         ((featurep package)
          `(progn ,@body))
         (delay
          `(run-with-timer ,delay nil (lambda () ,@(delq nil common))))
         (t
          `(progn ,@(delq nil common))))))))

;; Samples of `prot-emacs-package' (expand them with `pp-macroexpand-last-sexp').

;; (prot-emacs-package denote
;;   (setq denote-directory "path/to/dir")
;;   (define-key global-map (kbd "C-c n") #'denote)
;;   (:install '(denote . (:url "https://git.sr.ht/~protesilaos/denote" :branch "main")))
;;   (:delay 5)
;;   (setq denote-file-type nil))
;;
;; (prot-emacs-package denote
;;   (setq denote-directory "path/to/dir")
;;   (define-key global-map (kbd "C-c n") #'denote)
;;   (:install "https://git.sr.ht/~protesilaos/denote")
;;   (:delay 5)
;;   (setq denote-file-type nil))
;;
;; (prot-emacs-package denote
;;   (:delay 5)
;;   (setq denote-directory "path/to/dir")
;;   (define-key global-map (kbd "C-c n") #'denote)
;;   (:install "https://git.sr.ht/~protesilaos/denote")
;;   (setq denote-file-type nil))
;;
;; (prot-emacs-package denote
;;   (:install "https://git.sr.ht/~protesilaos/denote")
;;   (:delay 5)
;;   (setq denote-directory "path/to/dir")
;;   (define-key global-map (kbd "C-c n") #'denote)
;;   (setq denote-file-type nil))
;;
;; (prot-emacs-package denote
;;   (:delay 5)
;;   (setq denote-directory "path/to/dir")
;;   (define-key global-map (kbd "C-c n") #'denote)
;;   (setq denote-file-type nil))
;;
;; (prot-emacs-package denote
;;   (setq denote-directory "path/to/dir")
;;   (define-key global-map (kbd "C-c n") #'denote)
;;   (setq denote-file-type nil))

(defmacro prot-emacs-configure (&rest body)
  "Evaluate BODY as a `progn'.
BODY consists of ordinary Lisp expressions.  The sole exception
is an unquoted plist of the form (:delay NUMBER) which evaluates
BODY with NUMBER seconds of `run-with-timer'.

Note that `prot-emacs-configure' does not try to autoload
anything.  Use it only for forms that evaluate regardless.

Also see `prot-emacs-package'."
  (declare (indent 0))
  (let (delay)
    (dolist (element body)
      (when (plistp element)
        (pcase (car element)
          (:delay (setq delay (cadr element)
                        body (delq element body))))))
    (if delay
        `(run-with-timer ,delay nil (lambda () ,@body))
      `(progn ,@body))))

(defmacro prot-emacs-keybind (keymap &rest definitions)
  "Expand key binding DEFINITIONS for the given KEYMAP.
DEFINITIONS is a sequence of string and command pairs."
  (declare (indent 1))
  (unless (zerop (% (length definitions) 2))
    (error "Uneven number of key+command pairs"))
  (let ((keys (seq-filter #'stringp definitions))
        ;; We do accept nil as a definition: it unsets the given key.
        (commands (seq-remove #'stringp definitions)))
    `(when-let (((keymapp ,keymap))
                (map ,keymap))
       ,@(mapcar
          (lambda (pair)
            (let* ((key (car pair))
                   (command (cdr pair)))
              (unless (and (null key) (null command))
                `(define-key map (kbd ,key) ,command))))
          (cl-mapcar #'cons keys commands)))))

;; Sample of `prot-emacs-keybind'

;; (prot-emacs-keybind global-map
;;   "C-z" nil
;;   "C-x b" #'switch-to-buffer
;;   "C-x C-c" nil
;;   "C-x k" #'kill-buffer)

(defmacro prot-emacs-abbrev (table &rest definitions)
  "Expand abbrev DEFINITIONS for the given TABLE.
DEFINITIONS is a sequence of string pairs mapping the
abbreviation to its expansion."
  (declare (indent 1))
  (unless (zerop (% (length definitions) 2))
    (error "Uneven number of key+command pairs"))
  `(when-let (((abbrev-table-p ,table))
              (table ,table))
     ,@(mapcar
        (lambda (pair)
          (when-let ((abbrev (car pair))
                     (expansion (cadr pair)))
            `(define-abbrev table ,abbrev ,expansion)))
        (seq-split definitions 2))))

(defun prot-emacs-return-loaded-packages ()
  "Return a list of all loaded packages.
Here packages include both `prot-emacs-loaded-packages' and
`package-activated-list'.  The latter only covers what is found
in the `package-archives', whereas the former is for anything
that is expanded with the `prot-emacs-package' macro."
  (delete-dups (append prot-emacs-loaded-packages package-activated-list)))

(defvar prot-emacs-package-form-regexp
  "^(\\(prot-emacs-package\\|prot-emacs-keybind\\|prot-emacs-abbrev\\|require\\) +'?\\([0-9a-zA-Z-]+\\)"
  "Regexp to add packages to `lisp-imenu-generic-expression'.")

(eval-after-load 'lisp-mode
  `(add-to-list 'lisp-imenu-generic-expression
                (list "Packages" ,prot-emacs-package-form-regexp 2)))

(defconst prot-emacs-font-lock-keywords
  '(("(\\(prot-emacs-package\\)\\_>[ \t']*\\(\\(?:\\sw\\|\\s_\\)+\\)?"
     (2 font-lock-constant-face nil t))
    ("(\\(prot-emacs-\\(keybind\\|abbrev\\)\\)\\_>[ \t']*\\(\\(\\sw\\|\\s_\\)+\\)?"
     (3 font-lock-variable-name-face nil t))))

(font-lock-add-keywords 'emacs-lisp-mode prot-emacs-font-lock-keywords)

;; For those who use my dotfiles and need an easy way to write their
;; own extras on top of what I already load.  The file must exist at
;; ~/.emacs.d/prot-emacs-pre-custom.el
;;
;; The purpose of this file is for the user to define their
;; preferences BEFORE loading any of the modules.  For example, the
;; user option `prot-emacs-omit-packages' lets the user specify which
;; packages not to load.  Search for all `defcustom' forms in this
;; file for other obvious customisations.
(load (locate-user-emacs-file "prot-emacs-pre-custom.el") :no-error :no-message)

;; NOTE 2023-10-08: some Prot config are modified based on my workflow and needs
(require 'prot-emacs-theme)
(require 'prot-emacs-essentials)
(require 'prot-emacs-font)
(require 'prot-emacs-modeline)
(require 'prot-emacs-completion-common)
(pcase prot-emacs-completion-ui
  ('mct (require 'prot-emacs-completion-mct))
  ('vertico (require 'prot-emacs-completion-vertico)))
(require 'prot-emacs-search)
(require 'prot-emacs-dired)
(require 'prot-emacs-window)
(require 'prot-emacs-git)               ; git, diff, and related
(require 'prot-emacs-org)
(require 'prot-emacs-langs)
(require 'prot-emacs-web)
(when prot-emacs-load-icons
  (require 'prot-emacs-icons))
(require 'prot-emacs-evil)
(require 'prot-emacs-which-key)

;; NOTE 2023-10-05: my config
(require 'init-default)
(require 'init-functions)
(require 'init-env)
(require 'init-evil)
(require 'init-git)
(require 'init-programming)
(require 'init-latex)
(require 'init-lsp-bridge)
;; (require 'init-keybindings)
(require 'init-keychord)
(require 'init-avy)

(setq safe-local-variable-values
      '((org-hide-leading-stars . t)
        (org-hide-macro-markers . t)))

;; For those who use my dotfiles and need an easy way to write their
;; own extras on top of what I already load.  The file must exist at
;; ~/.emacs.d/prot-emacs-post-custom.el
;;
;; The purpose of the "post customisations" is to make tweaks to what
;; I already define, such as to change the default theme.  See above
;; for the `prot-emacs-pre-custom.el' to make changes BEFORE loading
;; any of my other configurations.
(load (locate-user-emacs-file "prot-emacs-post-custom.el") :no-error :no-message)

;;; init.el ends here
