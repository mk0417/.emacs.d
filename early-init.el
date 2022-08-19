;;;;; early-init.el -*- lexical-binding: t -*-

;;; Increase the GC threshold for faster startup
;; the default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold most-positive-fixnum)

;;; Don't use package.el, use straight.el instead
(setq package-enable-at-startup nil)

;;; Setup straight as package manager
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
;; straight configs
(setq straight-vc-git-default-clone-depth 1)

;;; Prefer loading newest compiled .el file
(setq load-prefer-newer noninteractive)

;;; Inhibit resizing frame
(setq frame-inhibit-implied-resize t)

;;; Native compilation settings
(when (featurep 'native-compile)
  ;; silence compiler warnings as they can be pretty disruptive
  (setq native-comp-async-report-warnings-errors nil)
  ;; Make native compilation happens asynchronously
  (setq native-comp-deferred-compilation t)
  (setq  compilation-scroll-output t)
  ;; set the right directory to store the native compilation cache
  ;; note the method for setting the eln-cache directory depends on the emacs version
  (when (fboundp 'startup-redirect-eln-cache)
    (if (version< emacs-version "29")
        (add-to-list 'native-comp-eln-load-path (convert-standard-filename (expand-file-name "var/eln-cache/" user-emacs-directory)))
      (startup-redirect-eln-cache (convert-standard-filename (expand-file-name "var/eln-cache/" user-emacs-directory)))))

  (add-to-list 'native-comp-eln-load-path (expand-file-name "eln-cache/" user-emacs-directory)))

;;; Premature redisplays can substantially affect startup times and produce ugly flashes of unstyled Emacs.
;; https://github.com/doomemacs/doomemacs/blob/master/early-init.el
(setq-default inhibit-redisplay t)
(add-hook 'window-setup-hook
          (lambda ()
            (setq-default inhibit-redisplay nil)
            (redisplay)))

;;; Minimal UI
(setq inhibit-startup-message t)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;;; No titlebar
(add-to-list 'default-frame-alist '(undecorated . t))

;;; Consistent color with theme
;; (when (featurep 'ns)
;;   (push '(ns-transparent-titlebar . t) default-frame-alist))

;;; Modus themes
;; load in early-init.el to avoid white screen flash
(setq modus-themes-prompts '(bold background))
(setq x-underline-at-descent-line t)
(setq modus-themes-hl-line '(intense))
(setq modus-themes-paren-match '(intense bold underline))
(setq modus-themes-region '(no-extend accented))
(setq modus-themes-completions '((matches . (background intense))))
(setq modus-themes-org-blocks 'gray-background)
(setq modus-themes-markup '(bold intense background))
(setq modus-themes-headings
      '((1 . (variable-pitch 1.3))
        (2 . (variable-pitch 1.2))
        (3 . (variable-pitch 1))
        (t . (variable-pitch 1))))

(load-theme 'modus-vivendi t)

;;; Maximize frame at startup
(setq initial-frame-alist (quote ((fullscreen . maximized))))

;;; Make the initial buffer load faster by setting its mode to fundamental-mode
(setq initial-major-mode 'fundamental-mode)

;;;;; early-init.el ends here
