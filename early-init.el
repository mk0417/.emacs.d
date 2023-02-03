;;;;; early-init.el --- Early init file -*- lexical-binding: t -*-

;;; Don't use package.el, use straight.el instead
(setq package-enable-at-startup nil)
(advice-add #'package--ensure-init-file :override #'ignore)

;;; Prefer loading newest compiled .el file
(setq load-prefer-newer t)

;;; Inhibit resizing frame
(setq frame-inhibit-implied-resize t)

;;; Make the initial buffer load faster by setting its mode to fundamental-mode
(setq initial-major-mode 'fundamental-mode)

(defvar p-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist p-file-name-handler-alist)))

;;; Increase the GC threshold for faster startup
(setq gc-cons-threshold most-positive-fixnum)
(setq gc-cons-percentage 0.6)

;;; Do not compact font cache
(setq inhibit-compacting-font-caches t)

;;; Setup straight as package manager
(setq straight-repository-branch "develop")
;; (setq straight-vc-git-default-clone-depth 1)
(setq straight-vc-git-default-clone-depth '(1 single-branch))
;; quickier init time
;; https://emacs.stackexchange.com/questions/71302/reducing-straight-el-bloat
(setq straight-check-for-modifications '(check-on-save find-when-checking))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;;; Native compilation settings
(when (featurep 'native-compile)
  ;; silence compiler warnings as they can be pretty disruptive
  (setq native-comp-async-report-warnings-errors nil)
  ;; Make native compilation happens asynchronously
  (setq native-comp-deferred-compilation t)
  (setq  compilation-scroll-output t))

;;; Modus themes
;; Load in early-init.el to avoid white screen flash
(setq modus-themes-mixed-fonts t)
(setq modus-themes-variable-pitch-ui t)
(setq modus-themes-bold-constructs t)
(setq modus-themes-common-palette-overrides
      '((fg-completion-match-0 fg-main)
        (fg-completion-match-1 fg-main)
        (fg-completion-match-2 fg-main)
        (bg-completion-match-0 bg-cyan-intense)
        (bg-completion-match-1 bg-green-intense)
        (bg-completion-match-2 bg-red-intense)
        (bg-completion bg-blue-nuanced)))
(setq modus-themes-completions
      '((selection . (extrabold))))

(setq modus-themes-org-blocks 'gray-background)
(setq modus-themes-headings
      '((0 . (variable-pitch light 1.6))
        (1 . (variable-pitch light 1.5))
        (2 . (variable-pitch regular 1.3))
        (3 . (variable-pitch regular 1.1))
        (t . (variable-pitch 1.0))))

(load-theme 'modus-vivendi t)

;;; No titlebar
(add-to-list 'default-frame-alist '(undecorated . t))

;;; Maximize frame at startup
(setq initial-frame-alist (quote ((fullscreen . maximized))))

;;; Premature redisplays can substantially affect startup times and produce ugly flashes of unstyled Emacs
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
(push '(horizontal-scroll-bars) default-frame-alist)
(setq inhibit-startup-buffer-menu t)

;;; Consistent color with theme
;; (when (featurep 'ns)
;;   (push '(ns-transparent-titlebar . t) default-frame-alist))

;;;;; early-init.el ends here
