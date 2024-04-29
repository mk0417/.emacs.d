;;;;; prot-emacs-web.el --- Web -*- lexical-binding: t -*-

;;;; `browse-url'
(use-package browse-url
  :ensure nil
  :defer t
  :config
  (setq browse-url-browser-function 'eww-browse-url)
  (setq browse-url-secondary-browser-function 'browse-url-default-browser))

;;;; `shr' (Simple HTML Renderer)
(use-package shr
  :ensure nil
  :defer t
  :config
  (setq shr-use-colors nil)             ; t is bad for accessibility
  (setq shr-use-fonts nil)              ; t is not for me
  (setq shr-max-image-proportion 0.6)
  (setq shr-image-animate nil)          ; No GIFs, thank you!
  (setq shr-width fill-column)          ; check `prot-eww-readable'
  (setq shr-max-width fill-column)
  (setq shr-discard-aria-hidden t)
  (setq shr-cookie-policy nil))

;;;; `url-cookie'
(use-package url-cookie
  :ensure nil
  :defer t
  :config
  (setq url-cookie-untrusted-urls '(".*")))

;;;; `eww' (Emacs Web Wowser)
(use-package eww
  :ensure nil
  :commands (eww)
  :bind
  ( :map eww-link-keymap
    ("v" . nil) ; stop overriding `eww-view-source'
    :map eww-mode-map
    ("L" . eww-list-bookmarks)
    :map dired-mode-map
    ("E" . eww-open-file) ; to render local HTML files
    :map eww-buffers-mode-map
    ("d" . eww-bookmark-kill)   ; it actually deletes
    :map eww-bookmark-mode-map
    ("d" . eww-bookmark-kill)) ; same
  :config
  (setq eww-restore-desktop t)
  (setq eww-desktop-remove-duplicates t)
  (setq eww-header-line-format nil)
  (setq eww-search-prefix "https://duckduckgo.com/html/?q=")
  (setq eww-download-directory (expand-file-name "~/Documents/eww-downloads"))
  (setq eww-suggest-uris
        '(eww-links-at-point
          thing-at-point-url-at-point))
  (setq eww-bookmarks-directory (locate-user-emacs-file "eww-bookmarks/"))
  (setq eww-history-limit 150)
  (setq eww-use-external-browser-for-content-type
        "\\`\\(video/\\|audio\\)") ; On GNU/Linux check your mimeapps.list
  (setq eww-browse-url-new-window-is-tab nil)
  (setq eww-form-checkbox-selected-symbol "[X]")
  (setq eww-form-checkbox-symbol "[ ]")
  ;; NOTE `eww-retrieve-command' is for Emacs28.  I tried the following
  ;; two values.  The first would not render properly some plain text
  ;; pages, such as by messing up the spacing between paragraphs.  The
  ;; second is more reliable but feels slower.  So I just use the
  ;; default (nil), though I find wget to be a bit faster.  In that case
  ;; one could live with the occasional errors by using `eww-download'
  ;; on the offending page, but I prefer consistency.
  ;;
  ;; '("wget" "--quiet" "--output-document=-")
  ;; '("chromium" "--headless" "--dump-dom")
  (setq eww-retrieve-command nil))

;;;; `prot-eww' extras
(use-package prot-eww
  :ensure nil
  :after eww
  :config
  (setq prot-eww-save-history-file
        (locate-user-emacs-file "prot-eww-visited-history"))
  (setq prot-eww-save-visited-history t)
  (setq prot-eww-bookmark-link nil)

  (add-hook 'prot-eww-history-mode-hook #'hl-line-mode)

  (define-prefix-command 'prot-eww-map)
  (define-key global-map (kbd "C-c w") 'prot-eww-map)
  (prot-emacs-keybind prot-eww-map
    "b" #'prot-eww-visit-bookmark
    "e" #'prot-eww-browse-dwim
    "s" #'prot-eww-search-engine)
  (prot-emacs-keybind eww-mode-map
    "B" #'prot-eww-bookmark-page
    "D" #'prot-eww-download-html
    "H" #'prot-eww-list-history
    "b" #'prot-eww-visit-bookmark
    "e" #'prot-eww-browse-dwim
    "o" #'prot-eww-open-in-other-window
    "E" #'prot-eww-visit-url-on-page
    "J" #'prot-eww-jump-to-url-on-page
    "R" #'prot-eww-readable
    "Q" #'prot-eww-quit))

(provide 'prot-emacs-web)
