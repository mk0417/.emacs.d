;;;; `browse-url'
(use-package browse-url
  :ensure nil
  :commands
  ( browse-url browse-url-at-point browse-url-at-mouse browse-url-of-file
    browse-url-firefox browse-url-chromium browse-url-epiphany)
  :config
  (setq browse-url-browser-function 'eww-browse-url)
  (setq browse-url-secondary-browser-function 'browse-url-default-browser))

;;;; `goto-addr'
(use-package goto-addr
  :ensure nil
  :commands (goto-address-mode goto-address-prog-mode)
  :hook
  ((text-mode . goto-address-mode)
   (prog-mode . goto-address-prog-mode))
  :config
  (setq goto-address-url-face 'link)
  (setq goto-address-url-mouse-face 'highlight)
  (setq goto-address-mail-face nil)
  (setq goto-address-mail-mouse-face 'highlight))

;;;; `shr' (Simple HTML Renderer)
(use-package shr
  :ensure nil
  :defer t
  :config
  (setq shr-use-colors nil)             ; t is bad for accessibility
  (setq shr-use-fonts nil)              ; t is superfluous, given `variable-pitch-mode'
  (setq shr-max-image-proportion 0.6)
  (setq shr-image-animate nil)          ; No GIFs, thank you!
  (setq shr-width fill-column)
  (setq shr-max-width fill-column)
  (setq shr-discard-aria-hidden t)
  (setq shr-fill-text nil)              ; Emacs 31
  (setq shr-cookie-policy nil))

;;;; `url-cookie'
(use-package url-cookie
  :ensure nil
  :commands (url-cookie-list)
  :config
  (setq url-cookie-untrusted-urls '(".*")))

;;;; `eww' (Emacs Web Wowser)
(use-package eww
  :ensure nil
  :after prot-simple
  :commands (eww)
  :bind
  ( :map eww-mode-map
    ("S" . nil) ; unmap `eww-list-buffers'
    ("b" . prot-simple-buffers-major-mode) ; a general version to show buffer of current mode
    ("m" . bookmark-set)
    :map eww-link-keymap
    ("v" . nil) ; stop overriding `eww-view-source'
    :map dired-mode-map
    ("E" . eww-open-file)) ; to render local HTML files
  :config
  (setq eww-auto-rename-buffer 'title)
  (setq eww-header-line-format nil)
  (setq eww-bookmarks-directory (locate-user-emacs-file "eww-bookmarks/"))
  (setq eww-history-limit 150)
  (setq eww-use-external-browser-for-content-type
        "\\`\\(video/\\|audio\\)") ; On GNU/Linux check your mimeapps.list
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
  (setq eww-retrieve-command nil)

  ;; NOTE 2025-02-15: Emacs has a robust framework for writing
  ;; bookmarks, which `eww' supports.  Though `eww' also defines its
  ;; own parallel bookmark data, which I do not want to use.  So here
  ;; I disable all the relevant commands.
  (dolist (command '( eww-list-bookmarks eww-add-bookmark eww-bookmark-mode
                      eww-list-buffers eww-toggle-fonts eww-toggle-colors
                      eww-switch-to-buffer))
    (put command 'disabled t)))

;;;; `prot-eww' extras
(use-package prot-eww
  :ensure nil
  :after eww
  :bind
  ( :map eww-mode-map
    ("F" . prot-eww-find-feed)
    ("o" . prot-eww-open-in-other-window)
    ("j" . prot-eww-jump-to-url-on-page)
    ("J" . prot-eww-visit-url-on-page)))

(provide 'prot-emacs-web)
