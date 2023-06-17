;;;;; init-launcher.el --- App launcher -*- lexical-binding: t -*-

;;; App-launcher for MacOS
;; https://github.com/SebastienWae/app-launcher/tree/main
(defcustom app-launcher-apps-directories
  '("/Applications" "/System/Applications")
  "Directories in which to search for applications."
  :type '(repeat directory))

(defcustom app-launcher--annotation-function #'app-launcher--annotation-function-default
  "Define the function that generates the annotation for each completion choice."
  :type 'function)

(defcustom app-launcher--action-function #'app-launcher--action-function-default
  "Define the function that is used to run the selected application."
  :type 'function)

(defvar app-launcher--cache nil
  "Cache of application data.")

(defvar app-launcher--cache-timestamp nil
  "Time when we last updated the cached application list.")

(defvar app-launcher--cached-files nil
  "List of cached application files.")

(defun app-launcher-list-applications ()
  "Return a list of all macOS applications."
  (let ((hash (make-hash-table :test #'equal))
        result)
    (dolist (dir app-launcher-apps-directories)
      (when (file-exists-p dir)
        (let ((dir (file-name-as-directory dir)))
          (dolist (file (directory-files dir nil "\\.app\\'"))
            (let ((id (file-name-base file)))
              (when (and (not (gethash id hash)) (file-directory-p (concat dir file)))
                (push (cons id (concat dir file)) result)
                (puthash id (concat dir file) hash)))))))
    result))

(defun app-launcher-parse-applications (applications)
  "Parse the application list to return usable information."
  (let ((hash (make-hash-table :test #'equal)))
    (dolist (entry applications hash)
      (let ((file (cdr entry)))
        (let ((name (file-name-nondirectory file)))
          (puthash name (list (cons 'file file)) hash))))))

(defun app-launcher-list-apps ()
  "Return list of all macOS applications."
  (let* ((new-applications (app-launcher-list-applications))
         (new-files (mapcar 'cdr new-applications)))
    (unless (and (equal new-files app-launcher--cached-files)
                 (null (cl-find-if
                        (lambda (file)
                          (time-less-p
                           app-launcher--cache-timestamp
                           (nth 5 (file-attributes file))))
                        new-files)))
      (setq app-launcher--cache (app-launcher-parse-applications new-applications))
      (setq app-launcher--cache-timestamp (current-time))
      (setq app-launcher--cached-files new-files)))
  app-launcher--cache)

(defun app-launcher--annotation-function-default (choice)
  "Default function to annotate the completion choices."
  nil)

(defun app-launcher--action-function-default (selected)
  "Default function used to run the selected application."
  (start-process "" nil "open" "-a" selected))

;;;###autoload
(defun app-launcher-run-app ()
  "Launch an application installed on your machine."
  (interactive)
  (let* ((candidates (app-launcher-list-apps))
         (result
          (completing-read
           "Run app: "
           (lambda (str pred flag)
             (if (eq flag 'metadata)
                 '(metadata
                   (annotation-function . (lambda (choice)
                                            (funcall
                                             app-launcher--annotation-function
                                             choice))))
               (complete-with-action flag candidates str pred)))
           nil t nil 'app-launcher nil nil)))
    (funcall app-launcher--action-function result)))

(defun dir-launcher-run-dir ()
  (interactive)
  (let ((dir (read-directory-name "Select directory: ")))
    (do-applescript
     (format "tell application \"Finder\"
                set theFolder to POSIX file \"%s\" as alias
                activate
                open theFolder
              end tell" (expand-file-name dir)))))

(defcustom selector-web-page-alist '()
  "Alist used by `selector-browse-bookmark' to associate
   web-links with their names. Needs to be an alist of the
   form (name . link) with both properties being
   strings. Initialised as an empty list as there is no point in
   predefining anything in it."
  :type 'alist)

(defun url-launcher-run-url ()
  (interactive)
  (browse-url
   (cdr (assoc (completing-read "Web-Page: " selector-web-page-alist) selector-web-page-alist))))

(defun emacs-launcher-font-bold-hook (beginning end _)
  (when (region-active-p)
    (let ((font-lock-unfontify-region-function #'ignore)
          (font-lock-fontify-region-function #'ignore))
      (font-lock-unfontify-region beginning end)
      (put-text-property beginning end 'face 'bold))))

;; Emacs as launcher
;; System Preferences > Privacy & Security > Accessibility: allow Emacs to control computer
;; https://github.com/Vidianos-Giannitsis/Dotfiles/tree/master/emacs/.emacs.d#emacs-launchers
(defun emacs-run-launcher (name width height function)
  (let* ((current-app (do-applescript "tell application \"System Events\" to get name of first application process whose frontmost is true"))
         (frame
          (make-frame
           `((name . ,name)
             (minibuffer . only)
             (width . ,width)
             (height . ,height)
             (auto-raise . t)
             (fullscreen . 0)
             ;; (undecorated . t)
             ;; (background-color . "steel blue")
             (tool-bar-lines . 0)
             (menu-bar-lines . 0)
             (internal-border-width . 3)
             (vertical-scroll-bars . nil)))))
    (select-frame-set-input-focus frame)
    (unwind-protect
        (progn
          (add-hook 'after-change-functions 'emacs-launcher-font-bold-hook nil t)
          (funcall function))
      (select-frame-set-input-focus (selected-frame))
      (do-applescript (format "tell application \"%s\" to activate" current-app))
      (run-at-time "0.1" nil #'delete-frame frame))))

(defun emacs-app-launcher ()
  (interactive)
  (emacs-run-launcher "emacs-app-launcher" 100 11 'app-launcher-run-app))

(defun emacs-dir-launcher ()
  (interactive)
  (emacs-run-launcher "emacs-dir-launcher" 100 11 'dir-launcher-run-dir))

(defun emacs-url-launcher ()
  (interactive)
  (emacs-run-launcher "emacs-web-page-launcher" 100 11 'url-launcher-run-url))

(setq selector-web-page-alist
      '(("Github" . "https://github.com/mk0417")
        ("Youtube" . "https://www.youtube.com/")
        ("Hex Color Codes" . "https://www.color-hex.com/")
        ("Detexify" . "https://detexify.kirelabs.org/classify.html")))

(provide 'init-launcher)
;;;;; init-launcher.el ends here
