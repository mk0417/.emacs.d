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
          (puthash name
                   (list (cons 'file file))
                   hash))))))

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

;; Emacs as the app launcher
;; System Preferences > Privacy & Security > Accessibility: allow Emacs to control computer
;; emacsclient -e "(emacs-run-launcher)"
;; Bind key for the above script using karabiner
(defun emacs-run-launcher ()
  (interactive)
  (with-selected-frame
      (make-frame
       '((name . "emacs-run-launcher")
         (minibuffer . only)
         ;; (border-width . 5)
         (auto-raise . t)
         (fullscreen . 0)
         (undecorated . t)
         (tool-bar-lines . 0)
         (menu-bar-lines . 0)
         (internal-border-width . 5)
         (width . 80)
         (height . 10)
         (background-color . "steel blue")))
    (unwind-protect
        (app-launcher-run-app)
      (delete-frame))))

;;; Keybindings
(with-eval-after-load 'evil
  (general-create-definer p-space-leader-def
    :prefix "SPC"
    :keymaps 'override
    :states '(normal visual))
  (p-space-leader-def
    "t"  '(:ignore t :which-key "toggle")
    "ta" 'app-launcher-run-app))

(provide 'init-launcher)
;;;;; init-launcher.el ends here
