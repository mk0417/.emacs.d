(prot-emacs-configure
  (:delay 5)
;;; Calendar
  (setq calendar-mark-diary-entries-flag nil)
  (setq calendar-mark-holidays-flag t)
  (setq calendar-mode-line-format nil)
  (setq calendar-time-display-form
        '( 24-hours ":" minutes
           (when time-zone (format "(%s)" time-zone))))
  (setq calendar-week-start-day 1)      ; Monday
  (setq calendar-date-style 'iso)
  (setq calendar-time-zone-style 'numeric) ; Emacs 28.1

  ;; (require 'solar)
  ;; (setq calendar-latitude 35.17         ; Not my actual coordinates
  ;;       calendar-longitude 33.36)

  ;; (require 'cal-dst)
  (setq calendar-standard-time-zone-name "+0200")
  (setq calendar-daylight-time-zone-name "+0300")

;;; Appt (appointment reminders which also integrate with Org agenda)
  (setq appt-display-diary nil
        appt-display-format nil
        appt-display-mode-line t
        appt-display-interval 3
        appt-audible nil ; TODO 2023-01-25: t does nothing because I disable `ring-bell-function'?
        appt-warning-time-regexp "appt \\([0-9]+\\)" ; This is for the diary
        appt-message-warning-time 6)

  (with-eval-after-load 'org-agenda
    (appt-activate 1))

;;; Org-mode (personal information manager)
  ;; NOTE 2023-05-20: Must be evaluated before Org is loaded,
  ;; otherwise we have to use the Custom UI.  No thanks!
  (setq org-export-backends '(html texinfo md))

  (setq org-directory (expand-file-name "~/Dropbox/org/"))
  (setq org-imenu-depth 7)
;;;; general settings
  (setq org-ellipsis "⮧")
  (setq org-adapt-indentation nil)      ; No, non, nein, όχι!
  (setq org-special-ctrl-a/e nil)
  (setq org-special-ctrl-k nil)
  (setq org-M-RET-may-split-line '((default . nil)))
  (setq org-hide-emphasis-markers nil)
  (setq org-hide-macro-markers nil)
  (setq org-hide-leading-stars nil)
  (setq org-cycle-separator-lines 0)
  (setq org-structure-template-alist
        '(("s" . "src")
          ("e" . "src emacs-lisp")
          ("E" . "src emacs-lisp :results value code :lexical t")
          ("x" . "example")
          ("q" . "quote")))
  (setq org-catch-invisible-edits 'show)
  (setq org-return-follows-link nil)
  (setq org-loop-over-headlines-in-active-region 'start-level)
  (setq org-modules '(ol-info ol-eww))
  (setq org-use-sub-superscripts '{})
  (setq org-insert-heading-respect-content t)

;;;; refile, todo
  (setq org-refile-targets
        '((org-agenda-files . (:maxlevel . 2))
          (nil . (:maxlevel . 2))))
  (setq org-refile-use-outline-path t)
  (setq org-refile-allow-creating-parent-nodes 'confirm)
  (setq org-refile-use-cache t)
  (setq org-reverse-note-order nil)
  ;; ;; NOTE 2023-04-07: Leaving this here for demo purposes.
  ;; (setq org-todo-keywords
  ;;       '((sequence "TODO(t)" "MAYBE(m)" "WAIT(w@/!)" "|" "CANCEL(c@)" "DONE(d!)")
  ;;         (sequence "COACH(k)" "|" "COACHED(K!)")))
  (setq org-todo-keywords
        '((sequence "TODO(t)" "|" "CANCEL(c@)" "DONE(d!)")
          (sequence "COACH(k)" "|" "COACHED(K!)")))

  (defface prot/org-bold-done
    '((t :inherit (bold org-done)))
    "Face for bold DONE-type Org keywords.")

  (setq org-todo-keyword-faces
        '(("CANCEL" . prot/org-bold-done)))
  (setq org-use-fast-todo-selection 'expert)
  (setq org-priority-faces nil)
  (setq org-fontify-done-headline nil)
  (setq org-fontify-todo-headline nil)
  (setq org-fontify-quote-and-verse-blocks t)
  (setq org-fontify-whole-heading-line nil)
  (setq org-fontify-whole-block-delimiter-line t)
  (setq org-highlight-latex-and-related nil) ; other options affect elisp regexp in src blocks
  (setq org-enforce-todo-dependencies t)
  (setq org-enforce-todo-checkbox-dependencies t)
  (setq org-track-ordered-property-with-tag t)
  (setq org-highest-priority ?A)
  (setq org-lowest-priority ?C)
  (setq org-default-priority ?A)

;;;; tags
  (setq org-tag-alist ; I don't really use those, but whatever
        '(("meeting")
          ("admin")
          ("emacs")
          ("modus")
          ("politics")
          ("economics")
          ("philosophy")
          ("book")
          ("essay")
          ("mail")
          ("purchase")
          ("hardware")
          ("software")
          ("website")))

  (setq org-auto-align-tags nil)
  (setq org-tags-column 0)

;;;; log
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (setq org-log-note-clock-out nil)
  (setq org-log-redeadline 'time)
  (setq org-log-reschedule 'time)
  (setq org-read-date-prefer-future 'time)

;;;; links
  (setq org-link-keep-stored-after-insertion nil)
  ;; TODO 2021-10-15 org-link-make-description-function

;;;; capture
  (setq org-capture-templates
        `(("b" "Basic task for future review" entry
           (file+headline "tasks.org" "Tasks to be reviewed")
           ,(concat "* %^{Title}\n"
                    ":PROPERTIES:\n"
                    ":CAPTURED: %U\n"
                    ":END:\n\n"
                    "%i %l")
           :empty-lines-after 1)
          ("c" "Clock in to a task" entry
           (file+headline "tasks.org" "Clocked tasks")
           ,(concat "* TODO %^{Title}\n"
                    ":PROPERTIES:\n"
                    ":EFFORT: %^{Effort estimate in minutes|5|10|15|30|45|60|90|120}\n"
                    ":END:\n\n"
                    "%a\n")
           :prepend t
           :clock-in t
           :clock-keep t
           :immediate-finish t
           :empty-lines-after 1)
          ("m" "Memorandum of conversation" entry
           (file+headline "tasks.org" "Tasks to be reviewed")
           ,(concat "* Memorandum of conversation with %^{Person}\n"
                    ":PROPERTIES:\n"
                    ":CAPTURED: %U\n"
                    ":END:\n\n"
                    "%i%?")
           :empty-lines-after 1)
          ("t" "Task with a due date" entry
           (file+headline "tasks.org" "Tasks with a date")
           ,(concat "* TODO %^{Title} %^g\n"
                    "SCHEDULED: %^t\n"
                    ":PROPERTIES:\n"
                    ":CAPTURED: %U\n"
                    ":END:\n\n"
                    "%a\n%i%?")
           :empty-lines-after 1)
          ("e" "Email note" entry
           (file+headline "tasks.org" "Tasks to be reviewed")
           ,(concat "* TODO %:subject :mail:\n"
                    ":PROPERTIES:\n"
                    ":CAPTURED: %U\n"
                    ":END:\n\n"
                    "%a\n%i%?")
           :empty-lines-after 1)
          ;; ;; NOTE 2023-01-29: See improved version in the
          ;; ;; prot-org.el further below.  It runs a custom function
          ;; ;; of mine that produces a more personalised template
          ;; ;; than what the built-in options support.
          ;;
          ;; ("p" "Private lesson or service" entry
          ;;  (file "coach.org")
          ;;  ,(concat "* COACH %^{Title} %^g\n"
          ;;           "%(prot/org-date-prompt-range-increment)"
          ;;           ":PROPERTIES:\n"
          ;;           ":CAPTURED: %U\n"
          ;;           ":APPT_WARNTIME: 20\n"
          ;;           ":END:\n\n"
          ;;           "%a\n%i%?")
          ;;  :prepend t
          ;;  :empty-lines 1)
          ))

;;;; agenda
;;;;; Basic agenda setup
  (setq org-default-notes-file (make-temp-file "emacs-org-notes-")) ; send it to oblivion
  ;; (setq org-agenda-files `(,org-directory))
  (setq org-agenda-files '("~/Dropbox/org/todo.org"))
  (setq org-agenda-span 'week)
  (setq org-agenda-start-on-weekday 1)  ; Monday
  (setq org-agenda-confirm-kill t)
  (setq org-agenda-show-all-dates t)
  (setq org-agenda-show-outline-path nil)
  (setq org-agenda-window-setup 'current-window)
  (setq org-agenda-skip-comment-trees t)
  (setq org-agenda-menu-show-matcher t)
  (setq org-agenda-menu-two-columns nil)
  (setq org-agenda-sticky nil)
  (setq org-agenda-custom-commands-contexts nil)
  (setq org-agenda-max-entries nil)
  (setq org-agenda-max-todos nil)
  (setq org-agenda-max-tags nil)
  (setq org-agenda-max-effort nil)

  ;; NOTE 2021-12-07: In my `prot-org.el' (see further below), I add
  ;; `org-agenda-to-appt' to various relevant hooks.
  ;;
  ;; Create reminders for tasks with a due date when this file is read.
  (run-at-time (* 60 5) nil #'org-agenda-to-appt)

;;;;; General agenda view options
  ;; NOTE 2021-12-07: Check further below my `org-agenda-custom-commands'
  (setq org-agenda-prefix-format
        '((agenda . " %i %-12:c%?-12t% s")
          (todo . " %i %-12:c")
          (tags . " %i %-12:c")
          (search . " %i %-12:c")))
  (setq org-agenda-sorting-strategy
        '(((agenda habit-down time-up priority-down category-keep)
           (todo priority-down category-keep)
           (tags priority-down category-keep)
           (search category-keep))))
  (setq org-agenda-breadcrumbs-separator "->")
  (setq org-agenda-todo-keyword-format "%-1s")
  (setq org-agenda-fontify-priorities 'cookies)
  (setq org-agenda-category-icon-alist nil)
  (setq org-agenda-remove-times-when-in-prefix nil)
  (setq org-agenda-remove-timeranges-from-blocks nil)
  (setq org-agenda-compact-blocks nil)
  (setq org-agenda-block-separator ?—)

;;;;; Agenda marks
  (setq org-agenda-bulk-mark-char "#")
  (setq org-agenda-persistent-marks nil)

;;;;; Agenda diary entries
  (setq org-agenda-insert-diary-strategy 'date-tree)
  (setq org-agenda-insert-diary-extract-time nil)
  (setq org-agenda-include-diary nil)
  ;; I do not want the diary, but there is no way to disable it
  ;; altogether.  This creates a diary file in the /tmp directory.
  (setq diary-file (make-temp-file "emacs-diary-"))
  (setq org-agenda-diary-file 'diary-file) ; TODO 2023-05-20: review Org diary substitute

;;;;; Agenda follow mode
  (setq org-agenda-start-with-follow-mode nil)
  (setq org-agenda-follow-indirect t)

;;;;; Agenda multi-item tasks
  (setq org-agenda-dim-blocked-tasks t)
  (setq org-agenda-todo-list-sublevels t)

;;;;; Agenda filters and restricted views
  (setq org-agenda-persistent-filter nil)
  (setq org-agenda-restriction-lock-highlight-subtree t)

;;;;; Agenda items with deadline and scheduled timestamps
  (setq org-agenda-include-deadlines t)
  (setq org-deadline-warning-days 0)
  (setq org-agenda-skip-scheduled-if-done nil)
  (setq org-agenda-skip-scheduled-if-deadline-is-shown t)
  (setq org-agenda-skip-timestamp-if-deadline-is-shown t)
  (setq org-agenda-skip-deadline-if-done nil)
  (setq org-agenda-skip-deadline-prewarning-if-scheduled 1)
  (setq org-agenda-skip-scheduled-delay-if-deadline nil)
  (setq org-agenda-skip-additional-timestamps-same-entry nil)
  (setq org-agenda-skip-timestamp-if-done nil)
  (setq org-agenda-search-headline-for-time nil)
  (setq org-scheduled-past-days 365)
  (setq org-deadline-past-days 365)
  (setq org-agenda-move-date-from-past-immediately-to-today t)
  (setq org-agenda-show-future-repeats t)
  (setq org-agenda-prefer-last-repeat nil)
  (setq org-agenda-timerange-leaders
        '("" "(%d/%d): "))
  (setq org-agenda-scheduled-leaders
        '("Scheduled: " "Sched.%2dx: "))
  (setq org-agenda-inactive-leader "[")
  (setq org-agenda-deadline-leaders
        '("Deadline:  " "In %3d d.: " "%2d d. ago: "))
  ;; Time grid
  (setq org-agenda-time-leading-zero t)
  (setq org-agenda-timegrid-use-ampm nil)
  (setq org-agenda-use-time-grid t)
  (setq org-agenda-show-current-time-in-grid t)
  (setq org-agenda-current-time-string
        (concat "Now " (make-string 70 ?-)))
  (setq org-agenda-time-grid
        '((daily today require-timed)
          ( 0500 0600 0700 0800 0900 1000
            1100 1200 1300 1400 1500 1600
            1700 1800 1900 2000 2100 2200)
          " ....." "-----------------"))
  (setq org-agenda-default-appointment-duration nil)

;;;;; Agenda global to-do list
  (setq org-agenda-todo-ignore-with-date t)
  (setq org-agenda-todo-ignore-timestamp t)
  (setq org-agenda-todo-ignore-scheduled t)
  (setq org-agenda-todo-ignore-deadlines t)
  (setq org-agenda-todo-ignore-time-comparison-use-seconds t)
  (setq org-agenda-tags-todo-honor-ignore-options nil)

;;;;; Agenda tagged items
  (setq org-agenda-show-inherited-tags t)
  (setq org-agenda-use-tag-inheritance
        '(todo search agenda))
  (setq org-agenda-hide-tags-regexp nil)
  (setq org-agenda-remove-tags nil)
  (setq org-agenda-tags-column -100)

;;;;; Agenda entry
  ;; NOTE: I do not use this right now.  Leaving everything to its
  ;; default value.
  (setq org-agenda-start-with-entry-text-mode nil)
  (setq org-agenda-entry-text-maxlines 5)
  (setq org-agenda-entry-text-exclude-regexps nil)
  (setq org-agenda-entry-text-leaders "    > ")

;;;;; Agenda logging and clocking
  ;; NOTE: I do not use these yet, though I plan to.  Leaving everything
  ;; to its default value for the time being.
  (setq org-agenda-log-mode-items '(closed clock))
  (setq org-agenda-clock-consistency-checks
        '((:max-duration "10:00" :min-duration 0 :max-gap "0:05" :gap-ok-around
                         ("4:00")
                         :default-face ; This should definitely be reviewed
                         ((:background "DarkRed")
                          (:foreground "white"))
                         :overlap-face nil :gap-face nil :no-end-time-face nil
                         :long-face nil :short-face nil)))
  (setq org-agenda-log-mode-add-notes t)
  (setq org-agenda-start-with-log-mode nil)
  (setq org-agenda-start-with-clockreport-mode nil)
  (setq org-agenda-clockreport-parameter-plist '(:link t :maxlevel 2))
  (setq org-agenda-search-view-always-boolean nil)
  (setq org-agenda-search-view-force-full-words nil)
  (setq org-agenda-search-view-max-outline-level 0)
  (setq org-agenda-search-headline-for-time t)
  (setq org-agenda-use-time-grid t)
  (setq org-agenda-cmp-user-defined nil)
  (setq org-agenda-sort-notime-is-late t) ; Org 9.4
  (setq org-agenda-sort-noeffort-is-high t) ; Org 9.4

;;;;; Agenda column view
  ;; NOTE I do not use these, but may need them in the future.
  (setq org-agenda-view-columns-initially nil)
  (setq org-agenda-columns-show-summaries t)
  (setq org-agenda-columns-compute-summary-properties t)
  (setq org-agenda-columns-add-appointments-to-effort-sum nil)
  (setq org-agenda-auto-exclude-function nil)
  (setq org-agenda-bulk-custom-functions nil)

;;;;; Agenda habits
  ;; (require 'org-habit)
  ;; (setq org-habit-graph-column 50)
  ;; (setq org-habit-preceding-days 9)
  ;; ;; Always show the habit graph, even if there are no habits for
  ;; ;; today.
  ;; (setq org-habit-show-all-today t)

;;;; code blocks
  (setq org-confirm-babel-evaluate nil)
  (setq org-src-window-setup 'current-window)
  (setq org-edit-src-persistent-message nil)
  (setq org-src-fontify-natively t)
  (setq org-src-preserve-indentation t)
  (setq org-src-tab-acts-natively t)
  (setq org-edit-src-content-indentation 0)

;;;; export
  (setq org-export-with-toc t)
  (setq org-export-headline-levels 8)
  (setq org-export-dispatch-use-expert-ui nil)
  (setq org-html-htmlize-output-type nil)
  (setq org-html-head-include-default-style nil)
  (setq org-html-head-include-scripts nil)
  ;; (require 'ox-texinfo)
  ;; (require 'ox-md)

;;;; IDs
  (setq org-id-link-to-org-use-id
        'create-if-interactive-and-no-custom-id)

;;;; Hooks and key bindings

  ;; See my `pulsar' package, defined elsewhere in this setup.
  (with-eval-after-load 'pulsar
    (dolist (hook '(org-agenda-after-show-hook org-follow-link-hook))
      (add-hook hook #'pulsar-recenter-center)
      (add-hook hook #'pulsar-reveal-entry)))

  (prot-emacs-keybind global-map
    "C-c A" #'org-agenda ; see the `prot-org' section for C-c a
    "C-c c" #'org-capture
    "C-c l" #'org-store-link
    "C-c o" #'org-open-at-point-global)

  (require 'org)

  (prot-emacs-keybind org-mode-map
    ;; I don't like that Org binds one zillion keys, so if I want one
    ;; for something more important, I disable it from here.
    "C-'" nil
    "C-," nil
    "M-;" nil
    "<C-return>" nil
    "<C-S-return>" nil
    "C-M-S-<right>" nil
    "C-M-S-<left>" nil
    "C-c M-l" #'org-insert-last-stored-link
    "C-c C-M-l" #'org-toggle-link-display)

;;; Custom extensions (prot-org.el)
  (require 'prot-org)
  (setq org-agenda-format-date #'prot-org-agenda-format-date-aligned)

  ;; Check the variable `prot-org-custom-daily-agenda' in prot-org.el
  (setq org-agenda-custom-commands
        `(("A" "Daily agenda and top priority tasks"
           ,prot-org-custom-daily-agenda
           ((org-agenda-fontify-priorities nil)
            (org-agenda-dim-blocked-tasks nil)))
          ("P" "Plain text daily agenda and top priorities"
           ,prot-org-custom-daily-agenda
           ((org-agenda-with-colors nil)
            (org-agenda-prefix-format "%t %s")
            (org-agenda-current-time-string ,(car (last org-agenda-time-grid)))
            (org-agenda-fontify-priorities nil)
            (org-agenda-remove-tags t))
           ("agenda.txt"))))

  ;; I bind `org-agenda' to C-c A, so this one puts me straight into my
  ;; custom block agenda.
  (define-key global-map (kbd "C-c a") (lambda () (interactive) (org-agenda nil "A")))

  (prot-emacs-keybind ctl-x-x-map
    "i" #'prot-org-id-headlines
    "h" #'prot-org-ox-html)

  (add-to-list 'org-capture-templates
               '("p" "Private lesson or service" entry
                 (file "coach.org")
                 #'prot-org-capture-coach
                 :prepend t
                 :empty-lines 1))

  (add-to-list 'org-capture-templates
               '("P" "Private service clocked" entry
                 (file+headline "coach.org" "Clocked services")
                 #'prot-org-capture-coach-clock
                 :prepend t
                 :clock-in t
                 :clock-keep t
                 :immediate-finish t
                 :empty-lines 1)))

(provide 'prot-emacs-org)
