;;; -*- lexical-binding: t; -*-

(defun hax/org-mode-configure ()
  (interactive)
  ;; Default inline latex highlighting is a bold white text, which is too
  ;; similar to a regular text.
  ;; (set-face-attribute
  ;;  'org-latex-and-related nil
  ;;  :foreground "dim gray")

  (setq flyspell-generic-check-word-predicate 'hax/flyspell-org-mode-verify)

  (defface org-bold `((t :inherit bold :foreground ,(doom-lighten 'red 0.7)))
    "Face for bold text in Org mode.")

  (defface org-italic `((t :inherit italic :foreground ,(doom-lighten 'yellow 0.7)))
    "Face for bold text in Org mode.")

  (defface org-underline `((t :inherit underline :foreground ,(doom-lighten 'cyan 0.7)))
    "Face for bold text in Org mode.")

  ;; Update the org-emphasis-alist
  (setq org-emphasis-alist
        '(("*" org-bold)
          ("/" org-italic)
          ("_" org-underline)
          ("=" org-verbatim)
          ("~" org-code)
          ("+" (:strike-through t))))

  (global-company-mode -1)
  (org-link-set-parameters "coords" :follow #'org-coords-open)
  (require 'ts)
  (require 'org-expiry)
  (require 'org-clock)
  (define-abbrev-table 'org-mode-abbrev-table
    '(("anon" "anonymous")
      ("inf" "infinite")
      ("vm" "VM")
      ("Flugel" "Fl\\Uuml{}gel")
      ;; Yes, I can't type for shit
      ("ofthe" "of the")
      ("tothe" "to the")
      ("onthe" "on the")
      ("forthe" "for the")
      ("athe" "at the")
      ("inthe" "in the")
      ("ast" "AST")
      ("teh" "the")
      ("im" "I'm")
      ("ambig" "ambiguous")
      ("assesment" "assessment")
      ("i" "I")))
  (setq org-priority-highest ?A)
  (setq org-priority-lowest ?X)
  (setq org-priority-default ?B)



  (setq org-priority-faces '((?A . (:foreground "#F0DFAF" :weight bold :underline t :overline t))
                             (?B . (:foreground "#FD971F"))
                             (?C . (:foreground "#66D9EF"))
                             (?D . (:foreground "#A1EFE4"))
                             (?E . (:foreground "#A6E22E" :weight light))
                             (?F . (:foreground "#AE81FF" :weight light))
                             (?S . (:foreground "#FD5FF0" :box t))
                             (?X . (:foreground "red" :box (:line-width 2 :color "red")))))

  (setq
   org-cycle-separator-lines 12000
   org-capture-templates
   ;; agenda just includes everything that contains an "active time
   ;; stamp". An active time stamp is any time stamp in angular
   ;; brackets. That's why all templates use `%U' instead of `%T' for
   ;; timestamps - creation date should not be inserted in the agenda.
   '(;; Add new entry to the inbox. No sorting, no hierarchical placement,
     ;; just dump everything in it, refile later.
     ("d" "Daily" entry (file+olp+datetree hax/notes.org)
      "** %U W%<%U> :recollection##day:
  :PROPERTIES:
  :CREATED: %U
  :END:

%?"
      :empty-lines-before 1
      :empty-lines-after 1)
     ("@" "Daily" entry (file+olp+datetree hax/notes.org)
      "** %U %(hax/immediate-note-tags)
  :PROPERTIES:
  :CREATED: %U
  :END:

%(hax/get-immediate-note-content)
"
      :immediate-finish t
      :empty-lines-before 1
      :empty-lines-after 1)
     ;; Immediate plans for today. Similar to GTD inbox, but reserved for
     ;; very minor items used to organize the thoughts. Items from here can
     ;; duplicate others (heading might be a direct link), and they are not
     ;; refiled anywhere else.
     ("i" "Immediate" entry (file+olp+datetree hax/notes.org)
      "* TODO %?
  :PROPERTIES:
  :CREATED: %U
  :END:
"
      :prepend t
      :empty-lines-after 1
      :empty-lines-before 1)
     ("D" "Daily start" plain (file+olp+datetree hax/notes.org)
      "%i%?"
      :empty-lines-before 1
      :empty-lines-after 1)
     ("c" "Clock" entry (clock)
      ;; (function hax/goto-top-clock)
      "** TODO %?
  :PROPERTIES:
  :CREATED: %U
  :END:
"
      :empty-lines-before 1
      :empty-lines-after 1)
     ("S" "Staging subtree" entry
      (function (lambda () (hax/org-goto-select-subtree
                            (org-collect-known-entries (list hax/staging.org)))))
      "** TODO %?
  :PROPERTIES:
  :CREATED: %U
  :END:
"
      :empty-lines-before 1
      :empty-lines-after 1
      )
     ("s" "Staging toplevel" entry (file hax/staging.org)
      ;; (function hax/goto-top-clock)
      "* TODO %?
  :PROPERTIES:
  :CREATED: %U
  :END:
"
      :empty-lines-before 1
      :empty-lines-after 1
      )
     ;; Todo items that don't have a concrete deadline yet, but must be
     ;; positioned under some task
     ("u" "Subtask under" entry
      (function hax/org-goto-select-subtree)
      "** TODO %?
  :PROPERTIES:
  :CREATED: %U
  :END:
"
      :empty-lines-before 1
      :empty-lines-after 1
      )
     ("a" "Subtask under active" entry
      (function hax/org-goto-select-active-subtree)
      "** TODO %?
  :PROPERTIES:
  :CREATED: %U
  :END:
"
      :empty-lines-before 1
      :empty-lines-after 1)
     ))

  (when hax/+roam
    (setq
     ;; Don not open org-roam buffer automatically
     +org-roam-open-buffer-on-find-file nil
     ;; Don't implicitly enable the super-distracting,
     ;; almost-never-needed-unless-explicitly requested automatic UI follow
     ;; mode that zooms to closely no nodes that you instantly loose all the
     ;; context of what you are working with.
     org-roam-ui-follow nil))

  (setq
   ;; Agenda is a main todo file and inbox
   org-agenda-files (list hax/main.org
                          hax/staging.org
                          hax/notes.org
                          hax/repeated.org
                          hax/projects.org
                          hax/projects_cold.org)
   org-refile-targets `((nil :maxlevel . 4)
                        (,hax/fic.org :maxlevel . 9)
                        (,hax/main.org :maxlevel . 3)
                        (,hax/projects.org :maxlevel . 7)
                        (,hax/projects_cold.org :maxlevel . 7)
                        (,hax/notes.org :maxlevel . 3)
                        (,hax/staging.org :maxlevel . 1))
   ;; Yes, you can in fact consider the entry completed with some of the
   ;; `TODO' left over, this happens, in real life not all tasks must be
   ;; closed with 100% accuracy.
   org-enforce-todo-dependencies nil
   ;; Add `COMPLETED' timestamp property
   org-log-done 'time
   ;; Log when schedule changed
   org-log-reschedule 'time
   org-tags-column -1
   ;; Notes should go from top to bottom
   org-log-states-order-reversed nil
   ;; Log when deadline changed
   org-log-redeadline 'time
   org-clock-modeline-total 'today
   ;; Not using default configuration - instead more informative version is
   ;; implemented using custom hooks.
   org-log-refile nil
   ;; This looks nice but has a lot of random glitches on this
   ;; configuration
   org-startup-indented t
   ;; Start week on monday like any normal human being
   calendar-week-start-day 1
   ;; Insert newlines before heading, but plain lists should be packed
   ;; together.
   org-blank-before-new-entry '((heading . t) (plain-list-item . nil))
   org-hierarchical-todo-statistics t
   ;; Override of the default agenda date formatting with customized
   ;; function
   org-agenda-format-date 'hax/org-agenda-format-date
   org-goto-interface 'outline-path-completionp
   org-image-actual-width (list 300)
   ;; Store seconds in the timestamp format
   org-time-stamp-formats '("<%Y-%m-%d %a>" . "<%Y-%m-%d %a %H:%M:%S %Z>")
   ;; Fontification of the 'headings' also affects checkbox items, and I
   ;; tend to them on several lines where heading fontification only
   ;; highlights first one. So essentially this features is pretty nice,
   ;; but just doesn't work properly for my use case
   org-fontify-done-headline nil
   org-startup-with-inline-images nil
   ;; `:DRAWER:' should be indented to the heading
   org-adapt-indentation t
   ;; Notes
   org-log-into-drawer t
   ;; Don't remove zero-length clocks. Most tasks can't be accomplished
   ;; under a minute, but I like to avoid having extra smart filtering of
   ;; the actions I make. If I need to do the analysis of my actions I can
   ;; filter out outliers manually
   org-clock-out-remove-zero-time-clocks nil)

  (setq
   org-agenda-custom-commands
   `(("l" "Long"
      ((agenda
        ""
        ((org-agenda-span 30)
         (org-agenda-start-day "-0d")
         (org-deadline-warning-days 35)))))
     ("*" "All"
      ((todo
        "NEXT|WIP|PAUSED|BLOCKED"
        ((org-agenda-overriding-header "In progress (NEXT/WIP/PAUSED/BLOCKED)")))
       (todo
        "TODO"
        ((org-agenda-overriding-header "Staging todo")
         (org-agenda-skip-function #'hax/org-agenda-skip)
         (org-agenda-files '(,hax/staging.org))))
       (todo
        "TODO"
        ((org-agenda-overriding-header "Notes & High priority project todos")
         (org-agenda-skip-function
          (lambda ()
            (cond
             ((string= (buffer-file-name) (expand-file-name hax/notes.org))
              (hax/org-agenda-skip))
             ((string= (buffer-file-name) (expand-file-name hax/projects.org))
              (hax/org-agenda-skip-low-priority))
             (t (point-max)))))
         (org-agenda-files (list hax/notes.org hax/projects.org))))
       (agenda
        ""
        ((org-agenda-overriding-header "2-week preview")
         (org-agenda-span 14)
         (org-agenda-start-day "-0d")
         (org-agenda-skip-function #'hax/org-agenda-skip)
         (org-deadline-warning-days 0)))
       (todo
        "TODO"
        ((org-agenda-overriding-header "Repeated todos")
         (org-agenda-files '(,hax/repeated.org))))))))

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((C . t)
     (nim . t)
     (shell . t)
     (mermaid . t)
     (sqlite . t)
     (python . t)))
  ;; Because fucking mmdc is /so/ stupid it cannot find `(which chromium-browser)'
  (setenv "PUPPETEER_EXECUTABLE_PATH" "/bin/chromium-browser")
  ;; And yes, whatever regular npm install does, it manages to fuck it up.
  (setq ob-mermaid-cli-path (f-expand "~/software/node_modules/.bin/mmdc"))
  (setq org-latex-listings 'minted)
  (setq org-todo-keywords
        ;; I don't use most of these keywords as well, and sometimes they
        ;; overlap with GTD management. You can think of them as "yes" and
        ;; "yes, but in green"
        '(("TODO(t!/!)"         ;; Must be done now
           "TODO(t!/!)"         ;; Some other function breaks
           ;; org-todo-keywords, and I need to add
           ;; TODO twice here. I don't know what is
           ;; the cause, but this is a FIXME, although
           ;; with low priority.
           "WIP(w!)"            ;; Working on it
           "NEXT(n!)"            ;; Working on it
           "TRIAGED(T!)"        ;; Investigated the issue in some detail,
           ;; might work on it later
           "REVIEW(r!/!)"       ;; Check if this task must be done or not
           "|"
           ;; Work is temporarily paused, but I have a vague idea about
           ;; next restart time.
           "PAUSED(p@/!)"
           "BLOCKED(b@/@)"   ;; External event is preventing further work
           "TIMEOUT(T)"      ;; Cannot be done due to time limits
           "FAILED(f@/@)"    ;; Tried to finish the task but failed
           "CANCELED(C@/@)"
           "COMPLETED(c!/@)" ;; Task completed
           "PARTIALLY(P@/@)" ;; Can be considered completed
           )))
  (setq org-todo-keyword-faces
        `(("TODO" . "orange")
          ("PAUSED" . "coral")
          ("CANCELED" . "snow")
          ("WIP" . "tan")
          ("BLOCKED" . "purple")

          ("PARTIALLY" . "goldenrod")
          ("REVIEW" . "SteelBlue")
          ("FAILED" . ,(doom-color 'red))
          ("TIMEOUT" . ,(doom-color 'red))))
  (setq hax/tags-file (f-full "~/.config/tags"))
  (setq hax/private-tags-file (f-full "~/.hax-private-tags"))
  (setq hax/private-tags-prefix-list '("work##vizio" "work##epam"))
  (when (f-exists? (f-join hax/cache.d "org-clock-stack"))
    (setq hax/org-clock-stack
          (read-from-file (f-join hax/cache.d "org-clock-stack"))))

  (when (f-exists? (f-join hax/cache.d "org-clock-history"))
    (setq hax/org-clock-history
          (read-from-file (f-join hax/cache.d "org-clock-history"))))
  (when (not (f-exists? hax/tags-file)) (f-touch hax/tags-file))
  (setq org-tag-alist
        (let ((tag-list
               (append
                (s-split "\n" (if (f-exists? hax/tags-file)
                                  (f-read hax/tags-file) "") t)
                (s-split "\n" (if (f-exists? hax/private-tags-file)
                                  (f-read hax/private-tags-file) "") t))))
          (cl-remove-duplicates
           (--map
            `(,(substring it 1 (length it)) . ??)
            (--filter (< 0 (length it)) tag-list))
           :test #'equal))))
