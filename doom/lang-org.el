;;; -*- lexical-binding: t; -*-


(defun hax/org-update-all-cookies ()
  (interactive)
  (let ((current-prefix-arg '(4)))
    (org-update-statistics-cookies "ALL")))

(defun hax/org-insert-todo-entry ()
  "Insert new TODO entry with creation date filled in. Simplified
down version of the `+org/insert-item-below' that does not do
anything context-aware. It simply inserts new 'TODO' entry after
the current one."
  (interactive)
  (org-end-of-subtree)
  (insert "\n\n" (make-string (or (org-current-level) 1) ?*) " TODO ")
  (org-expiry-insert-created)
  (evil-insert-state)
  (hax/org-update-all-cookies))

(defun hax/popup-mode-hook ()
  (interactive)
  (message "Evil insertt state triggered"))

;; (add-hook! '+popup-mode-hook 'hax/popup-mode-hook)

;; (after!
;;   )

(defun font-lock-replace-keywords (mode keyword-list)
  "Use `font-lock-remove-keywords' on each keyword and then add
  `keyword-list' to font lock."
  (font-lock-remove-keywords
   mode
   (-map 'car keyword-list))
  (font-lock-add-keywords mode keyword-list))

(setq abbrev-expand-function
      (lambda ()
        (unless (org-in-src-block-p)
          (abbrev--default-expand))))

(font-lock-add-keywords
 'org-mode
 `((,(rx bol (any space) "- " (group "[X]")) 1 'org-done prepend)
   (,(rx bol (any space) "- " (group "[ ]")) 1 'org-todo prepend))
 'append)

(defun hax/replace-next-placeholder ()
  "Find placeholder string, ('{{{replace-target}}}') delete it and enter insert
mode"
  (interactive)
  (let ((target "{{{replace}}}"))
    (search-forward target)
    (delete-backward-char (length target))
    (evil-insert-state)))

(defun hax/org-download-setup ()
  (require 'org-download)
  (let ((base
         (if (buffer-file-name) (buffer-file-name)
           (let ((buf (org-capture-get :buffer)))
             (if buf (buffer-file-name buf)) (f-join hax/todo.d "images")))))
    (setq org-download-image-dir (f-dirname base))
    (setq org-download-timestamp (concat (f-filename base) "_%Y%m%d-%H%M%S_" ))
    (setq org-image-actual-width 300)
    (setq org-download-link-format "[[file:%s]]")
    (setq org-download-method 'directory)
    (setq org-download-heading-lvl nil)))

(defun hax/org-paste-clipboard (&optional default-name)
  (interactive "P")
  (require 'org-download)
  (let ((file
         (if default-name
             (read-string
              (format "Filename [%s]: " org-download-screenshot-basename)
              nil nil org-download-screenshot-basename)

           default-name))
        (out (s-split
              "\n"
              (with-output-to-string
                (call-process
                 "xclip" nil standard-output nil
                 "-o" "-selection" "cli" "-t" "TARGETS")))))
    ;; If clipboard does not contain image use regular pasting logic,
    ;; otherwise insert image. Mapybe `p' should work differently in
    ;; org-mode instead, but I'm not sure about that.
    (if (--any? (s-starts-with? "image/" it) out)
        (org-download-clipboard file)
      (evil-paste-after-without-register 1))))

(defun hax/org-mode-hook ()
  (interactive)
  ;; https://aliquote.org/post/enliven-your-emacs/ font-lock `prepend/append'
  ;; pair here is copied from this blog post, since I can't really figure out
  ;; what exactly is going on with ordering. But that implementation allows
  ;; me to override the default checkbox highlighting for checkboxes.
  

  (message "Org-mode hook executed")
  (abbrev-mode 1)
  (flyspell-mode 1)
  (org-indent-mode -1)
  ;; Indentation guides slow down org-mode when there are multiple folds
  ;; (at least I was able to identifiy the implementation ot that point)
  (highlight-indent-guides-mode -1)
  ;; https://github.com/hlissner/doom-emacs/blob/develop/docs/faq.org#my-new-keybinds-dont-work
  ;; because I override the default keybindings I had to use this
  ;; abomination of a `map!' call to do what I need.
  (map!
   :map org-mode-map
   [C-S-return] nil
   [C-return] nil
   [M-return] nil
   )

  (map! :map evil-org-mode-map :nvi "M-p" #'hax/org-paste-clipboard)
  (map! :map org-mode-map :nvi "M-p" #'hax/org-paste-clipboard)

  (map!
   :map evil-org-mode-map
   :ni [C-S-return] nil
   :ni [C-return] nil
   [M-return] nil
   )

  (map!
   :map org-mode-map
   [M-return] #'org-add-note
   [C-S-return] #'hax/org-insert-todo-entry
   [C-return] #'+org/insert-item-below)

  (hax/org-download-setup)
  (map!
   :map evil-org-mode-map
   ;; Consistent multicursor bindings are more important for me, so
   ;; `org-shiftdown' and `org-shiftup' can go elsewhere.
   :ni "C-S-j" nil
   :ni "C-S-k" nil

   :ni "C-;" #'flyspell-correct-wrapper
   [M-return] #'org-add-note
   :ni [C-S-return] #'hax/org-insert-todo-entry
   :ni [C-return] #'+org/insert-item-below

   :nv ",#" #'hax/org-update-all-cookies
   :nv ",eh" 'org-html-export-to-html
   :desc "Async export to pdf"
   :nv ",ep" (cmd! (org-latex-export-to-pdf t))
   :nv ",ea" 'org-ascii-export-to-ascii
   :nv ",eP" 'org-latex-export-to-pdf
   :nv ",el" 'org-latex-export-to-latex
   :nv ",in" #'org-add-note
   :n ",tc" #'org-toggle-checkbox
   :n ",ta" #'counsel-org-tag
   :desc "insert #+begin_src"
   :n ",ic" (cmd!
             (evil-insert-state)
             (yas-expand-snippet "#+begin_src $1\n$0\n#+end_src"))
   :nv ",hi" #'org-indent-mode
   :nv ",ci" #'org-clock-in
   :nv ",co" #'org-clock-out
   :desc "start WIP clocking"
   :nv ",cw" (cmd! (org-todo "WIP") (org-clock-in)))

  (map!
   :leader
   :n "SPC" #'hax/replace-next-placeholder)

  (setq company-backends
        '(company-capf (:separate company-ispell company-dabbrev company-yasnippet))))

(add-hook! 'org-mode-hook 'hax/org-mode-hook)

(defun hax/org-capture-hook ()
  (interactive)
  ;; I use adaptive indentation for drawers, but I don't want to forcefully
  ;; indent the text inside of the subtrees.
  (let ((text (buffer-substring (line-beginning-position) (point))))
    (when (string-match "^\s+$" text)
      (message "%s" text)
      (delete-region (line-beginning-position) (point)))))

;; Scroll to the last position of the message buffer after something has
;; been printed.
(defadvice message (after message-tail activate)
  "goto point max after a message"
  (with-current-buffer "*Messages*"
    (goto-char (point-max))
    (walk-windows
     (lambda (window)
       (if (string-equal (buffer-name (window-buffer window)) "*Messages*")
           (set-window-point window (point-max))))
     nil
     t)))


(add-hook! 'org-capture-mode-hook 'hax/org-capture-hook)
(setq hax/fullscreen-capture nil)
(defun hax/org-post-capture-hook ()
  (interactive)
  ;; If starting capture from dropdown mode, delete the client window frame
  ;; after. That won't work if it is the single client, but when launching
  ;; using daemon mode it does /exactly/ what is needed.
  (when hax/fullscreen-capture
    (delete-frame))
  ;; Fullscreen capture must be set again with new `--eval' argument
  ;; when starting next cature.
  (setq hax/fullscreen-capture nil))

(add-hook! 'org-capture-after-finalize-hook 'hax/org-post-capture-hook)


(map!
 :n ",nt" (cmd! (org-capture nil "t"))
 :n ",nd" (cmd! (org-capture nil "d"))
 ;; Quick access to common operations. No specific meaning behind the key
 ;; specification.
 :desc "Global agenda"
 :n [M-f7] (cmd! (org-agenda nil "*"))
 :desc "New note"
 :n [M-f8] (cmd! (org-capture nil "d"))
 :desc "New todo"
 :n [M-f9] (cmd! (org-capture nil "t"))
 :desc "New note for clock"
 :n [M-f10] (cmd! (org-capture nil "c")))

(defun hax/agenda-mode-hook ()
  (interactive)
  ;; I want to be able to use fX __**EVERYWHERE**__.
  (local-set-key (kbd "<f1>") 'winum-select-window-1)
  (local-set-key (kbd "<f2>") 'winum-select-window-2)
  (local-set-key (kbd "<f3>") 'winum-select-window-3)
  (local-set-key (kbd "<f4>") 'winum-select-window-4)
  (local-set-key (kbd "<f5>") 'winum-select-window-5)
  (local-set-key (kbd "<f6>") 'winum-select-window-6)
  (local-set-key (kbd "<M-f7>") 'org-agenda-quit))

(add-hook! 'org-agenda-mode-hook 'hax/agenda-mode-hook)


(defmacro hax/defaccept! (name body)
  `(defun ,name ()
     (let* (;; Use org-element API instead of cave-tier hacks with forward
            ;; regex searches.
            (tree (org-element-at-point))
            ;; Store the position of the tree end
            (subtree-end (save-excursion (org-end-of-subtree t))))
       ;; If element is not scheduled, skip it
       (unless ,body
         subtree-end))))

(hax/defaccept! accept-scheduled/deadlined
                (or (org-element-property :scheduled tree)
                    (org-element-property :deadline tree)))

(hax/defaccept! skip-scheduled/deadlined
                (not (or (org-element-property :scheduled tree)
                         (org-element-property :deadline tree))))

(setq
 org-agenda-custom-commands
 `(("*" "All"
    ((agenda
      ""
      ((org-agenda-span 7)
       ;; Start showing events from today onwards, when quickly assessing
       ;; target tasks I don't really need to focus on the past events.
       (org-agenda-start-day "-0d")
       (org-agenda-skip-function 'accept-scheduled/deadlined)))
     (todo "WIP")
     (todo
      "TODO"
      ((org-agenda-skip-function 'skip-scheduled/deadlined)
       (org-agenda-sorting-strategy '((priority-down)))))
     (todo "POSTPONED")))))

(defun hax/parent-subtrees()
  "test"
  (let* ((len 18)
         (str (s-pad-left len " " (s-join "." (org-get-outline-path)))))
    (substring str (- (length str) len) (length str))))

(setq
 org-agenda-prefix-format '((agenda . " %i %-12t% s %(hax/parent-subtrees) ")
                            (todo . " %i %(hax/parent-subtrees) ")
                            (tags . " %i %(hax/parent-subtrees) ")
                            (search . " %i %(hax/parent-subtrees) "))
 org-agenda-start-on-weekday nil
 org-agenda-ndays 14
 org-agenda-show-all-dates t
 org-agenda-skip-deadline-if-done t
 org-agenda-skip-scheduled-if-done t
 org-agenda-repeating-timestamp-show-all nil
 org-deadline-warning-days 14
 org-agenda-start-day "-0d"
 )



(defun hax/capture-location ()
  "Get formatted string about capture location"
  (let ((orig (org-capture-get :original-file)))
    (if (magit-toplevel)
        (let* ((top (f-filename (magit-toplevel)))
               (file (magit-file-relative-name orig)))
          (format "from ~%s:%s:%s~" top file (point)))
      (if orig
          (format "from ~%s:%s~" (f-filename orig) (point))
        ""))))

(after!
  org
  (require 'org-expiry)
  (define-abbrev-table 'org-mode-abbrev-table
    '(("rst" "RST")
      ("anon" "anonymous")
      ("inf" "infinite")
      ("js" "JavaScript")
      ("rust" "Rust")
      ("vm" "VM")
      ("dod" "data-oriented")
      ("cxx" "C++")
      ("im" "I'm")
      ("ambig" "ambiguous")
      ("i" "I")))

  (setq org-capture-templates
        '(;; Add new entry to the inbox. No sorting, no hierarchical placement,
          ;; just dump everything in it, refile later.
          ("t" "GTD todo inbox" entry (file hax/inbox.org)
           "* TODO %?
  :PROPERTIES:
  :CREATED: %T
  :END:
"
           :empty-lines-before 1
           :empty-lines-after 1)
          ("d" "Daily" entry (file+olp+datetree hax/notes.org)
           "** %T %(hax/capture-location)\n\n%?"
           :empty-lines-before 1
           :empty-lines-after 1)
          ("c" "Clock" entry (clock)
           "** %T %(hax/capture-location)\n\n%?"
           :empty-lines-before 1
           :empty-lines-after 1)))

  (setq
   ;; Main notes directory
   org-directory "~/defaultdirs/notes/personal"
   ;; Directory for todo management
   hax/todo.d (f-join org-directory "todo")
   ;; GTD inbox
   hax/inbox.org (f-join hax/todo.d "inbox.org")
   ;; Main GTD organizer
   hax/main.org (f-join hax/todo.d "main.org")
   ;; Random junk notes that I generate, copy from other places etc.
   hax/notes.org (f-join hax/todo.d "notes.org")
   ;; Agenda is a main todo file
   org-agenda-files (list hax/main.org hax/inbox.org)
   org-refile-targets `((,hax/main.org :maxlevel . 2))

   ;; Log when schedule changed
   org-log-reschedule 'time
   ;; Notes should go from top to bottom
   org-log-states-order-reversed nil
   ;; Log when deadline changed
   org-log-redeadline 'time
   org-log-refile 'time
   ;; This looks nice but has a lot of random glitches on this
   ;; configuration
   org-startup-indented nil
   ;; Start week on monday like any normal human being
   calendar-week-start-day 1
   ;; Insert newlines before heading, but plain lists should be packed
   ;; together.
   org-blank-before-new-entry '((heading . t) (plain-list-item . nil))
   org-hierarchical-todo-statistics t
   org-image-actual-width (list 300)
   ;; Store seconds in the timestamp format
   org-time-stamp-formats '("<%Y-%m-%d %a>" . "<%Y-%m-%d %a %H:%M:%S>")
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

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((C . t)
     (nim . t)
     (shell . t)
     (python . t)))
  (setq org-todo-keywords
        ;; I don't use most of these keywords as well, and sometimes they
        ;; overlap with GTD management. You can think of them as "yes" and
        ;; "yes, but in green"
        '((sequence
           "TODO(t!/!)"         ;; Must be done now
           "LATER(l!)"          ;; Can be done sometimes later
           "NEXT(n!)"           ;; Next task after current
           "POSTPONED(P!/!)"    ;; Work is temporarily paused
           "WIP(w!)"            ;; Working on it
           "STALLED(s!)"        ;; External event is preventing further work
           "REVIEW(r!/!)"       ;; Check if this task must be done or not
           "TIMEOUT(T)"         ;; Cannot be done due to time limits
           "FAILED(f@/!)"       ;; Tried to finish the task but failed
           "CANCELED(C@/!)"
           "|"
           "DONE(d!/@)"         ;; Task completed
           "COMPLETED(c!/@)"    ;; Task completed
           "NUKED(N@/!)"        ;; Completed but angry
           "PARTIALLY(p@/!)"    ;; Can be considered completed
           "FUCKING___DONE(F)"  ;; Completed but very angry
           )))
  (setq org-todo-keyword-faces
        '(("TODO" . "orange")
          ("LATER" . "DeepPink")
          ("NEXT" . "DarkRed")
          ("POSTPONED" . "coral")
          ("DONE" . "green")
          ("NUKED" . "purple")
          ("CANCELED" . "snow")
          ("PARTIALLY" . "goldenrod")
          ("WIP" . "tan")
          ("STALLED" . "gold")
          ("REVIEW" . "SteelBlue")
          ("FAILED" . "red")
          ("FUCKING___DONE" . "gold1")
          ("TIMEOUT" . "red")))
  (setq org-tag-alist
        (concatenate
         'list
         '((:startgroup . nil)
           ("@work" . ?w)
           ("@home" . ?h)
           ;; Personal code and other projects
           ("@projects" . ?p)
           ("@organization" . ?o)
           ("@errands" . ?e)
           (:endgroup . nil))
         (--map
          `(,(substring it 1 (length it)) . ??)
          (--filter
           (< 0 (length it))
           (s-split "\n" (f-read (f-join hax/todo.d "tags"))))))))
