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
    (setq org-download-image-dir
          (f-join (f-dirname base) (concat (f-base base) ".images")))
    (setq org-download-timestamp (concat (f-filename base) "_%Y%m%d-%H%M%S_" ))
    (setq org-image-actual-width 300)
    ;; Update configuration to it's original values - some doom emacs
    ;; configuration changes these as well, and I don't need it.
    (setq org-download-link-format-function #'org-download-link-format-function-default)
    (setq org-download-abbreviate-filename-function #'file-relative-name)
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

(defun hax/org-assign-tag ()
  "Add or remove tags in `org-mode'. If new tag is added, store
it in the persistent list of tags, and update current list of tags"
  (interactive)
  (save-excursion
    (if (eq major-mode 'org-agenda-mode)
        (if org-agenda-bulk-marked-entries
            (setq counsel-org-tags nil)
          (let ((hdmarker (or (org-get-at-bol 'org-hd-marker)
                              (org-agenda-error))))
            (with-current-buffer (marker-buffer hdmarker)
              (goto-char hdmarker)
              (setq counsel-org-tags (counsel--org-get-tags)))))
      (unless (org-at-heading-p)
        (org-back-to-heading t))
      (setq counsel-org-tags (counsel--org-get-tags)))
    (let* ((org-last-tags-completion-table
            (append (and (or org-complete-tags-always-offer-all-agenda-tags
                             (eq major-mode 'org-agenda-mode))
                         (org-global-tags-completion-table
                          (org-agenda-files)))
                    (unless (boundp 'org-current-tag-alist)
                      org-tag-persistent-alist)
                    (or (if (boundp 'org-current-tag-alist)
                            org-current-tag-alist
                          org-tag-alist)
                        (org-get-buffer-tags))))
           (selected (ivy-read (counsel-org-tag-prompt)
                               (lambda (str _pred _action)
                                 (delete-dups
                                  (all-completions str #'org-tags-completion-function)))
                               :history 'org-tags-history
                               :action #'counsel-org-tag-action
                               :caller 'hax/org-assign-tag)))
      (unless (--any (s-equals? (car it) selected) org-tag-alist)
        (f-append-text (concat "\n#" selected) 'utf-8 hax/tags-file)
        (setq org-tag-alist (push (cons selected ??) org-tag-alist ))
        (message
         "New tag %s"
         (propertize selected 'face `(:foreground ,(doom-color 'red)))))
      selected)))

(defun hax/org-insert-link-to-subtree (&optional description)
  "Insert link to any subtree in any org file, using
`[[path][ID]]' link. If DESCRIPTION is `nil', use the heading
name, otherwise use description."
  (interactive)
  (let (entries)
    (dolist (b (buffer-list))
      (with-current-buffer b
        (when (derived-mode-p 'org-mode)
          (setq entries
                (nconc entries
                       (counsel-outline-candidates
                        (cdr (assq 'org-mode counsel-outline-settings))
                        (counsel-org-goto-all--outline-path-prefix)))))))
    (ivy-read
     "Goto: " entries
     :history 'counsel-org-goto-history
     :action (lambda (x)
               ;; Go to position of the found entry, get IT's id (creating
               ;; one if it is missing), and then insert result into the
               ;; final output
               (let ((id (with-current-buffer (marker-buffer (cdr x))
                           ;; Save excursion to avoid moving cursor in
                           ;; other buffers (or in the same buffer if
                           ;; linking within one file)
                           (save-excursion
                             (goto-char (marker-position (cdr x)))
                             (org-id-get-create)))))
                 (insert (format " [[id:%s][%s]]" id (if description description (car x))))))
     :caller 'hax/org-insert-link-to-heading)))

(cl-defun www-get-page-title (url &optional (timeout 15))
  "Return title of the URL page, or if not found the page's
filename (as an approximation)"
  (message "%s" timeout)
  (let ((title)
        (content (url-retrieve-synchronously url nil nil timeout)))
    (if (not content)
        (error "Failed to retrieve URL content")
      (with-current-buffer content
        (goto-char (point-min))
        (re-search-forward "<title>\\([^<]*\\)</title>" nil t 1)
        (setq title (match-string 1))
        (goto-char (point-min))
        (re-search-forward "charset=\\([-0-9a-zA-Z]*\\)" nil t 1)
        (if title
            (decode-coding-string
             title
             'utf-8
             ;; (pcase (match-string 1)
             ;;   ;; Safeguard against CAPS in names, maybe other weird coding
             ;;   ;; schemes.
             ;;   ("UTF-8" 'utf-8)
             ;;   (_ (intern (match-string 1))))
             )
          (url-filename (url-generic-parse-url url)))))))

;; (www-get-page-title "https://www.studyinjapan.go.jp/en/planning/about-scholarship/")


(defun hax/org-insert-clipboard-link (&optional description)
  ;; FIXME https://astralcodexten.substack.com/p/heuristics-that-almost-always-work?s=r
  (interactive)
  (let ((run-again t))
    (while run-again
      (let* ((link (simpleclip-get-contents))
             (name (if description description
                     (when link (www-get-page-title link))))
             (org-link (format "[[%s][%s]]" link name))
             (read-answer-short t)
             (selected
              (if name
                  (read-answer
                   (format "Insert '%s" org-link)
                   '(("yes" ?y "insert link")
                     ("no" ?n "do not insert link")
                     ("update" ?u "update link from clipboard")
                     ("help" ?h "show help")
                     ("edit" ?e "prompt for new link name")
                     ("quit" ?q "quit")))
                (warn "Bad url: %s" link)
                (setq run-again nil)
                nil)))
        (setq description name)
        (cond
         ((string= selected "yes") (insert org-link) (setq run-again nil))
         ((string= selected "no") (setq run-again nil))
         ((string= selected "edit") (setq description
                                          (read-string "New link name: ")))
         ((string= selected "update") t)
         ((string= selected "help") (setq run-again nil))
         ((string= selected "quit") (setq run-again nil)))))))

(defun org-wrapping-subtree (&optional tree)
  (if (eq 'headline (car tree))
      tree
    (org-wrapping-subtree (org-element-property :parent tree))))

(defun hax/org-insert-footnote (footnote)
  (interactive "sfootnote name: ")
  (insert (format "[fn:%s]" footnote))
  (goto-char (org-element-property
              :end (org-wrapping-subtree (org-element-at-point))))
  (insert (format "[fn:%s] " footnote))
  (evil-insert-state)
  (save-excursion (insert "\n\n")))

(defun hax/org-gen-footnote-name ()
  (interactive)
  (point)
  (format
   "%s-%s"
   (s-replace-regexp
    (rx (not word))
    ""
    (s-trim (buffer-substring (point) (save-excursion (backward-word) (point)))))
   (line-number-at-pos)))

(defun hax/org-insert-footnote-link (footnote)
  (interactive "sfootnote name: ")
  (insert (format "[fn:%s]" footnote))
  (let* ((at (org-element-at-point))
         (tree (if (eq 'headline (car at)) at (org-element-property :parent at))))
    (save-excursion
      (goto-char (org-element-property :end tree))
      (insert (format "\n[fn:%s] " footnote))
      (hax/org-insert-clipboard-link)
      (insert "\n\n"))))

(defvar hax/org-refile-refiled-from-id nil)
(defvar hax/org-refile-refiled-from-header nil)


(defun hax/org-subtree-timestamp (arg &optional inactive)
  "Insert active timestamp in current subtree"
  ;; TODO Implement insertion of the time ranges
  ;; MAYBE Check for conflicts with 'DEADLINE' and 'SCHEDULED'
  (interactive "P")
  (save-excursion
    (org-back-to-heading)
    (let* ((old (org-entry-get nil "TIMESTAMP"))
           (tree (org-element-at-point))
           (dedl (org-entry-get nil "DEADLINE"))
           (shed (org-entry-get nil "SCHEDULED"))
           (lvl (org-element-property :level tree)))
      (if old
          (error "FIXME - implement update of the existing dates")
        (let ((time (org-read-date arg 'totime)))
          (next-line)
          (insert (s-repeat (1+ lvl) " "))
          (insert "TIMESTAMP: ")
          (org-insert-time-stamp time (or org-time-was-given arg) inactive)
          (if (not (or dedl shed))
              (insert "\n")))))))

;; (defvar hax/internal::org-refile-refiled-from-params nil "Parameters of ")

(defun hax/org-save-source-id-and-header ()
  "Saves refile's source entry's id and header name to
`hax/org-refile-refiled-from-id' and
`hax/org-refile-refiled-from-header'. If refiling entry is
first level entry then it stores file path and buffer name
respectively."
  (interactive)
  (save-excursion
    (if (org-up-heading-safe)
        ;; If we are in some outline, assign
        (progn
          ;; Store original node ID, optionally creating it if missing
          (setq hax/org-refile-refiled-from-id (cons 'id (org-id-get-create)))
          ;; Get original heading path, without tags, todo, priority elements
          (setq hax/org-refile-refiled-from-header
                (counsel-org-goto-all--outline-path-prefix)))
      (setq hax/org-refile-refiled-from-id
            (cons 'file (buffer-file-name)))
      (setq hax/org-refile-refiled-from-header (buffer-name)))))

;; IDEA add more complex log entries too: make this function accept
;; org-mode subtree in elisp form (parsed from org-elements)
(defun org-add-log-entry (text)
  "Add log entry for current subtree"
  (save-excursion
    (goto-char (- (org-log-beginning t) 1))
    (insert (concat "\n" (s-repeat (current-indentation) " ") "- " text))))

(defun hax/org-refile-add-refiled-from-note ()
  "Adds a note to entry at point on where the entry was refiled
from using the org ID from `hax/org-refile-refiled-from-id'
and `hax/org-refile-refiled-from-header' variables."
  (interactive)
  (when (and hax/org-refile-refiled-from-id
             hax/org-refile-refiled-from-header)
    (let* ((time-format (substring (cdr org-time-stamp-formats) 1 -1))
           (kind (car hax/org-refile-refiled-from-id))
           (src (cdr hax/org-refile-refiled-from-id))
           (time-stamp (format-time-string time-format (current-time))))
      (org-add-log-entry
       (format
        "Refiled on [%s] from [[%s][%s]]"
        time-stamp
        (if (eq kind 'id) (format "id:%s" src)
          (format "file:%s" (f-relative src (f-dirname (buffer-file-name)))))
        hax/org-refile-refiled-from-header)))
    (setq hax/org-refile-refiled-from-id nil)
    (setq hax/org-refile-refiled-from-header nil)))

(add-hook
 'org-after-refile-insert-hook #'hax/org-refile-add-refiled-from-note)

(advice-add
 'org-refile :before #'hax/org-save-source-id-and-header)



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
  ;; (highlight-indent-guides-mode -1)


  ;; https://github.com/hlissner/doom-emacs/blob/develop/docs/faq.org#my-new-keybinds-dont-work
  ;; because I override the default keybindings I had to use this
  ;; abomination of a `map!' call to do what I need.
  (map!
   :map org-mode-map
   [C-S-return] nil
   [C-return] nil
   [M-return] nil
   )



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
   :map org-mode-map
   :localleader
   :nv "dt" #'hax/org-subtree-timestamp
   )

  (map!
   :map evil-org-mode-map
   ;; Consistent multicursor bindings are more important for me, so
   ;; `org-shiftdown' and `org-shiftup' can go elsewhere.
   :ni "C-S-j" nil
   :ni "C-S-k" nil

   :ni "C-;" #'flyspell-correct-wrapper
   :nvi [M-return] #'org-add-note
   :ni [C-S-return] #'hax/org-insert-todo-entry
   :ni [C-return] #'+org/insert-item-below

   :nv ",#" #'hax/org-update-all-cookies
   :nv ",eh" 'org-html-export-to-html
   :desc "Async export to pdf"
   :nv ",ep" (cmd! (org-latex-export-to-pdf t))
   :nv ",ea" 'org-ascii-export-to-ascii
   :nv ",eP" 'org-latex-export-to-pdf
   :nv ",el" 'org-latex-export-to-latex
   :nv ",eo" 'org-odt-export-to-odt
   :nv ",eh" 'org-html-export-to-html
   :nv ",in" #'org-add-note
   :n ",tc" #'org-toggle-checkbox
   :n ",ta" #'hax/org-assign-tag
   :ni "M-i M-i" #'hax/org-paste-clipboard
   :ni "M-i M-S-i" (cmd! (insert "#+capion: ")
                         (save-excursion (hax/org-paste-clipboard)))

   :desc "link to word"
   :v "M-i M-l M-w" (cmd! (let ((text (get-selected-region-text)))
                            (delete-region (get-selected-region-start) (get-selected-region-end))
                            (insert (format "[[%s][%s]]" text text))))

   :desc "link to subtree"
   :v "M-i M-l M-t" (cmd! (let ((text (get-selected-region-text)))
                            (delete-region (get-selected-region-start) (get-selected-region-end))
                            (hax/org-insert-clipboard-link text)))
   :desc "link to clipboard"
   :v "M-i M-l M-l" (cmd! (let ((text (get-selected-region-text)))
                            (delete-region (get-selected-region-start) (get-selected-region-end))
                            (hax/org-insert-clipboard-link text)))

   :ni "M-i M-l M-t" #'hax/org-insert-link-to-subtree
   :ni "M-i M-l M-l" #'hax/org-insert-clipboard-link
   :ni "M-i M-l M-d" (lambda (description)
                       (interactive "sLink description: ")
                       (hax/org-insert-clipboard-link description))

   :ni "M-i M-l M-f" (cmd! (hax/org-insert-footnote-link
                            (hax/org-gen-footnote-name)))
   :ni "M-i M-f" (cmd! (hax/org-insert-footnote
                        (hax/org-gen-footnote-name)))
   :desc "insert {{{macro}}}"
   :ni "M-i M-{" (cmd! (insert "{{{") (save-excursion (insert "}}}")))
   :desc "insert #+begin_src"
   :n ",ic" (cmd!
             (evil-insert-state)
             (yas-expand-snippet "#+begin_src $1\n$0\n#+end_src"))
   :nv ",hi" #'org-indent-mode
   :nv ",ci" #'org-clock-in
   :nv ",co" #'org-clock-out
   :desc "start WIP clocking"
   :nv ",cw" (cmd! (org-todo "WIP") (org-clock-in)))



  (setq company-backends
        '(company-capf (:separate company-ispell company-dabbrev company-yasnippet))))

(map!
 ;; https://github.com/hlissner/doom-emacs/issues/3978#issuecomment-699004440
 ;; `:leader' key mapping is global, and I want to use `SPC-SPC' only for
 ;; org-mode. But local leader (`:localleader') is configured to be
 ;; `SPC-m', and I don't wnat to use this combinations (or change mnemnics
 ;; for every single other combination), so keybindings here are global,
 ;; with hack for specific goto configurations.
 ;;
 ;; Not specifying `:map' as `:leader' takes priority anyway.
 :leader
 :n "SPC" #'hax/replace-next-placeholder
 :nv "si" (cmd! (call-interactively
                 (cond
                  ((eq major-mode 'org-mode) 'counsel-org-goto)
                  (t 'counsel-imenu)))) )

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
(setq hax/fullscreen-client-name nil)
(defun hax/org-post-capture-hook ()
  (interactive)
  ;; If starting capture from dropdown mode, delete the client window frame
  ;; after. That won't work if it is the single client, but when launching
  ;; using daemon mode it does /exactly/ what is needed.
  (when hax/fullscreen-capture
    (delete-frame))
  ;; Fullscreen capture must be set again with new `--eval' argument
  ;; when starting next cature.
  (setq hax/fullscreen-capture nil)
  (setq hax/fullscreen-client-name nil))

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

(hax/defaccept! accept-distant-scheduled/deadlined
                (let* ((dead (org-element-property :deadline tree))
                       (shed (org-element-property :scheduled tree))
                       (now (time-to-days (current-time))))
                  (when (or dead shed)
                    (let* ((dead-days
                            (if dead (- (time-to-days (org-timestamp-to-time dead)) now) nil))
                           (shed-days
                            (if shed (- (time-to-days (org-timestamp-to-time shed)) now) nil)))
                      ;; (message "dead: %s shed: %s" dead-days shed-days)
                      (or (and dead-days (< 15 dead-days 90))
                          (and shed-days (< 15 shed-days 90)))))))

(setq
 org-agenda-custom-commands
 `(
   ("l" "Long"
    ((agenda
      ""
      ((org-agenda-span 90)
       (org-agenda-start-day "-7d")
       (org-deadline-warning-days 35)))))
   ("*" "All"
    ((agenda
      ""
      ((org-agenda-span 14)
       ;; Start showing events from today onwards, when quickly assessing
       ;; target tasks I don't really need to focus on the past events.
       (org-agenda-start-day "-0d")
       ;; I show planned and deadlined events for the next two weeks - no
       ;; need to repeat the same information again for today.
       (org-deadline-warning-days 0)))

     (todo "WIP")
     (todo "POSTPONED")
     ;; ;; Show all todo items in the agenda files.
     ;; (todo
     ;;  "TODO"
     ;;  ((org-agenda-skip-function 'skip-scheduled/deadlined)
     ;;   (org-agenda-sorting-strategy '((priority-down)))))
     ))))

(defun hax/parent-subtrees()
  "test"
  (let* ((len 18)
         (str (s-pad-left len " " (s-join "." (org-get-outline-path)))))
    (substring str (- (length str) len) (length str))))

(setq
 org-agenda-prefix-format '((agenda . " %i %-12t%-12s %(hax/parent-subtrees) ")
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


;; Store capture location *before* the capture happens, this way location
;; information is reported to the hook properly.
(defvar hax/org-capture-from-line nil)
(defun hax/org-pre-capture-hook (&optional fun args)
  (interactive)
  (when (and fun args)
    (setq hax/org-capture-from-line (line-number-at-pos))))

(advice-add 'org-capture :before #'hax/org-pre-capture-hook)


(defun hax/capture-location ()
  "Get formatted string about capture location"
  (let ((orig (org-capture-get :original-file)))
    (message "[%s]" hax/fullscreen-client-name)
    (if (magit-toplevel)
        (let* ((top (f-filename (magit-toplevel)))
               (file (magit-file-relative-name orig)))
          (format "from ~%s:%s:%s~" top file hax/org-capture-from-line))
      (if orig
          (format "from ~%s:%s~" (f-filename orig) hax/org-capture-from-line)
        (format "from ~%s~" (s-replace "~" "∼" hax/fullscreen-client-name))))))

(after!
  org
  (require 'org-expiry)
  (defun org-odt-inline-src-block (_inline-src-block _contents _info)
    "Transcode an INLINE-SRC-BLOCK element from Org to ODT.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
    (format "<text:span text:style-name=\"%s\">%s</text:span>"
            "OrgCode" (org-odt--encode-plain-text
                       (org-element-property :value _inline-src-block))))

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
        ;; agenda just includes everything that contains an "active time
        ;; stamp". An active time stamp is any time stamp in angular
        ;; brackets. That's why all templates use `%U' instead of `%T' for
        ;; timestamps - creation date should not be inserted in the agenda.
        '(;; Add new entry to the inbox. No sorting, no hierarchical placement,
          ;; just dump everything in it, refile later.
          ("t" "GTD todo inbox" entry (file hax/inbox.org)
           "* TODO %?
  :PROPERTIES:
  :CREATED: %U
  :ID: %(org-id-new)
  :ORIGIN: %(hax/capture-location)
  :END:
"
           :empty-lines-before 1
           :empty-lines-after 1)
          ("d" "Daily" entry (file+olp+datetree hax/notes.org)
           "** %U %(hax/capture-location)
  :PROPERTIES:
  :CREATED: %U
  :ID: %(org-id-new)
  :END:

%?"
           :empty-lines-before 1
           :empty-lines-after 1)
          ("i" "Immediate" entry (file+olp+datetree hax/immediate.org)
           "* TODO %?
  :PROPERTIES:
  :CREATED: %U
  :ID: %(org-id-new)
  :ORIGIN: %(hax/capture-location)
  :END:
"
           :empty-lines-after 1
           :empty-lines-before 1)
          ("D" "Daily start" plain (file+olp+datetree hax/notes.org)
           "%i%?"
           :empty-lines-before 1
           :empty-lines-after 1)
          ("c" "Clock" entry (clock)
           "** %U %(hax/capture-location)
  :PROPERTIES:
  :CREATED: %U
  :ID: %(org-id-new)
  :END:

%?"
           :empty-lines-before 1
           :empty-lines-after 1)))

  (setq
   ;; Main notes directory
   org-directory "~/defaultdirs/notes/personal"
   ;; File with locations of the org-id entries
   org-id-locations-file (f-join org-directory ".org-id-locations")
   ;; Directory for todo management
   hax/todo.d (f-join org-directory "todo")
   ;; GTD inbox
   hax/inbox.org (f-join hax/todo.d "inbox.org")
   ;; Immediate plans for today. Similar to GTD inbox, but reserved for
   ;; very minor items used to organize the thoughts. Items from here can
   ;; duplicate others (heading might be a direct link), and they are not
   ;; refiled anywhere else.
   hax/immediate.org (f-join hax/todo.d "immediate.org")
   ;; Main GTD organizer
   hax/main.org (f-join hax/todo.d "main.org")
   ;; Random junk notes that I generate, copy from other places etc.
   hax/notes.org (f-join hax/todo.d "notes.org")
   hax/fic.org (f-join hax/todo.d "fic.org")
   ;; Project configuration
   hax/projects.org (f-join hax/todo.d "projects.org")
   ;; Agenda is a main todo file and inbox
   org-agenda-files (list hax/main.org hax/inbox.org hax/immediate.org)
   org-refile-targets `((nil :maxlevel . 3)
                        (,hax/fic.org :maxlevel . 4)
                        (,hax/main.org :maxlevel . 3)
                        (,hax/projects.org :maxlevel . 3)
                        (,hax/inbox.org :maxlevel . 2))

   ;; Add `COMPLETED' timestamp property
   org-log-done 'time
   ;; Log when schedule changed
   org-log-reschedule 'time
   ;; Notes should go from top to bottom
   org-log-states-order-reversed nil
   ;; Log when deadline changed
   org-log-redeadline 'time
   ;; Not using default configuration - instead more informative version is
   ;; implemented using custom hooks.
   org-log-refile nil
   ;; This looks nice but has a lot of random glitches on this
   ;; configuration
   org-startup-indented nil
   ;; Start week on monday like any normal human being
   calendar-week-start-day 1
   ;; Insert newlines before heading, but plain lists should be packed
   ;; together.
   org-blank-before-new-entry '((heading . t) (plain-list-item . nil))
   org-hierarchical-todo-statistics t
   org-goto-interface 'outline-path-completionp
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
           "MAYBE(m!)"          ;; Not Guaranteed to happen
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
        `(("TODO" . "orange")
          ("LATER" . "DeepPink")
          ("NEXT" . "DarkRed")
          ("POSTPONED" . "coral")
          ("DONE" . ,(doom-color 'green))
          ("MAYBE" . ,(doom-color 'blue))
          ("NUKED" . ,(doom-color 'purple))
          ("CANCELED" . "snow")
          ("PARTIALLY" . "goldenrod")
          ("WIP" . "tan")
          ("STALLED" . "gold")
          ("REVIEW" . "SteelBlue")
          ("FAILED" . ,(doom-color 'red))
          ("FUCKING___DONE" . "gold1")
          ("TIMEOUT" . ,(doom-color 'red))))
  (setq hax/tags-file (f-join hax/todo.d "tags"))
  (setq org-tag-alist
        (concatenate
         'list
         '((:startgroup . nil)
           ("@work" . ?w)
           ("@home" . ?h)
           ("@univeristy" . ?u)
           ;; Personal code and other projects
           ("@projects" . ?p)
           ("@organization" . ?o)
           ("@errands" . ?e)
           (:endgroup . nil))
         (--map
          `(,(substring it 1 (length it)) . ??)
          (--filter
           (< 0 (length it))
           (s-split "\n" (f-read hax/tags-file)))))))

(setq
 org-odt-category-map-alist
 '(("__Table__" "Figure" "value" "Таблица"
    org-odt--enumerable-p)
   ("__Figure__" "Figure" "value" "Рисунок"
    org-odt--enumerable-image-p)
   ("__MathFormula__" "Text" "math-formula" "Equation"
    org-odt--enumerable-formula-p)
   ("__DvipngImage__" "Equation" "value" "Equation"
    org-odt--enumerable-latex-image-p)
   ("__Listing__" "Listing" "value" "Листинг"
    org-odt--enumerable-p)))

(setq
 org-odt-transform-processes
 '(("Optimize Column Width of all Tables"
    "soffice" "--norestore" "--invisible" "--headless"
    "macro:///OrgMode.Utilities.OptimizeColumnWidth(%I)")
   ("Update All"
    "soffice" "--norestore" "--invisible" "--headless"
    "macro:///OrgMode.Utilities.UpdateAll(%I)")
   ("Reload"
    "soffice" "--norestore" "--invisible" "--headless"
    "macro:///OrgMode.Utilities.Reload(%I)")))

(setq org-odt--entry-template "
      <text:table-of-content-entry-template text:outline-level=\"%d\" text:style-name=\"Contents_20_%d\">
       <text:index-entry-link-start text:style-name=\"Internet_20_link\"/>
       <text:index-entry-chapter/>
       <text:index-entry-text/>
       <text:index-entry-tab-stop style:type=\"right\" style:leader-char=\".\"/>
       <text:index-entry-page-number />
       <text:index-entry-link-end/>
      </text:table-of-content-entry-template>\n")

(defun org-odt--format-toc (title entries depth)
  "Return a table of contents.
TITLE is the title of the table, as a string, or nil.  ENTRIES is
the contents of the table, as a string.  DEPTH is an integer
specifying the depth of the table."
  (concat
   "
<text:table-of-content text:style-name=\"OrgIndexSection\" text:protected=\"true\" text:name=\"Table of Contents\">\n"
   (format "  <text:table-of-content-source text:outline-level=\"%d\">" depth)
   (and title
        (format "
    <text:index-title-template text:style-name=\"Contents_20_Heading\">%s</text:index-title-template>
"
                title))

   (let ((levels (number-sequence 1 10)))
     (mapconcat
      (lambda (level)
        (format org-odt--entry-template level level)) levels ""))
   "
  </text:table-of-content-source>
  <text:index-body>"
   (and title
        (format "
    <text:index-title text:style-name=\"Sect1\" text:name=\"Table of Contents1_Head\">
      <text:p text:style-name=\"Contents_20_Heading\">%s</text:p>
    </text:index-title>\n"
                title))
   entries
   "
  </text:index-body>
</text:table-of-content>"))

(after! org-special-block-extras
  (o-defblock
   parallel (cols 2) (bar nil)
   (format "[[<%s>]]" contents)))
