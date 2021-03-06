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
  (setq org-expiry-inactive-timestamps t)
  (org-expiry-insert-created)
  (evil-insert-state)
  (hax/org-update-all-cookies)
  (org-todo "TODO"))

(defun hax/popup-mode-hook ()
  (interactive)
  (message "Evil insertt state triggered"))

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
             (if buf (buffer-file-name buf)) (f-join hax/indexed.d "images")))))
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


(load! "lang-org-tags.el")
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
                    org-tag-alist))
           (selected (ivy-read (counsel-org-tag-prompt)
                               (lambda (str _pred _action)
                                 (delete-dups
                                  (all-completions
                                   str #'org-tags-completion-function)))
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

(defun hax/maybe-numeric-prefix (else)
  "Return optional numeric prefix, if one was supplied for current
interactive function call"
  (if (and current-prefix-arg (not (consp current-prefix-arg)))
      (prefix-numeric-value current-prefix-arg)
    else))

(defun hax/?? (&optional last-n)
  (message "[%s]" last-n))

(map! :n "M-s-d" (cmd! (hax/?? (hax/maybe-numeric-prefix))))

(defun org-collect-known-entries ()
  (let (entries)
    (dolist (b (buffer-list))
      (with-current-buffer b
        (when (derived-mode-p 'org-mode)
          (setq entries
                (nconc entries
                       (counsel-outline-candidates
                        (cdr (assq 'org-mode counsel-outline-settings))
                        (counsel-org-goto-all--outline-path-prefix)))))))
    entries))


;; (advice-add
;;  'counsel-outline-candidates
;;  :filter-return #'hax/org-add-filename-to-counsel-outline-candidates)


(defun hax/org-collect-active-entries (&optional with-todo)
  "Get list of the 'targetable' entries - WIP todo items, ones that
have active timestamp in the -7/+7 day range, or ones that were
created today."
  (--map
   ;; it
   (let* ((marker (org-element-property :org-marker it))
          (outline (hax/org-outline-path-at-marker marker)))
     (cons
      (if with-todo
          (format "%s %s %s"
                  ;; TODO make buffer file name optional formatting parameter
                  (f-base (buffer-file-name (marker-buffer marker)))
                  (org-element-property :todo-keyword it)
                  outline)
        outline)
      (org-element-property :org-marker it)))
   (let ((begin-7 (ts-adjust 'day -7 (ts-now)))
         (end+7 (ts-adjust 'day +7 (ts-now)))
         (begin-1 (ts-adjust 'day -1 (ts-now)))
         (end+1 (ts-adjust 'day +1 (ts-now))))
     (org-ql-select (org-agenda-files)
       `(or
         (todo "POSTPONED" "WIP" "NEXT")
         (and (todo "TODO")
              (or
               ;; Explicitly marked as an inbox entry
               (tags "@inbox")
               ;; track activity in daily notes for one week before and after
               (and (path "notes.org$") (ts :from ,begin-7 :to ,end+7))
               ;; Track general planning in 14-day frame
               (planning :from ,begin-7 :to ,end+7)
               ;; Track any entries that were created/stamped today
               (ts :on today))))
       :action 'element-with-markers))))

(cl-defun hax/org-select-subtree-callback
    (prompt callback caller &optional (entries (org-collect-known-entries)))
  "Select subtree from ENTRIES and execute CALLBACK on the
selection result. Provide PROMPT for selection input"
  (interactive)
  (let (result)
    (ivy-read
     prompt
     entries
     :history 'counsel-org-goto-history
     :action callback)))

(cl-defun hax/org-select-subtree (&optional (entries (org-collect-known-entries)))
  "Interactively select subtree and return cons with `(description . marker)'"
  (interactive)
  (let (result)
    (hax/org-select-subtree-callback
     "Select: "
     ;; IMPORTANT in order for this hack to work, lexical binding must be
     ;; enabled.
     (lambda (x) (setq result x))
     'hax/org-select-subtree
     entries)
    result))

(defun goto-marker (marker)
  (switch-to-buffer (marker-buffer marker))
  (goto-char (marker-position marker)))

(defun hax/org-action-interactively (action &optional select-all)
  "Execution ACTION (function with no arguments) on the selected subtree position"
  (save-excursion
    (goto-marker (cdr (hax/org-select-subtree
                       (if select-all
                           (org-collect-known-entries)
                         (hax/org-collect-active-entries t)))))
    (funcall action)))

(defun hax/org-clock-in-interactively ()
  "Interactively select target to clock in using
`hax/org-collect-active-entries'"
  (interactive)
  (save-excursion
    (hax/org-action-interactively
     (lambda () (org-clock-in) (org-todo "WIP")))))

(map!
 :leader
 :n "oai" #'hax/org-clock-in-interactively
 :n "oao" #'org-clock-out
 :desc "Complete current clock"
 :n "oac" (cmd! (save-excursion
                  (org-clock-goto)
                  (org-clock-out)
                  (org-todo "COMPLETED")))
 :n "oag" #'hax/org-goto-select-active-subtree
 :n "oaC" #'hax/org-complete-interactively)

(cl-defun hax/org-complete-interactively (&optional (state "COMPLETED"))
  "Interactively select target to complete using
`hax/org-collect-active-entries'"
  (interactive)
  (save-excursion
    (hax/org-action-interactively (lambda () (org-todo state)))))


(defun hax/org-goto-select-subtree ()
  "Interactively select subtree and return position of the marker for it"
  (interactive)
  (goto-marker (cdr (hax/org-select-subtree))))

(defun hax/org-goto-select-active-subtree ()
  "Interactively select active subtree and return position of the marker for it"
  (interactive)
  (goto-marker (cdr (hax/org-select-subtree
                     (hax/org-collect-active-entries t)))))

(defun hax/org-unformat-title (title)
  (s-replace-all '(("=" . "") ("*" . "") ("~" . "")) title))


(defun hax/org-outline-path-at-marker (marker &optional last-n with-tags cleanup-name-formatting)
  "Get formatted outline entry at position specified by MARKER"
  (with-current-buffer (marker-buffer marker)
    (save-excursion
      (goto-char (marker-position marker))
      ;; If full path is requested, return it
      ;; formatted directly, otherwise fall back to
      ;; default formatting logic.
      (let* ((tags (org-get-tags-string))
             (loc (-slice (org-get-outline-path t)
                          (if last-n (- 0 last-n) 0)))
             (path (org-format-outline-path loc 256)))
        (concat (if cleanup-name-formatting (hax/org-unformat-title path) path)
                (if with-tags tags ""))))))

(defun hax/insert-subtree-link-cb
    (tree-car description last-n with-tags cleanup-name-formatting)
  ;; If function is called with prefix value
  (let* ((name (hax/org-outline-path-at-marker
                (cdr tree-car)
                last-n
                with-tags
                cleanup-name-formatting))
         ;; Go to position of the found entry, get IT's id (creating
         ;; one if it is missing), and then insert result into the
         ;; final output
         (id (with-current-buffer (marker-buffer (cdr tree-car))
               ;; Save excursion to avoid moving cursor in
               ;; other buffers (or in the same buffer if
               ;; linking within one file)
               (save-excursion
                 (goto-char (marker-position (cdr tree-car)))
                 (org-id-get-create)))))
    (insert (format " [[id:%s][%s]]" id (if description description name)))))

(cl-defun hax/org-insert-link-to-subtree
    (&key description
          last-n
          (entries (org-collect-known-entries))
          (with-tags nil)
          (cleanup-name-formatting t))
  "Insert link to any subtree in any org file, using
  `[[path][ID]]' link. If DESCRIPTION is `nil', use the heading
  name, otherwise use description."
  (interactive)
  (hax/org-select-subtree-callback
   "Goto: "
   (lambda (x) (hax/insert-subtree-link-cb
                x description last-n with-tags cleanup-name-formatting))
   'hax/org-insert-link-to-heading
   entries))

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
  (if (or (eq 'headline (car tree)) (not tree))
      tree
    (org-wrapping-subtree (org-element-property :parent tree))))





(defun hax/org-before-logical-end ()
  (interactive)
  (let* ((wrap (org-wrapping-subtree (org-element-at-point)))
         (end (org-element-property :end wrap))
         (level (org-element-property :level wrap))
         (result (if end
                     (save-excursion
                       (if (re-search-forward
                            (rx-to-string `(and bol ,(s-repeat (+ 1 level) "*")))
                            end t)
                           (progn (backward-char (+ 1 level)) (point))
                         end))
                   (point-max)))
         (with-newline (save-excursion
                         (goto-char result)
                         (if (eq result (line-beginning-position))
                             result
                           (progn (insert "\n") (point))))))
    with-newline))

(defun hax/back-until-content ()
  "Move cursor backwards until a non-empty line is found"
  (while (eq (line-beginning-position) (point)) (backward-char))
  (forward-char))

(defun hax/back-to-first-footnote ()
  (interactive)
  (let ((wrap (org-wrapping-subtree (org-element-at-point))))
    (while (search-backward-regexp
            (rx bol "[fn:")
            (org-element-property :begin wrap) t))))

(cl-defun hax/org-insert-footnote (footnote &optional (goto-created t))
  (interactive "sfootnote name: ")
  (insert (format "[fn:%s]" footnote))
  (let* ((real-start (point)))
    (goto-char (hax/org-before-logical-end))
    (insert (format "[fn:%s] " footnote))
    (evil-insert-state)
    (save-excursion (insert "\n\n"))
    (when (not goto-created) (goto-char real-start))))

(defun hax/org-add-trailing-note ()
  (interactive)
  (goto-char (hax/org-before-logical-end))
  (backward-char)
  (hax/back-to-first-footnote)
  (hax/back-until-content)
  (insert (format "- %s " (hax/org-current-timestamp)))
  (save-excursion (insert "\n")))

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
  "Interactively insert trailing footnote and link with specified
  FOOTNOTE as a name"
  (interactive "sfootnote name: ")
  (insert (format "[fn:%s]" footnote))
  (let* ((at (org-element-at-point))
         (tree (if (eq 'headline (car at))
                   at
                 (org-element-property :parent at))))
    (save-excursion
      (goto-char (org-element-property :end tree))
      (insert (format "\n[fn:%s] " footnote))
      (hax/org-insert-clipboard-link)
      (insert "\n\n"))))

(defvar hax/org-refile-refiled-from-id nil)
(defvar hax/org-refile-refiled-from-header nil)
(defvar hax/org-refile-refiled-from-mark nil)
(defvar hax/org-refile-refiled-from-file nil)


(defun diary-day (daylist)
  (memq (calendar-day-of-week date) daylist))

(defun diary-block-d (year month d-before d-after)
  "Diary block that accepts humane time ranges - year if first,
  then month, they day. Like every other sane person would do -
  like ISO standard does."
  (diary-block month d-before year month d-after year))

(defun diary-block-m (year m-before m-after d-before d-after)
  "Diary block in the same year, but with different month and day.
  Also uses *STANDARD* time part arrangement, instead of the
  default org-mode abomination."
  (diary-block m-before d-before year m-after d-after year))

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

(defun org-is-completed-state (state)
  "Check if STATE is a complete org-mode todo state (todo entry has
                                                          been completed - DONE, CANCELLED etc.)"
  (and state
       (let* ((result nil))
         (catch 'done
           (dolist (line org-todo-keywords)
             (dolist (it line)
               (if (s-starts-with-p state it) (throw 'done t)
                 (when (s-equals-p it "|")
                   (setq result t)
                   (throw 'done t))))))
         result)))

(defun org-is-incomplete-state (state)
  "Check if STATE is an active org-mode todo state (todo entry has
                                                          not been completed yet - TODO, WIP, REVIEW etc.)"
  (and state
       (let ((result nil))
         (catch 'done
           (dolist (line org-todo-keywords)
             (dolist (it line)
               (if (s-starts-with-p state it) (progn (setq result t) (throw 'done t))
                 (when (s-equals-p it "|") (throw 'done))))))
         result)))

(defun org-at-active-timestamp ()
  "Check if currently at the active timestamp"
  (when (org-at-timestamp-p 'lax)
    (save-excursion
      (goto-char (match-beginning 0))
      ;; There is no dedicated function to check if current timestamp is
      ;; active or not, so using direct character checking here
      (eq (char-after) ?<))))

(defun org-toggle-timestamp-or-range-type ()
  "Change type of the org-mode timestamp under cursor"
  (interactive)
  (org-at-timestamp-p 'lax)
  (let* (;; Timestamp range checking uses regex and
         ;; implicitly fills in necessary data to use
         ;; for later matching. The same approach is
         ;; used in `org-toggle-timestamp-type', so I
         ;; hope it won't be replaced later on.
         (beg (match-beginning 0))
         (end (match-end 0))
         (active (org-at-active-timestamp)))
    (save-excursion
      (goto-char beg)
      ;; `<' or `[' at the start of the time range does not trigger
      ;; `org-at-date-range-p', so move one character forward here
      (forward-char)
      (if (org-at-date-range-p)
          (progn
            (org-toggle-timestamp-type)
            (goto-char end)
            (forward-char 4)
            (org-toggle-timestamp-type))
        (org-toggle-timestamp-type)))))

(defun hax/org-after-todo-change-hook ()
  "Execute after TODO change state in the org-mode tree"
  (interactive)
  (when (org-is-completed-state org-state)
    (save-excursion
      (org-back-to-heading)
      (let* ((tree (org-element-at-point))
             (old (org-entry-get nil "TIMESTAMP")))
        (when old
          ;; Search for active timestamp range
          (search-forward "TIMESTAMP")
          (if (search-forward "<")
              (org-toggle-timestamp-or-range-type)))))))

(add-hook 'org-after-todo-state-change-hook #'hax/org-after-todo-change-hook)

(defun hax/org-save-source-id-and-header ()
  "Saves refile's source entry's id and header name to
  `hax/org-refile-refiled-from-id' and
  `hax/org-refile-refiled-from-header'. If refiling entry is
  first level entry then it stores file path and buffer name
  respectively."
  (interactive)
  (save-excursion
    (setq hax/org-refile-refiled-from-mark (point-marker))
    (setq hax/org-refile-refiled-from-file (buffer-file-name))
    (if (org-up-heading-safe)
        ;; If we are in some outline, assign
        (progn
          ;; Store original node ID, optionally creating it if missing
          (setq hax/org-refile-refiled-from-id (cons 'id (org-id-get-create)))
          ;; Get original heading path, without tags, todo, priority elements
          (setq hax/org-refile-refiled-from-header
                (org-format-outline-path (org-get-outline-path t))))
      (setq hax/org-refile-refiled-from-id
            (cons 'file (buffer-file-name)))
      (setq hax/org-refile-refiled-from-header (buffer-name)))))

;; IDEA add more complex log entries too: make this function accept
;; org-mode subtree in elisp form (parsed from org-elements)
(defun org-add-log-entry (text)
  "Add log entry for current subtree"
  (save-excursion
    (goto-char (- (org-log-beginning t) 1))
    (let ((ind (current-indentation)))
      (dolist (line (s-lines text))
        (insert "\n")
        (indent-line-to ind)
        (insert line)))))

(defun hax/org-refile-add-refiled-from-note ()
  "Adds a note to entry at point on where the entry was refiled
  from using the org ID from `hax/org-refile-refiled-from-id'
  and `hax/org-refile-refiled-from-header' variables."
  (interactive)
  (save-excursion
    (when (and hax/org-refile-refiled-from-id
               hax/org-refile-refiled-from-header)
      (let* ((time-format (substring (cdr org-time-stamp-formats) 1 -1))
             (kind (car hax/org-refile-refiled-from-id))
             (src (cdr hax/org-refile-refiled-from-id))
             (time-stamp (format-time-string time-format (current-time))))
        (org-add-log-entry
         (format
          "- Refiled on [%s] from [[%s][%s:%s]]"
          time-stamp
          (if (eq kind 'id)
              (format "id:%s" src)
            (format "file:%s" (f-relative src (f-dirname (buffer-file-name)))))
          (f-base hax/org-refile-refiled-from-file)
          hax/org-refile-refiled-from-header)))
      (when hax/org-refile-refiled-from-mark
        (goto-marker hax/org-refile-refiled-from-mark)
        (setq hax/org-refile-refiled-from-mark nil))

      (setq hax/org-refile-refiled-from-file nil)
      (setq hax/org-refile-refiled-from-id nil)
      (setq hax/org-refile-refiled-from-header nil))))


(add-hook
 'org-after-refile-insert-hook
 #'hax/org-refile-add-refiled-from-note)

(advice-add
 'org-refile :before #'hax/org-save-source-id-and-header)


(defvar hax/org-src-use-full t
  "Org-src buffer edits should use full buffer, not small popup")

(defun hax/org-toggle-src-full ()
  (interactive)
  (setq hax/org-src-use-full (not hax/org-src-use-full)))

(set-popup-rule!
  (lambda (name action) (and (s-starts-with? "*Org Src" name) (not hax/org-src-use-full)))
  :size 0.5  :quit nil :select t :autosave t :modeline t :ttl nil)

(set-popup-rule!
  (lambda (name action) (and (s-starts-with? "*Org Src" name) hax/org-src-use-full))
  :ignore t)

(defun hax/org-current-timestamp ()
  "Return current time as formatted inactive org timestamp"
  (s-replace-all '(("<" . "[") (">" . "]"))
                 (ts-format (concat (cdr org-time-stamp-formats))
                            (ts-now))))

(defun hax/goto-list-end ()
  (interactive)
  (let* ((elt (org-element-at-point))
         (item (org-element-property :parent elt)))
    (when (eq (car item) 'item)
      (goto-char (org-element-property :end item)))))

(defun pop-selection ()
  (when (use-region-p)
    (let ((res (buffer-substring-no-properties (region-beginning) (region-end))))
      (delete-region (region-beginning) (region-end))
      res)))

(defun hax/org-mode-hook ()
  (interactive)
  ;; https://aliquote.org/post/enliven-your-emacs/ font-lock `prepend/append'
  ;; pair here is copied from this blog post, since I can't really figure out
  ;; what exactly is going on with ordering. But that implementation allows
  ;; me to override the default checkbox highlighting for checkboxes.

  (require 'evil-surround)
  (message "Org-mode hook executed")
  (hl-todo-mode 1)

  (push '(?$ . ("\\(" . "\\)")) evil-surround-pairs-alist)

  (setq flyspell-generic-check-word-predicate 'hax/flyspell-org-mode-verify)
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
   [M-return] #'hax/org-add-trailing-note
   [C-S-return] #'hax/org-insert-todo-entry
   [C-return] #'+org/insert-item-below)

  (hax/org-download-setup)
  (map!
   :map org-mode-map
   :localleader
   :nv "dt" #'hax/org-subtree-timestamp
   :desc "sort, todo state order"
   :nv "soo" #'hax/sort-subtree-contextually)

  (map!
   :map evil-org-mode-map
   ;; Consistent multicursor bindings are more important for me, so
   ;; `org-shiftdown' and `org-shiftup' can go elsewhere.
   :ni "C-S-j" nil
   :ni "C-S-k" nil

   :ni "C-;" #'flyspell-correct-wrapper
   :nvi [M-return] #'hax/org-add-trailing-note
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
   :n ",tc" (cmd! (when (org-in-item-p)
                    (save-excursion
                      (goto-char (org-in-item-p))
                      (org-toggle-checkbox))))
   :n ",ta" #'hax/org-assign-tag
   :ni "M-i M-i" #'hax/org-paste-clipboard
   :desc "math"
   :ni "M-i M-m" (lambda (text)
                   (interactive "sMath: ")
                   (insert (format "\\(%s\\)" text)))
   :desc "math"
   :v "M-i M-m" (cmd! (insert (format "\\(%s\\)" (pop-selection))))
   :desc "insert ~code~"
   :ni "M-i M-`" (cmd! (insert (format "~%s~" (read-string "Code: "))))
   :desc "insert src_code{}"
   :ni "M-i M-c M-s" (cmd!
                      (insert
                       (format
                        "src_%s{%s}"
                        (ivy-read "Language: "
                                  '("asm" "cpp" "sh")
                                  :history 'hax/insert-stc-code-history)
                        (read-string "Code: "))))
   :desc "separator"
   :ni "M-i M-s" (cmd! (insert
                        (format "%s\n" (s-repeat fill-column "-"))))
   :desc "timestamped element timestamp"
   :ni "M-i M-e M-t" (cmd! (insert (hax/org-current-timestamp)))
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

   :desc "link subtree, full name"
   :ni "M-i M-l M-t M-f" (cmd! (hax/org-insert-link-to-subtree))
   :desc "link subtree, short name"
   :ni "M-i M-l M-t M-s" (cmd! (hax/org-insert-link-to-subtree :last-n 1))
   :desc "subtree, only short name"
   :ni "M-i M-l M-t M-n" (cmd! (hax/org-select-subtree-callback
                                "Insert: "
                                (lambda (x) (insert (hax/org-outline-path-at-marker (cdr x) 1)))
                                'hax/org-insert-link-to-heading))

   :desc "link active subtree, full name"
   :ni "M-i M-l M-a" (cmd! (hax/org-insert-link-to-subtree
                            :entries (hax/org-collect-active-entries)))

   :ni "M-i M-l M-l" #'hax/org-insert-clipboard-link
   :ni "M-i M-l M-d" (lambda (description)
                       (interactive "sLink description: ")
                       (hax/org-insert-clipboard-link description))

   :ni "M-i M-l M-f" (cmd! (hax/org-insert-footnote-link (hax/org-gen-footnote-name)))
   :desc "footnote & prompt"
   :ni "M-i M-F" (cmd! (save-excursion
                         (hax/org-insert-footnote (hax/org-gen-footnote-name))
                         (insert (read-string "Footnote: "))
                         (org-fill-paragraph)))
   :desc "footnote & goto"
   :ni "M-i M-f" (cmd! (hax/org-insert-footnote (hax/org-gen-footnote-name)))
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
 :desc "New immediate todo"
 :n [M-f10] (cmd! (org-capture nil "i"))
 :desc "New note for clock"
 :n [M-f11] (cmd! (org-capture nil "c"))
 :desc "New capture"
 :n [M-insert] #'org-capture)

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



(defun hax/parent-subtrees()
  "test"
  (let* ((len 18)
         (str (s-pad-left len " " (s-join "." (org-get-outline-path)))))
    (substring str (- (length str) len) (length str))))

(defun hax/maybe-relative-time ()
  "Insert relative time (in hours) between current time and target
subtree deadline/sheduled/timestamp if any."
  (save-excursion
    ;; (with-no-warnings (defvar date) (defvar entry))
    ;; (message "%s" (calendar-absolute-from-gregorian date))

    (if (org-current-level)
        (let* ((time (org-entry-get nil "TIMESTAMP"))
               (dead (org-entry-get nil "DEADLINE"))
               (shed (org-entry-get nil "SCHEDULED")))
          (if (or time dead shed)
              ;; TODO Ensure prioritization of the deadline/sheduled time
              ;; happens properly.
              (let* ((parsed (ts-parse-org (or dead shed time)))
                     (time-diff (ts-difference parsed (ts-now))))
                (if (< -99 (/ time-diff 3600))
                    (format "%03d%s"
                            (/ time-diff 3600)
                            (hax/closest-unicode-fraction
                             (hax/relative-hour-fraction time-diff)))
                  "    "))
            "    "))
      "    ")))

(setq
 org-agenda-prefix-format '(;; For regular agenda items, show (?whatever?)
                            ;; first, then align time to five characters,
                            ;; then 12 for scheduled information. Title and
                            ;; all the other data will be placed afterwards.
                            (agenda . "%(hax/maybe-relative-time) %-12t ")
                            (todo . "%(hax/parent-subtrees) ")
                            (tags . "%(hax/parent-subtrees) ")
                            (search . "%(hax/parent-subtrees) "))
 org-agenda-start-on-weekday nil
 org-agenda-ndays 14
 org-agenda-show-all-dates t
 org-agenda-skip-deadline-if-done t
 org-agenda-skip-scheduled-if-done t
 org-agenda-repeating-timestamp-show-all nil
 org-deadline-warning-days 14
 org-agenda-time-grid '(;; Unconditionally show time grid for today
                        (daily today)
                        ;; Show data for 8, 10, 12 hours etc.
                        (800 1000 1200 1400 1600 1800 2000)
                        ;; No trailing dots after hour
                        ""
                        ;; Unconditionally show time grid for today
                        "")
 org-agenda-start-day "-0d")


;; Store capture location *before* the capture happens, this way location
;; information is reported to the hook properly.
(defvar hax/org-capture-from-line nil)
(defun hax/org-pre-capture-hook (&optional fun args)
  (interactive)
  (setq hax/org-capture-from-line (line-number-at-pos)))

(advice-add 'org-capture :before #'hax/org-pre-capture-hook)

(require 'uuidgen)

(defun hax/capture-location ()
  "Get formatted string about capture location"
  (let ((orig (org-capture-get :original-file))
        (uuid
         (substring
          (s-replace "-" "" (s-upcase (uuidgen-4))) 0 8)))
    (message "[%s]" hax/fullscreen-client-name)
    (if (magit-toplevel)
        (let* ((top (f-filename (magit-toplevel)))
               (file (magit-file-relative-name orig)))
          (format
           "from ~%s:%s:%s~ (=%s=)"
           top file hax/org-capture-from-line uuid))
      (if orig
          (format
           "from ~%s:%s~ (=%s=)"
           (f-filename orig) hax/org-capture-from-line uuid)
        (format
         "from ~%s~ (=%s=)"
         (s-replace "~" "???" hax/fullscreen-client-name) uuid)))))

(defun org-odt-inline-src-block (_inline-src-block _contents _info)
  "Transcode an INLINE-SRC-BLOCK element from Org to ODT.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  (format "<text:span text:style-name=\"%s\">%s</text:span>"
          "OrgCode" (org-odt--encode-plain-text
                     (org-element-property :value _inline-src-block))))

(progn
  (cl-defun org-note-insert-at-time+date
      (text second minute hour day month year &optional (tags '()))

    (let ((org-overriding-default-time
           (encode-time second minute hour day month year)))
      (setq hax/immediate-note-tags tags)
      (setq hax/immediate-note-content text)
      (org-capture nil "@")
      (setq hax/immediate-note-content "")
      (setq hax/immediate-note-tags '())))

  (defun hax/tg-extract-date (msg)
    (with-temp-buffer
      (insert msg)
      (goto-char 0)
      (re-search-forward (rx "["
                             (group (1+ digit)) "/" ;; 1 month
                             (group (1+ digit)) "/" ;; 2 day
                             (group (1+ digit)) " " ;; 3 year
                             (group (1+ digit)) ":" ;; 4 hour
                             (group (1+ digit)) " " ;; 5 minute
                             (group (| "PM" "AM"))  ;; 6 AM/PM
                             "]"))
      (let ((hour (string-to-number (match-string 4))))
        (list
         ;; sec
         0
         ;; min
         (string-to-number (match-string 5))
         ;; hour
         (if (string= (match-string 6) "AM") (+ hour 12) hour)
         ;; day
         (string-to-number (match-string 2))
         ;; mon
         (string-to-number (match-string 1))
         ;; year
         (+ 2000 (string-to-number (match-string 3)))))))

  (defun hax/tg-extract-tags (msg)
    (with-temp-buffer
      (insert msg)
      (goto-char 0)
      (-distinct (matches-in-buffer
                  (rx "#" (group (1+ (| word "_" "#"))))
                  (lambda () (match-string 1))))))
  (defun hax/tg-insert-note (msg)
    (interactive)
    (pcase (hax/tg-extract-date msg)
      (`(,sec ,min ,hour ,day ,month ,year)
       (message
        "Inserted note with sec:%s min:%s hour:%s day:%s month:%s year:%s"
        sec min hour day month year)
       ;; (calendar-gregorian-from-absolute
       ;;  (time-to-days (encode-time sec min hour day month year))
       ;;  )
       (org-note-insert-at-time+date
        (s-delete-lines (s-trim msg) 1) sec min hour day month year
        (append '("from_tg") (hax/tg-extract-tags msg)))
       )))

  (defun hax/tg-insert-selected-note (beginning end)
    (interactive "r")
    (hax/tg-insert-note (buffer-substring beginning end))
    (kill-region beginning end)
    (message (propertize "inserted note" 'face
                         `(:foreground ,(doom-color 'red))))))

(defun org-coords-open (path _)
  "Open org-mode link with coordinates"
  (browse-url (format "https://www.openstreetmap.org/#map=16/%s" path)))

(defun hax/org-end-of-the-day-stamp ()
  (format-time-string "<%Y-%m-%d %a 23:59:59>"))

(require 'f)

(defvar org-roam-db-update-queue
  (list) "List of the files for delayed org-roam update")

(defun org-roam-db-schedule-update-file (&optional file-path)
  ;; do same logic as original to determine current file-path if not
  ;; passed as arg
  (setq file-path (or file-path (buffer-file-name (buffer-base-buffer))))
  (with-temp-buffer
    (find-file file-path)
    (if (-contains-p (org-get-tags) "DO_NOT_ORG_ROAM")
        (message "org-roam: skipping update of %s" file-path)
      (progn (message "org-roam: scheduling update of %s" file-path)
             (if (not (memq file-path org-roam-db-update-queue))
                 (push file-path org-roam-db-update-queue))))))

;; this function will be called when emacs is idle for a few seconds
(defun org-roam-db-idle-update-files ()
  ;; go through queued filenames one-by-one and update db
  ;; if we're not idle anymore, stop. will get rest of queue next idle.
  (while (and org-roam-db-update-queue (current-idle-time))
    (let ((file (pop org-roam-db-update-queue)))
      (message "org-roam: running update of %s" file)
      ;; apply takes function var and list
      (org-roam-db-update-file file))))

;; we'll only start updating db if we've been idle for this many seconds
(run-with-idle-timer 5 t #'org-roam-db-idle-update-files)

(defun hax/org-after-save ()
  (when (derived-mode-p 'org-mode)
    (org-roam-db-schedule-update-file (buffer-file-name))))

(add-hook! 'after-save-hook #'hax/org-after-save)



(setq
 org-highlight-latex-and-related '(latex script entities)
 ;; Main notes directory
 org-directory "~/defaultdirs/notes/personal"
 ;; Do not fontify super/sub-scripts differently from the rest of the text.
 tex-fontify-script nil
 ;; File with locations of the org-id entries
 org-id-locations-file (f-join org-directory ".org-id-locations")
 ;; Directory for todo management and other indexed entries
 hax/indexed.d (f-join org-directory "indexed")
 ;; Directory to store org-roam topic files. Because I can both write
 ;; standalone conceptual notes and reference them in daily captures I see
 ;; no reason to separate those two.
 org-roam-directory hax/indexed.d
 ;; I implemented custom update on save + idle refresh, so no need to use
 ;; built-in implementation
 org-roam-db-update-on-save nil
 ;; Main index file for org-roamm
 org-roam-index-file (f-join org-roam-directory "roam-index.org")
 ;; Allow exclusion of certain singular files that are tagged with
 ;; `DO_NOT_ORG_ROAM' filetag.
 org-roam-db-node-include-function
 (lambda () (if (member "DO_NOT_ORG_ROAM" (org-get-tags)) nil t))
 ;; Remove all formatting from titles - bold, italic, verbatim etc. are not
 ;; shown in the roam UI anyway, and I mostly view the graph/database using
 ;; it.
 org-roam-node-formatter (lambda (node) (hax/org-unformat-title
                                         (org-roam-node-title node)))
 ;; GTD inbox
 hax/inbox.org (f-join hax/indexed.d "inbox.org")
 ;; Main GTD organizer
 hax/main.org (f-join hax/indexed.d "main.org")
 ;; Random junk notes that I generate, copy from other places etc.
 hax/notes.org (f-join hax/indexed.d "notes.org")
 hax/fic.org (f-join hax/indexed.d "fic.org")
 ;; Project configuration
 hax/projects.org (f-join hax/indexed.d "projects.org")
 org-structure-template-alist '(("f" . "formula\n")
                                ("a" . "export ascii\n")
                                ("d" . "definition\n")
                                ("c" . "center\n")
                                ("C" . "comment\n")
                                ("e" . "example\n")
                                ("E" . "export\n")
                                ("h" . "export html\n")
                                ("l" . "export latex\n")
                                ("q" . "quote\n")
                                ("s" . "src\n")
                                ("v" . "verse\n")))

(defun hax/org-mode-configure()
  (interactive)
  ;; Default inline latex highlighting is a bold white text, which is too
  ;; similar to a regular text.
  (set-face-attribute
   'org-latex-and-related nil
   :foreground "dim gray")

  ;; Because one of the main roam configuration variables it not good
  ;; enough of an authority to warrant implicit directory creation *if it
  ;; is missing*.
  (if (not (f-exists-p org-roam-directory))
      (f-mkdir-full-path org-roam-directory))
  (org-link-set-parameters "coords" :follow #'org-coords-open)
  (require 'ts)
  (require 'org-expiry)
  (define-abbrev-table 'org-mode-abbrev-table
    '(("anon" "anonymous")
      ("inf" "infinite")
      ("vm" "VM")
      ("Flugel" "Fl\\Uuml{}gel")
      ;; Yes, I can't type for shit
      ("ofthe" "of the")
      ("onthe" "on the")
      ("forthe" "for the")
      ("athe" "at the")
      ("inthe" "in the")
      ("teh" "the")
      ("im" "I'm")
      ("ambig" "ambiguous")
      ("i" "I")))
  (setq
   org-capture-templates
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
  :ORIGIN: %(hax/capture-location)
  :END:
"
      :empty-lines-before 1
      :empty-lines-after 1)
     ("I" "Idea" entry (file hax/inbox.org)
      "* %? :idea:
  :PROPERTIES:
  :CREATED: %U
  :ORIGIN: %(hax/capture-location)
  :END:
"
      :empty-lines-before 1
      :empty-lines-after 1)
     ("d" "Daily" entry (file+olp+datetree hax/notes.org)
      "** %U %(hax/capture-location)
  :PROPERTIES:
  :CREATED: %U
  :END:

%?"
      :empty-lines-before 1
      :empty-lines-after 1)
     ("@" "Daily" entry (file+olp+datetree hax/notes.org)
      "** %U %(hax/capture-location) %(hax/immediate-note-tags)
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
  :ORIGIN: %(hax/capture-location)
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
      "** %U %(hax/capture-location)
  :PROPERTIES:
  :CREATED: %U
  :END:

%?"
      :empty-lines-before 1
      :empty-lines-after 1)
     ("s" "Subtask-now" entry (clock)
      ;; (function hax/goto-top-clock)
      "** TODO %?
  :PROPERTIES:
  :CREATED: %U
  :ORIGIN: %(hax/capture-location)
  :END:
"
      :clock-in t
      :empty-lines-before 1
      :empty-lines-after 1)
     ;; For todo items that I want to finish today. If they are not properly
     ;; finalized, they stay visible for a week in agenda view, so I can
     ;; return to them anyway.
     ("u" "Subtask under; Deadline today" entry
      (function hax/org-goto-select-subtree)
      "** TODO %?
  DEADLINE: %(hax/org-end-of-the-day-stamp)
  :PROPERTIES:
  :CREATED: %U
  :ORIGIN: %(hax/capture-location)
  :END:
"
      :empty-lines-before 1
      :empty-lines-after 1)
     ;; Todo items that don't have a concrete deadline yet, but must be
     ;; positioned under some task
     ("U" "Subtask under" entry
      (function hax/org-goto-select-subtree)
      "** TODO %?
  :PROPERTIES:
  :CREATED: %U
  :ORIGIN: %(hax/capture-location)
  :END:
"
      :empty-lines-before 1
      :empty-lines-after 1
      )
     ("a" "Subtask under active; Deadline today" entry
      (function hax/org-goto-select-active-subtree)
      "** TODO %?
  DEADLINE: %(hax/org-end-of-the-day-stamp)
  :PROPERTIES:
  :CREATED: %U
  :ORIGIN: %(hax/capture-location)
  :END:
"
      :empty-lines-before 1
      :empty-lines-after 1)
     ("A" "Subtask under active" entry
      (function hax/org-goto-select-active-subtree)
      "** TODO %?
  :PROPERTIES:
  :CREATED: %U
  :ORIGIN: %(hax/capture-location)
  :END:
"
      :empty-lines-before 1
      :empty-lines-after 1)
     ))

  (setq
   ;; Don not open org-roam buffer automatically
   +org-roam-open-buffer-on-find-file nil
   ;; Don't implicitly enable the super-distracting,
   ;; almost-never-needed-unless-explicitly requested automatic UI follow
   ;; mode that zooms to closely no nodes that you instantly loose all the
   ;; context of what you are working with.
   org-roam-ui-follow nil
   ;; Agenda is a main todo file and inbox
   org-agenda-files (list hax/main.org
                          hax/inbox.org
                          hax/notes.org
                          hax/projects.org)
   org-refile-targets `((,hax/fic.org :maxlevel . 9)
                        (,hax/main.org :maxlevel . 3)
                        (,hax/projects.org :maxlevel . 3)
                        (,hax/inbox.org :maxlevel . 1)
                        (,hax/inbox.org :maxlevel . 2))
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
   org-startup-indented nil
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

  (setq
   org-agenda-custom-commands
   `(("l" "Long"
      ((agenda
        ""
        ((org-agenda-span 90)
         (org-agenda-start-day "-7d")
         (org-deadline-warning-days 35)))))
     ("*" "All"
      ((todo "WIP" ((org-agenda-files '(,hax/notes.org))))
       (todo "TODO" ((org-agenda-files '(,hax/notes.org))))
       ;; Show unfinished tasks for the last week, without filling all the
       ;; days - only show todo items.
       (agenda
        ""
        ((org-agenda-span 7)
         (org-agenda-start-day "-7d")
         (org-agenda-show-all-dates nil)
         (org-deadline-warning-days 0)))
       ;; Show all todo items for the next two weeks with filled days.
       (agenda
        ""
        ((org-agenda-span 14)
         ;; Start showing events from today onwards, when quickly assessing
         ;; target tasks I don't really need to focus on the past events.
         (org-agenda-start-day "-0d")
         ;; I show planned and deadlined events for the next two weeks - no
         ;; need to repeat the same information again for today.
         (org-deadline-warning-days 0)))

       (todo "WIP")
       (todo "POSTPONED")))))

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
        '(("TODO(t!/!)"         ;; Must be done now
           "TODO(t!/!)"         ;; Some other function breaks
           ;; org-todo-keywords, and I need to add
           ;; TODO twice here. I don't know what is
           ;; the cause, but this is a FIXME, although
           ;; with low priority.
           "LATER(l!)"          ;; Can be done sometimes later. Like
           ;; POSTPONED but without any idea when the
           ;; work can start again
           "NEXT(n!)"           ;; Next task after current
           "POSTPONED(P!/!)"    ;; Work is temporarily paused, but I have a
           ;; vague idea about next restart time.
           "WIP(w!)"            ;; Working on it
           "STALLED(s!)"        ;; External event is preventing further work
           "MAYBE(m!)"          ;; Not Guaranteed to happen
           "REVIEW(r!/!)"       ;; Check if this task must be done or not
           "|"
           "TIMEOUT(T)"         ;; Cannot be done due to time limits
           "FAILED(f@/!)"       ;; Tried to finish the task but failed
           "CANCELED(C@/!)"
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
  (setq hax/tags-file (f-join org-directory "tags"))
  (when (f-exists? (f-join hax/cache.d "org-clock-stack"))
    (setq hax/org-clock-stack
          (read-from-file (f-join hax/cache.d "org-clock-stack"))))

  (when (f-exists? (f-join hax/cache.d "org-clock-history"))
    (setq hax/org-clock-history
          (read-from-file (f-join hax/cache.d "org-clock-history"))))
  (setq org-tag-alist
        (concatenate
         'list
         '((:startgroup . nil)
           ("@work" . ?w)
           ("@home" . ?h)
           ("@university" . ?u)
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

(after! org
  (hax/org-mode-configure))

(setq
 org-odt-category-map-alist
 '(("__Table__" "Figure" "value" "??????????????"
    org-odt--enumerable-p)
   ("__Figure__" "Figure" "value" "??????????????"
    org-odt--enumerable-image-p)
   ("__MathFormula__" "Text" "math-formula" "Equation"
    org-odt--enumerable-formula-p)
   ("__DvipngImage__" "Equation" "value" "Equation"
    org-odt--enumerable-latex-image-p)
   ("__Listing__" "Listing" "value" "??????????????"
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

(setq hax/immediate-note-content "")
(setq hax/immediate-note-tags '())

(defun hax/get-immediate-note-content ()
  hax/immediate-note-content)

(cl-defun hax/immediate-note-tags (&optional (predefined '()))
  (let ((tags (append predefined hax/immediate-note-tags)))
    (if (< 0 (length tags))
        (s-wrap (s-join ":" tags) ":" ":")
      "")))

(hax/immediate-note-tags)




(when nil
  (defface hax/org-speech
    `((t (:foreground ,(doom-lighten (doom-color 'base7) 0.6)
          :weight light
          :slant italic))) "Face for keystrokes"
    :group 'org-faces)

  (font-lock-replace-keywords
   'org-mode
   `((,(rx (group "\"" (+ (not "\"")) "\"")) (0 'hax/org-speech t)))))

;;; Reimplementation of the built-in org-mode clock system with something
;;; that works properly instead of failing on every corner. Fixes following
;;; (and other) bugs and misfeatures: (*) Clocks are not truly persistent,
;;; I need to re-enable current clock to restore the time when emacs was
;;; turned off. (*) Undoing clock activation breaks emacs' brain and need
;;; to manually discard all the active clock time. (*) Clocking does not
;;; support nested tasks - I'm more than capable of subdividing current
;;; activity into shorter todo items as I go, clocking them in and finaly
;;; finishing everything. (*) cannot "pause and resume" clock - no
;;; immediate history of activity.

(defvar hax/org-clock-stack '() "Stack of currently active tasks")
(defvar hax/org-clock-history '() "History of the clocked task activation")

(defun read-from-file (file)
  "Parse FILE as a serialized elisp value and return the result of
parsing."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (read (current-buffer))))

(defun write-to-file (file data)
  "Write DATA to FILE"
  (with-temp-file file
    (prin1 data (current-buffer))))

(defconst hax/cache.d (expand-file-name "$HOME/.cache/haxscramper"))



(defun org-element-subtree-by-id (id)
  "Get parsed org-element for entry with `id', or `nil' if no such
subtree can be found."
  (with-temp-buffer
    (org-id-open id nil)
    (org-element-at-point)))

(defun hax/dbg/looking-at ()
  (interactive)
  (message
   "looking at: %s:%s..%s = [%s]"
   (line-number-at-pos)
   (point)
   (line-end-position)
   (propertize
    (buffer-substring-no-properties (point) (line-end-position))
    'face 'font-lock-warning-face)))


(defun hax/dbg/looking-around ()
  (interactive)
  (message
   "looking at: %s:%s..%s = [%s%s]"
   (line-number-at-pos)
   (line-beginning-position)
   (line-end-position)
   (propertize
    (buffer-substring-no-properties (line-beginning-position) (point)))
   'face 'font-lock-variable-name-face)
  (propertize
   (buffer-substring-no-properties (point) (line-end-position))
   'face 'font-lock-warning-face))

(defun hax/dbg/point (p)
  (save-excursion
    (goto-char p)
    (hax/dbg/looking-at)))

(defun org-get-logbook-extent ()
  "Get start and end position of the `:LOGBOOK:' drawer for current
subtree."
  (save-excursion
    (unless (org-at-heading-p) (outline-previous-heading))
    (goto-char (- (org-log-beginning t) 2))
    ;; For unknown reasons `org-log-BEGINNING' puts cursor on the END of
    ;; the logbook. Super intuitive, I know.
    (search-backward ":LOGBOOK:")
    (let* ((elt (org-element-property-drawer-parser nil))
           (beg (org-element-property :contents-begin elt))
           (end (org-element-property :contents-end elt)))
      (cons beg end))))

(defun org-get-logbook-notes ()
  "Get text of the org-mode logbook drawer notes"
  (let ((range (org-get-logbook-extent)))
    (buffer-substring-no-properties (car range) (cdr range))))

(defun hax/tmp ()
  (interactive)
  (let ((con (org-get-logbook-extent)))
    (hax/org-last-active-clock (car con) (cdr con))))

(defun org-element-parse-string (str)
  "Parse input string as org-mode buffer and return result"
  (with-temp-buffer
    (insert str)
    (org-element-parse-buffer)))

(defun hax/org-get-active-clock-timestamp-position ()
  "Get position of the active clock in current logbook, if any.
Create new `:LOGBOOK:' if one"
  (save-excursion
    (cl-block main
      (org-back-to-heading)
      (forward-line)
      (let ((pair (org-get-logbook-extent))
            (ind (current-indentation)))
        (goto-char (car pair))
        (while (< (point) (cdr pair))
          (when (org-at-clock-log-p)
            (forward-char
             (+
              ;; indentation
              ind
              ;; `CLOCK:' and space
              7
              ;; Bug in the `org-at-date-range' that prevents proper
              ;; checking if the cursor is placed on the starting `['
              ;; requires extra positioning
              1
              ))
            (unless (org-at-date-range-p t)
              (backward-char 1)
              (cl-return-from main (point))))
          (forward-line))))))

(defun hax/org-finish-timestamp ()
  (when (and (re-search-forward (rx "]") nil t)
             (not (org-at-date-range-p t)))
    (insert "--")
    (org-insert-time-stamp (org-current-time) 'with-hm 'inactive)
    (org-evaluate-time-range)))

(defun hax/maybe-finish-active-clock ()
  "Finsh last active clock if current subtree has any. If any
changes where made, return `t', otherwise return nil."
  (save-excursion
    (let ((active (hax/org-get-active-clock-timestamp-position)))
      (when active
        (goto-char active)
        (hax/dbg/looking-at)
        (hax/org-finish-timestamp)
        t))))

(defun hax/org-clock-out-this-task ()
  "Clock out task under cursor if logbook contains any active
items. If logbook is does not exist, might create new one."
  (interactive)
  ;; Find timestamp position in the current active clock of the entry
  (hax/maybe-finish-active-clock))

(defun hax/org-clock-in-this-task (&optional select start-time)
  "Add current task the clock stack"
  (interactive)
  (save-excursion
    (push (org-id-get-create) hax/org-clock-history)
    (push (org-id-get-create) hax/org-clock-stack)
    (org-back-to-heading)
    (forward-line)
    (hax/maybe-finish-active-clock)
    (org-add-log-entry
     (format
      "CLOCK: %s"
      (with-temp-buffer
        (org-insert-time-stamp (org-current-time) 'with-hm 'inactive)
        (buffer-substring (point-min) (point-max)))))))




(defun hax/org-clock-out-task (id)
  "Clock out the task with ID from the stack and all
subsequent (nested) ones"
  (when (not id) (error "Cannot clock out `nil' task."))
  (save-excursion
    (message "Finished task %s" id)
    (org-id-open id nil)
    (hax/dbg/looking-at)
    (hax/org-clock-out-this-task)))

(defun hax/org-clock-out-top-task
    (&optional SWITCH-TO-STATE FAIL-QUIETLY AT-TIME)
  "Clock out the topmost task from the stack."
  (interactive)
  (hax/org-clock-out-task (pop hax/org-clock-stack)))

(defun hax/around-org-clock-in (fun &rest args)
  (interactive)
  (funcall-interactively 'hax/org-clock-in-this-task args))

(defun hax/around-org-clock-out (fun &rest args)
  (interactive)
  (funcall-interactively 'hax/org-clock-out-top-task args))

(defun advice-remove-all (sym)
  "AUTOMATICALLY remove all advice from a function symbol, without
having to go through repeated invocations of the same function
over and over as suggested in the
https://lists.gnu.org/archive/html/emacs-devel/2017-04/msg00767.html (the
use case is 'I want to automate the logic and this is a basic
function for cleaning up in case the 'smart' advice system shits
itself once again')"
  (interactive "aFunction symbol: ")
  (advice-mapc (lambda (advice _props) (advice-remove sym advice)) sym))

(when nil
  (advice-add 'org-clock-in :around #'hax/around-org-clock-in)
  (advice-add 'org-clock-out :around #'hax/around-org-clock-out))

(defun hax/goto-top-clock ()
  (interactive)
  (let ((top (car (last hax/org-clock-stack))))
    (if top
        (org-id-open top nil)
      (error "No active clocked item - cannot add subtask"))))

(defun hax/open-org ()
  (interactive)
  (find-file hax/inbox.org)
  (hax/org-mode-configure)
  (hax/org-mode-hook)
  (find-file hax/inbox.org)
  (find-file hax/main.org)
  (org-roam-db)
  (find-file hax/notes.org))


(defun hax/org-element-get-logbook ()
  "Get org-element of the `:LOGBOOK:' entry for a current tree if
any."
  (save-excursion
    (org-back-to-heading)
    (when (re-search-forward ":LOGBOOK:" nil t)
      (let* ((elem (org-element-at-point))
             (min-pos (org-element-property :contents-begin elem))
             (max-pos (org-element-property :contents-end elem)))
        (org-element-parse-string
         (buffer-substring-no-properties min-pos max-pos))))))

(defun hax/org-get-logbook-ranges ()
  (org-element-map
      (hax/org-element-get-logbook)
      'clock
    (lambda (item) (org-element-property :value item))))

(defun hax/org-get-entry-activations ()
  "Return (TODO, now prints) full list of the activation ranges for
all known agenda entries."
  (let* ((now (ts-now))
         (year (ts-year now))
         (month (ts-month now))
         (day (ts-day now)))
    (--each (org-map-entries
             (lambda ()
               (let* ((ranges (hax/org-get-logbook-ranges))
                      (filtered
                       (when ranges
                         (--filter
                          (and
                           (eq (org-element-property :year-end it) year)
                           (eq (org-element-property :month-end it) month)
                           (eq (org-element-property :day-end it) day))
                          ranges))))
                 (when (and ranges (< 0 (length filtered)))
                   (cons (org-get-outline-path t) ranges))))
             nil
             `(,hax/main.org))
      (when it
        (message "%s" (org-format-outline-path (-slice (car it) -2) 256))
        (dolist (time (cdr it))
          (message "  %s" (org-element-property :raw-value time)))))))

(defun hax/closest-unicode-fraction (value)
  (let* ((values '((1 . "???")
                   (0 . "???")
                   (0.25 . "??")
                   (0.5 . "??")
                   (0.75 . "??")
                   (0.33 . "???")
                   (0.66 . "???")
                   (0.2 . "???")
                   (0.4 . "???")
                   (0.6 . "???")
                   (0.8 . "???")
                   (0.16 . "???")
                   (0.83 . "???"))))
    (cdr (first (sort (--map (cons (abs (- (abs value) (car it))) (cdr it)) values)
                      (lambda (lhs rhs) (< (car lhs) (car rhs))))))))

;; (hax/closest-unicode-fraction 0.5)

(defun hax/relative-hour-fraction (tdiff)
(/ (if (< 0 tdiff)
       (mod (/ tdiff 60) 60)
     (- 60 (mod (/ tdiff 60) 60))) 60))

(defun hax/org-agenda-format-date (date)
  "Format a DATE string for display in the daily/weekly agenda.
This function makes sure that dates are aligned for easy reading.
Mostly reimplements `org-agenda-format-date-aligned', but also
displays relative (from the current time) hour and minute range."
  (require 'cal-iso)
  (let* ((day (cadr date))
         (month (car date))
         (year (nth 2 date))
         ;; Immediately convert to the `ts` time format, because regular
         ;; emacs time formatting is an absolutely nauseating trash.
         (ts (ts-apply :year year :month month :day day :hour 0 :minute 0 (ts-now)))
         (tdiff (ts-difference ts (ts-now))))
    (format
     "%03d%s %s"
     (/ tdiff 3600)
     (hax/closest-unicode-fraction (hax/relative-hour-fraction tdiff))
     (ts-format "%m-%d %a" ts))))

(defun hax/org-company-tags (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'hax/org-company-tags))
    (prefix (progn
              (when (and (looking-back "#\\w*")
                         (derived-mode-p 'org-mode))
                (company-grab-symbol))))
    (candidates (--filter (and (stringp it) (cl-search arg it))
                          (--map (car it) org-tag-alist)))
    (meta (format "This value is named %s" arg))))

(add-to-list 'company-backends 'hax/org-company-tags)

(defun tmp()
  (interactive)
  (setq
   company-backends
   '(company-capf
     (:separate hax/org-company-tags company-dabbrev company-yasnippet company-ispell)
     (:separate hax/org-company-tags company-ispell company-dabbrev company-yasnippet))))


(defun hax/after-inline-uppercase ()
  (interactive)
  (let* ((word (current-word)))
    (s-uppercase-p (substring word 0 1))))

(defun hax/flyspell-org-mode-verify ()
  "Customized wrapper around `org-mode-flyspell-verify' that also
skips capitalized and upperacsed words (names and abbreviations)"
  (when (org-mode-flyspell-verify)
    (not (hax/after-inline-uppercase))))

(defun org-count-subheadings ()
  "Count the number of direct and recursive subheadings below the current heading. Return cons with `(CHILDREN . DESCENDANTS)'"
  (interactive)
  (let ((descendants 0)
        (children 0)
        (heading-level (1+ (org-outline-level)))
        (end (save-excursion
               (ignore-errors
                 (outline-end-of-subtree)
                 (point)))))
    (when end
      (save-excursion
        (while (and (outline-next-heading)
                    (< (point) end))
          (progn
            (setf descendants (1+ descendants))
            (when (= heading-level (org-outline-level))
              (setf children (1+ children)))))))
    (cons children descendants)))

(defun hax/org-has-subtree () (< 0 (car (org-count-subheadings))))

(defun hax/compare-subtrees (t1 t2)
  (let* ((todo1 (car t1))
         (todo2 (car t2))
         (head1 (cdr t1))
         (head2 (cdr t2))
         (names (hax/org-todo-only-names))
         (res (if (and todo1 todo2) (> (-elem-index todo1 names)
                                       (-elem-index todo2 names))
                (if todo1 t
                  (if todo2 t
                    (string< head1 head2))))))
    ;; (message ">> %s ? %s = %s" t1 t2 res)
    res))

(defun hax/getkey-title ()
  (cons (org-get-todo-state) (org-get-heading)))

(defun hax/sort-subtree-contextually ()
  (interactive)
  (if (hax/org-has-subtree)
      (progn
        (org-sort-entries nil ?F #'hax/getkey-title #'hax/compare-subtrees)
        (save-excursion
          (org-back-to-heading)
          (org-cycle-internal-local)
          (org-cycle-internal-local)))
    (save-excursion
      (outline-up-heading 1)
      (org-sort-entries nil ?f #'hax/getkey-title #'hax/compare-subtrees)
      (org-back-to-heading)
      (org-cycle-internal-local)
      (org-cycle-internal-local))))

(put 'org-mode 'flyspell-generic-check-word-predicate 'hax/flyspell-org-mode-verify)

(defun hax/org-todo-only-names ()
  (--map (substring it 0 (s-index-of "(" it)) (car org-todo-keywords)))

