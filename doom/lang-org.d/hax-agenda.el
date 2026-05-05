;;; -*- lexical-binding: t; -*-

(require 'dash)
(require 'org-element)
(require 'org-ql)

(defun hax/org-time-to-iso-8601 (time)
  (when time
    (format-time-string "%Y-%m-%dT%H:%M:%S%z" time)))

(defun hax/org-element-timestamp-to-time (timestamp)
  (when-let ((raw (org-element-property :raw-value timestamp)))
    (org-time-string-to-time raw)))



(defun hax/org-element--headline-path (headline)
  (let ((path nil)
        (node headline))
    (while node
      (when (eq (org-element-type node) 'headline)
        (push
         (substring-no-properties
          (or (org-element-property :raw-value node) ""))
         path))
      (setq node (org-element-property :parent node)))
    (cdr path)))

(defun hax/org-element--latest-clock-value (headline)
  (message "%s headline" headline)
  (let ((latest nil))
    (org-element-map headline 'clock
      (lambda (clock)
        (message "clock: %s" clock)
        (when-let ((value (org-element-property :value clock))
                   (start (car value)))
          (when (or (null latest) (time-less-p latest start))
            (setq latest start)))))
    latest))

(defun hax/org-element-entry-plist (headline)
  (let* ((marker (org-element-property :org-marker headline))
         (title (substring-no-properties
                 (or (org-element-property :raw-value headline) "")))
         (todo-state (when-let ((todo (org-element-property :todo-keyword headline)))
                       (substring-no-properties todo)))
         (tags (--map (substring-no-properties it)
                      (org-element-property :tags headline)))
         (created
          (org-element-map headline 'node-property
            (lambda (node)
              (when (string= (org-element-property :key node) "CREATED")
                (org-time-string-to-time
                 (org-element-property :value node))))
            nil t))
         (last-clocked-in (hax/org-element--latest-clock-value headline))
         (file (substring-no-properties
                (or (buffer-file-name (marker-buffer marker)) "")))
         (path (hax/org-element--headline-path headline))
         (overall-minutes
          (org-with-point-at marker
            (org-clock-sum-current-item))))
    (list
     :title title
     :created (hax/org-time-to-iso-8601 created)
     :last-clocked-in (hax/org-time-to-iso-8601 last-clocked-in)
     :overall-time overall-minutes
     :todo-state todo-state
     :subtree-tags tags
     :file file
     :subtree-path path)))

(defun hax/org-ql-collect-groups (groups)
  (mapcar
   (lambda (group)
     (let* ((header (plist-get group :header))
            (files (plist-get group :files))
            (query (plist-get group :query))
            (sort (plist-get group :sort))
            (entries
             (--map
              (hax/org-element-entry-plist it)
              (org-ql-select files query
                :sort sort
                :action 'element-with-markers))))
       (list
        :header header
        :entries entries)))
   groups))

(defun hax/detail/configure-org-agenda-query ()
  (setq hax/org-ql-dashboard-groups
        `((:header "In progress (NEXT/WIP/PAUSED/BLOCKED)"
           :files ,(org-agenda-files)
           :query (todo "NEXT" "WIP" "PAUSED" "BLOCKED"))

          (:header "Staging todo"
           :files (,(expand-file-name hax/staging.org))
           :query (and
                   (todo "TODO")
                   (not (function hax/org-agenda-skip))))

          (:header "Notes & High priority project todos"
           :files (,(expand-file-name hax/notes.org)
                   ,(expand-file-name hax/projects.org))
           :query (and
                   (todo "TODO")
                   (or
                    (and
                     (path ,(expand-file-name hax/notes.org))
                     (not (function hax/org-agenda-skip)))
                    (and
                     (path ,(expand-file-name hax/projects.org))
                     (not (function hax/org-agenda-skip-low-priority))))))

          (:header "2-week preview"
           :files ,(org-agenda-files)
           :query (and
                   (not (function hax/org-agenda-skip))
                   (or
                    (scheduled :from today :to 14)
                    (deadline :from today :to 14)
                    (ts :from today :to 14))))

          (:header "Repeated todos"
           :files (,(expand-file-name hax/repeated.org))
           :query (todo "TODO")))))





(defun hax/org-agenda-clocked-time ()
  "Safely calculate the clocked time for the current agenda item."
  (require 'org-clock)
  (condition-case nil
      (save-excursion
        (format "[%s]"
                (s-pad-left 5 "0" (org-duration-from-minutes (org-clock-sum-current-item)))))
    (error "")))


(defun hax/org-agenda-last-clocked-in-time ()
  "Get formatting string showing how much time has passed since subtree was clocked in last time."
  (condition-case nil
      (save-excursion
        (let* ((last-time (if (org-clock-get-last-clock-out-time)
                              (org-clock-get-last-clock-out-time)
                            (org-time-string-to-time (org-entry-get (point) "CREATED"))))
               (now (current-time))
               (diff-time (time-subtract now last-time))
               (minutes (/ (float-time diff-time) 60)))
          ;; (hax/log "LT: %s" last-time)
          (let* ((raw-time (seconds-to-time (* minutes 60)))
                 (years (/ minutes (* 60 24 365)))
                 (months (/ minutes (* 60 24 30)))
                 (weeks (/ minutes (* 60 24 7)))
                 (days (/ minutes (* 60 24)))
                 (hours (/ minutes 60))
                 (mins (mod minutes 60)))
            (format "[%6s]" (cond
                             ((>= years 1)
                              (format "%2dy%2dm" years (mod months 12)))
                             ((>= months 1)
                              (format "%2dm%2dw" months (/ (mod days 30) 7)))
                             ((>= weeks 1)
                              (format "%2dw%2dd" weeks (mod days 7)))
                             ((>= days 1)
                              (format "%2dd%2dh" days (mod hours 24)))
                             ((>= hours 1)
                              (format "%2dh%2dm" hours mins))
                             (t
                              (format "%2dm" minutes))))))
        )
    (error
     (format "[%6s]" "ERR"))))

(setq
 org-agenda-prefix-format
 '(;; For regular agenda items, show (?whatever?)
   ;; first, then align time to five characters,
   ;; then 12 for scheduled information. Title and
   ;; all the other data will be placed afterwards.
   ;;
   ;; 'e' is for time estimates.
   (agenda . "%(hax/maybe-relative-time) %-5(hax/org-agenda-clocked-time) %-12t ")
   ;; Indentation to align effort time
   (todo . "%-5(hax/org-agenda-last-clocked-in-time) %-5(hax/org-agenda-clocked-time) ")
   (tags . "%-5(hax/org-agenda-clocked-time) ")
   (search . "%-5(hax/org-agenda-clocked-time) "))
 org-agenda-start-on-weekday nil
 org-agenda-ndays 14
 org-agenda-show-all-dates t
 org-agenda-skip-deadline-if-done t
 org-agenda-skip-scheduled-if-done t
 org-agenda-hide-tags-regexp ".*"
 org-agenda-block-separator (s-repeat 45 "-")
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


(defun hax/org-agenda-skip-recurring ()
  (let* ((org-repeater-regexp
          (rx
           (group
            "<"  ; Opening angle bracket
            (seq (repeat 4 digit)
                 "-"
                 (repeat 2 digit)
                 "-"
                 (repeat 2 digit)  ; Date: YYYY-MM-DD
                 (seq " "
                      (repeat 3 alpha))  ; Optional day of week: %a
                 (seq " "
                      (repeat 2 digit)
                      ":"
                      (repeat 2 digit)
                      ":"
                      (repeat 2 digit)  ; Optional time: HH:MM:SS
                      (optional ; Optional timezone: %Z
                       " +"
                       (repeat 2 digit)))
                 (seq " "
                      (one-or-more (any ".+-"))  ; Repeater prefix
                      (one-or-more digit)
                      (any "dwmy")  ; Repeater
                      (optional "/" (one-or-more digit) (any "dwmy")))
                                        ; Secondary spacing
                 ">"))  ; Closing angle bracket

           ))
         (subtree-end (save-excursion (org-end-of-subtree t))))
    (if (re-search-forward org-repeater-regexp subtree-end t)
        (progn subtree-end)
      nil)))

(defun hax/org-has-tag (tag)
  "Skip all entries that correspond to TAG.

If OTHERS is true, skip all entries that do not correspond to TAG."

  (let* ((subtree-point (or (and (org-at-heading-p) (point))
                            (save-excursion (org-back-to-heading) (point))))
         (current-subtree (org-get-tags-at subtree-point)))
    ;; (hax/log "%s %s %s" subtree-point current-subtree (hax/dbg/looking-at))
    (if (member tag current-subtree)
        (goto-char (org-end-of-subtree current-subtree))
      nil)))


(defun hax/org-agenda-delete-empty-blocks ()
  "Remove empty agenda blocks.
An empty block is a header line followed immediately by another header
line or end of buffer, with only blank lines in between."
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char (point-min))
      (let (block-start block-header-end has-content)
        (while (not (eobp))
          (cond
           ;; Separator line marks the end of a block
           ((looking-at "^-\\{10,\\}")
            (when (and block-start (not has-content))
              ;; Delete from block start to end of separator line
              (delete-region block-start (line-beginning-position))
              ;; Also remove the now-orphaned separator if it's at point
              (when (looking-at "^-\\{10,\\}")
                (delete-region (line-beginning-position)
                               (min (1+ (line-end-position)) (point-max)))))
            (setq block-start nil
                  block-header-end nil
                  has-content nil)
            (unless (eobp) (forward-line 1)))
           ;; Overriding header line (starts at column 0, non-blank, non-separator)
           ((and (not block-start)
                 (looking-at "^[^ \t\n-]"))
            (setq block-start (line-beginning-position)
                  has-content nil)
            (forward-line 1)
            (setq block-header-end (point)))
           ;; Blank line — doesn't count as content
           ((looking-at "^\\s-*$")
            (forward-line 1))
           ;; Anything else is content
           (t
            (setq has-content t)
            (forward-line 1))))
        ;; Handle last block if empty (no trailing separator)
        (when (and block-start (not has-content))
          (delete-region block-start (point-max)))))))


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
        (hax/log "%s" (org-format-outline-path (-slice (car it) -2) 256))
        (dolist (time (cdr it))
          (hax/log "  %s" (org-element-property :raw-value time)))))))

(defun hax/closest-unicode-fraction (value)
  (let* ((values '((1 . "⅟")
                   (0 . "⁰")
                   (0.25 . "¼")
                   (0.5 . "½")
                   (0.75 . "¾")
                   (0.33 . "⅓")
                   (0.66 . "⅔")
                   (0.2 . "⅕")
                   (0.4 . "⅖")
                   (0.6 . "⅗")
                   (0.8 . "⅘")
                   (0.16 . "⅙")
                   (0.83 . "⅚"))))
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
         (ts (ts-apply :year year
                       :month month
                       :day day
                       :hour 0
                       :minute 0 (ts-now)))
         (tdiff (ts-difference ts (ts-now))))
    (format
     "%03d%s %s"
     (/ tdiff 3600)
     (hax/closest-unicode-fraction (hax/relative-hour-fraction tdiff))
     (ts-format "%m-%d %a" ts))))


(defface hax/org-agenda-header
  '((t :inherit org-agenda-structure
     :extend t))
  "Face for custom agenda section headers.")

(set-face-attribute 'hax/org-agenda-header nil
                    :background (doom-darken (doom-color 'red) 0.5))

(defun hax/org-agenda-style-headers ()
  "Add empty line after headers and style them with red background."
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (when (get-text-property (point) 'org-agenda-structural-header)
          (let ((end (line-end-position)))
            (add-text-properties (line-beginning-position) end
                                 '(face hax/org-agenda-header))
            ;; Extend face to full width
            (goto-char end)
            (unless (and (not (eobp))
                         (save-excursion (forward-line 1)
                                         (looking-at "^\\s-*$")))
              (insert "\n"))))
        (forward-line 1)))))



(setq org-duration-format '((special . h:mm)))

(defun hax/org-agenda-hook ()
  (display-line-numbers-mode 1)
  (hax/org-agenda-style-headers)
  (hax/org-agenda-delete-empty-blocks)
  )

(add-hook 'org-agenda-finalize-hook #'hax/org-agenda-hook)





(setq org-startup-indented t)

(defun hax/org-agenda-skip ()
  (or (hax/org-has-tag "no_agenda")
      (hax/org-agenda-skip-recurring)))

(defun hax/org-agenda-skip-low-priority ()
  (save-excursion
    (org-back-to-heading t)
    (unless (or (re-search-forward ":bug:" (line-end-position) t)
                (re-search-forward "#[XAS]" (line-end-position) t))
      (or (outline-next-heading) (point-max)))))


(when t
  (defun hax/tag-to-state (tags)
    "Convert tags to a state string."
    (mapconcat
     'identity
     (delq nil
           (mapcar
            (lambda (tag)
              (pcase tag
                ("status##pending_clarification" "QUESTION")
                ("status##not_reproducible" "*NO REPRO*")
                ("status##waiting_review" "*ON REVIEW*")
                ("status##need_help" "*NEED HELP*")
                ("status##blocking_dependency" "*DEPENDENCY*")
                ;; Use status block reason
                ("BLOCKED" nil)
                ((or "COMPLETED" "WIP") (concat "*" tag "*"))
                ;; Add more tag-to-state mappings here
                ))
            tags))
     ", "))

  (defun hax/process-subtree ()
    "Process the current subtree and generate a checklist item."
    (let ((title (org-get-heading t t))
          (tags (org-get-tags))
          (todo-keyword (org-get-todo-state)))
      (let* ((state (hax/tag-to-state (append tags (list todo-keyword))))
             (id (org-id-get-create))
             (formatted-title (if (string-match "=\\(\\w+-\\w+\\)=" title)
                                  (match-string 1 title)
                                ""))
             (remainder-title (replace-regexp-in-string "=.*?=" "" title))
             (prefix (concat "- "
                             (if (> (length state) 0) (concat state " "))
                             "[[" id "][" formatted-title "]] "))
             (prefix-len (- 88 (- (length prefix) (+ 2 (length id)))))
             (truncated-title (if (> (length remainder-title) prefix-len)
                                  (substring remainder-title 0 prefix-len)
                                remainder-title)))
        (concat prefix "_" (s-trim truncated-title) "_"))))

  (defun hax/generate-todo-checklist ()
    "Generate a todo checklist from all matching subtrees."
    (interactive)
    (let ((checklist '()))
      (org-map-entries
       (lambda ()
         (when (string-match "=.*?-.*?=" (org-get-heading t t))
           (push (hax/process-subtree) checklist))))
      (substring-no-properties (mapconcat 'identity (nreverse checklist) "\n"))))

  (defun hax/generate-todo-checklist-file (filename)
    "Generate a todo checklist from all matching subtrees in the file specified by FILENAME."
    (with-current-buffer (find-file-noselect filename)
      (hax/generate-todo-checklist)))

  (defun hax/insert-todo-checklist-staging ()
    (interactive)
    (insert (hax/generate-todo-checklist-file hax/staging.org))))

(add-hook! 'org-agenda-mode-hook 'hax/agenda-mode-hook)
