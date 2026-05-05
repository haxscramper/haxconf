;;; -*- lexical-binding: t; -*-

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

(defun hax/org-add-log-entry (text &optional drawer)
  "Insert TEXT as a log entry for the current Org heading, respecting indentation.

Uses `org-log-beginning' to find/create the drawer insertion point.
If DRAWER is non-nil, log into that drawer name (default: \"LOGBOOK\").

TEXT is inserted without text properties and indented like other drawer
contents (drawer indentation + 2 spaces)."
  (let* ((org-log-into-drawer (or drawer "LOGBOOK"))
         (text (substring-no-properties (or text "")))
         ;; Normalize to no trailing newline; we'll add newlines ourselves.
         (text (replace-regexp-in-string "\n\\'" "" text)))
    (org-with-wide-buffer
     (org-back-to-heading t)
     (save-excursion
       (let* ((pos (org-log-beginning t))      ; creates drawer if needed
              (drawer-indent
               (save-excursion
                 (goto-char pos)
                 ;; `org-log-beginning' commonly returns BOL of the :END: line,
                 ;; whose indentation matches the drawer's indentation.
                 (current-indentation)))
              (prefix (make-string drawer-indent ?\s)))
         (goto-char pos)
         (hax/log "DRAWER-INDENT: %s" drawer-indent)
         (unless (bolp) (insert "\n"))
         (dolist (line (split-string text "\n"))
           (insert prefix line "\n")))))))

(defun hax/org-rename-subtree (new-title)
  "Rename current Org subtree heading to NEW-TITLE and log the rename.

Adds a LOGBOOK entry:
  - Renamed on <rename date> from \"<original title>\""
  (interactive
   (progn
     (unless (derived-mode-p 'org-mode)
       (user-error "Not in org-mode"))
     (org-back-to-heading t)
     (let ((old (substring-no-properties (org-get-heading t t t t))))
       (list (read-string (format "Rename \"%s\" to: " old) old)))))

  (org-with-wide-buffer
   (org-back-to-heading t)
   (let* ((old-title (substring-no-properties (org-get-heading t t t t)))
          ;; `org-time-stamp-format' already includes <...> (or [...] if inactive),
          ;; so do NOT wrap it in extra <>.
          (ts (format-time-string (org-time-stamp-format t t) (current-time))))
     (org-edit-headline new-title)
     (hax/org-add-log-entry
      (format "- Renamed on %s from %S" ts old-title) ; %S gives "...", escaped
      "LOGBOOK")
     new-title)))


(defun hax/maybe-relative-time ()
  "Insert relative time (in hours) between current time and target
  subtree deadline/sheduled/timestamp if any."
  (save-excursion
    ;; (with-no-warnings (defvar date) (defvar entry))
    ;; (hax/log "%s" (calendar-absolute-from-gregorian date))

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


(defun hax/org-prevent-same-state-change (func arg &optional _start)
  "Prevent state changes that don't actually change the state.
This function is meant to be used as advice before `org-todo'.
If the state to change to (ARG) is the same as the current state,
use `:override' advice to do nothing and prevent `org-todo' from
being called."

  (let ((current-state (remove-string-properties (org-get-todo-state))))
    (if (string-equal current-state arg)
        (hax/log "State is already \"%s\"" current-state)
      ;; (hax/log "Override transitioning states %s -> %s" current-state arg)
      (cl-return-from org-todo))))

;; (advice-add 'org-todo :around #'hax/org-prevent-same-state-change)

(setq hax/org-priority-list '(?X ?S ?A ?B ?C ?D ?E ?F))

(defun hax/org-priority-set-p ()
  (interactive)
  (let* ((element (org-element-context))
         (type (org-element-type element)))
    (if (eq type 'headline) (org-element-property :priority element)
      nil)))


(defun hax/org-change-priority (direction)
  (interactive)
  (let* ((cur-str (org-entry-get (point) "PRIORITY"))
         (cur (string-to-char cur-str))
         (new (char-to-string
               (if (eq direction 'up)
                   (or (nth 1 (member cur (reverse hax/org-priority-list)))
                       org-priority-lowest)
                 (or (nth 1 (member cur hax/org-priority-list))
                     org-priority-highest)))))
    (if (hax/org-priority-set-p)
        (if (or (and (eq direction 'down) (equal cur ?F))
                (and (eq direction 'up) (equal cur ?X)))
            (progn
              (hax/org-remove-priority-at-point)
              (org-add-log-entry
               (format "- Priority \"%s\" Removed at %s"
                       cur-str (hax/current-timestamp))))
          (progn
            (org-entry-put (point) "PRIORITY" new)
            (org-add-log-entry
             (format "- Priority \"%s\" Changed From \"%s\" at %s"
                     new cur-str (hax/current-timestamp)))))
      (progn
        (org-entry-put (point) "PRIORITY" cur-str)
        (org-add-log-entry
         (format "- Priority \"%s\" Added at %s" cur-str (hax/current-timestamp)))))))

(defun hax/org-remove-priority-at-point ()
  (interactive)
  (let* ((element (org-element-at-point))
         (priority (org-element-property :priority element)))
    (when priority  ;; Only operate on headlines with a priority
      (save-excursion
        (goto-char (org-element-property :begin element))
        (re-search-forward "\\[#.\\]\s?" (org-element-property :end element))
        (replace-match "")))))


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
    ;; (hax/log ">> %s ? %s = %s" t1 t2 res)
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


(defun hax/org-insert-subtree-comment ()
  (interactive)
  (insert (format "%s COMMENT" (s-repeat (+ 1 (org-current-level)) "*")))
  (org-expiry-insert-created))


(defun hax/org-insert-subtree-offset (title level-offset)
  (interactive "sTitle: ")
  (insert
   (format
    "%s %s"
    (s-repeat (hax/clamp (+ level-offset (org-current-level)) 1 nil)
              "*")
    title))
  (org-id-get-create)
  (org-expiry-insert-created))

(defun hax/org-insert-subtree-same (title)
  (interactive "sTitle: ")
  (hax/org-insert-subtree-offset title 0))

(defun hax/org-insert-subtree-below (title)
  (interactive "sTitle: ")
  (hax/org-insert-subtree-offset title 1))

(defun hax/org-insert-subtree-above (title)
  (interactive "sTitle: ")
  (hax/org-insert-subtree-offset title -1))

(cl-defun hax/org-insert-subtree-old-archive (&optional only-archives)
  (interactive)
  (hax/org-select-subtree-callback
   "Target subtree: "
   (lambda (x) (org-entry-put
                (point)
                "ARCHIVE_OLPATH_PARENT_ID"
                (hax/get-subtree-id-for-marker (cdr x))))
   'hax/org-insert-link-to-heading
   (org-collect-known-entries (org-get-known-file-buffers-archival only-archives))))

(defun hax/org-insert-todo-entry ()
  "Insert new TODO entry with creation date filled in. Simplified
down version of the `+org/insert-item-below' that does not do
anything context-aware. It simply inserts new 'TODO' entry after
the current one."
  (interactive)
  (org-end-of-subtree)
  (setq org-adapt-indentation t)
  (insert "\n\n" (make-string (or (org-current-level) 1) ?*) " TODO ")
  (setq org-expiry-inactive-timestamps t)
  (org-expiry-insert-created)
  (evil-insert-state)
  (hax/org-update-all-cookies)
  (hax/ensure-todo "TODO"))


