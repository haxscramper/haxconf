;;; -*- lexical-binding: t; -*-

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

(defun hax/org-current-timestamp ()
  "Return current time as formatted inactive org timestamp"
  (s-replace-all '(("<" . "[") (">" . "]"))
                 (ts-format (concat (cdr org-time-stamp-formats))
                            (ts-now))))


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

