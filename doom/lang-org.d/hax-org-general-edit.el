;;; -*- lexical-binding: t; -*-

(defun hax/org-shiftup (&optional arg)
  "Act on current element according to context.
Call `org-timestamp-up' or `org-priority-up', or
`org-previous-item', or `org-table-move-cell-up'.  See the
individual commands for more information."
  (interactive "P")
  (cond
   ((run-hook-with-args-until-success 'org-shiftup-hook))
   ((and org-support-shift-select (org-region-active-p))
    (org-call-for-shift-select 'previous-line))
   ((org-at-timestamp-p 'lax)
    (call-interactively (if org-edit-timestamp-down-means-later
			    'org-timestamp-down 'org-timestamp-up)))
   ((and (not (eq org-support-shift-select 'always))
	 org-priority-enable-commands
	 (org-at-heading-p))
    (hax/org-change-priority 'up))
   ((and (not org-support-shift-select) (org-at-item-p))
    (call-interactively 'org-previous-item))
   ((org-clocktable-try-shift 'up arg))
   ((and (not (eq org-support-shift-select 'always))
	 (org-at-table-p))
    (org-table-move-cell-up))
   ((run-hook-with-args-until-success 'org-shiftup-final-hook))
   (org-support-shift-select
    (org-call-for-shift-select 'previous-line))
   (t (org-shiftselect-error))))

(defun hax/org-shiftdown (&optional arg)
  "Act on current element according to context.
Call `org-timestamp-down' or `org-priority-down', or
`org-next-item', or `org-table-move-cell-down'.  See the
individual commands for more information."
  (interactive "P")
  (cond
   ((run-hook-with-args-until-success 'org-shiftdown-hook))
   ((and org-support-shift-select (org-region-active-p))
    (org-call-for-shift-select 'next-line))
   ((org-at-timestamp-p 'lax)
    (call-interactively (if org-edit-timestamp-down-means-later
			    'org-timestamp-up 'org-timestamp-down)))
   ((and (not (eq org-support-shift-select 'always))
	 org-priority-enable-commands
	 (org-at-heading-p))
    (hax/org-change-priority 'down))
   ((and (not org-support-shift-select) (org-at-item-p))
    (call-interactively 'org-next-item))
   ((org-clocktable-try-shift 'down arg))
   ((and (not (eq org-support-shift-select 'always))
	 (org-at-table-p))
    (org-table-move-cell-down))
   ((run-hook-with-args-until-success 'org-shiftdown-final-hook))
   (org-support-shift-select
    (org-call-for-shift-select 'next-line))
   (t (org-shiftselect-error))))



(defun hax/fill-paragraph (&optional arg)
  (interactive "P")
  (let ((fill-column (if (and arg (numberp (prefix-numeric-value arg)))
                         (prefix-numeric-value arg)
                       999999)))
    (funcall-interactively 'fill-paragraph 'full)))

(defun hax/org-insert-uuid-anchor ()
  (interactive)
  (insert "<<")
  (kill-ring-save
   (point)
   (progn (uuidgen t) (point)))
  (insert ">>"))


(defun ensure-text-around (expr)
  (if (= (- (line-end-position) 1) (point))
      (progn
        (goto-char (+ 1 (point)))
        (when (not (or (= (char-after) ?\s)
                       (= (char-before) ?\s)))
          (insert " "))
        (funcall expr))
    (progn
      (when (or (not (char-after))
                (not (= (char-after) ?\s)) ) (insert " "))
      (when (not (= (char-before) ?\s)) (insert " "))
      (funcall expr)
      (when (not (= (char-before ?\s))) (insert " "))
      (when (or (not (char-after))
                (not (= (char-after) ?\s))) (insert " ")))))

(defun hax/org-insert-text-tag ()
  "Insert org-mode hashtag under corrent cursor position, adding
necessary whitespace around it if he cursor wasn't positioned at
the empty area."
  (interactive)
  (ensure-text-around (lambda () (insert (concat "#" (hax/select-tag nil))))))

(defun hax/org-insert-timestamp ()
  "Insert org-mode hashtag under corrent cursor position, adding
necessary whitespace around it if he cursor wasn't positioned at
the empty area."
  (interactive)
  (ensure-text-around (lambda () (org-insert-time-stamp (current-time) t t))))

(defun hax/org-insert-timestamped-parens ()
  (interactive)
  (ensure-text-around
   (lambda ()
     (insert "((")
     (org-insert-time-stamp (current-time) t t)
     (insert "))"))))

