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
