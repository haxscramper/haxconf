;;; -*- lexical-binding: t; -*-


(defun org-collect-known-entries (&optional sources)
  (let* ((buffers
          (if sources
              (--map (if (bufferp it) it (get-file-buffer it)) sources)
            (org-get-known-file-buffers)))
         (entries nil))
    (dolist (b (delq nil buffers))
      (with-current-buffer b
        (setq entries
              (nconc entries
                     (counsel-outline-candidates
                      (cdr (assq 'org-mode counsel-outline-settings))
                      (counsel-org-goto-all--outline-path-prefix))))))
    entries))

(defun org-collect-known-entries (&optional file-list)
  (let (entries)
    (dolist (b (if file-list
                   (--map (get-file-buffer it) file-list)
                 (org-get-known-file-buffers)))
      (with-current-buffer b
        (setq entries
              (nconc entries
                     (counsel-outline-candidates
                      (cdr (assq 'org-mode counsel-outline-settings))
                      (counsel-org-goto-all--outline-path-prefix))))))
    entries))


(cl-defun hax/org-select-subtree-callback
    (prompt callback caller &optional
            (entries (org-collect-known-entries))
            (history 'counsel-org-goto-history))
  "Select subtree from ENTRIES and execute CALLBACK on the selected result."
  (interactive)
  (hax/log "%s" entries)
  (let* ((choice
          (completing-read
           prompt
           entries
           nil
           t
           nil
           history))
         (result (assoc choice entries)))
    (funcall callback result)))


(cl-defun hax/org-select-subtree (&optional
                                  (entries (org-collect-known-entries))
                                  (history 'counsel-org-goto-history))
  "Interactively select subtree and return cons with `(description . marker)'"
  (interactive)
  (let (result)
    (hax/org-select-subtree-callback
     "Select: "
     ;; IMPORTANT in order for this hack to work, lexical binding must be
     ;; enabled.
     (lambda (x) (setq result x))
     'hax/org-select-subtree
     entries
     history)
    result))

(defun hax/org-collect-active-entries (&optional with-todo)
  "Get list of the 'targetable' entries, sorted by last clock-in time."
  (let ((begin-7 (ts-adjust 'day -7 (ts-now)))
        (end+7 (ts-adjust 'day +7 (ts-now)))
        (begin-1 (ts-adjust 'day -1 (ts-now)))
        (end+1 (ts-adjust 'day +1 (ts-now))))
    (--map
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
        marker))
     (-map
      #'cdr
      (-sort
       (lambda (a b)
         (time-less-p
          (or (car b) (seconds-to-time 0))
          (or (car a) (seconds-to-time 0))))
       ;; TODO Extract this into a separate function for getting the
       ;; last clocked time in a subtree. 
       (--map
        (let* ((marker (org-element-property :org-marker it))
               (clock (org-with-point-at marker
                        (org-clock-sum-current-item)
                        (save-excursion
                          (goto-char marker)
                          (let ((latest nil))
                            (org-element-map (org-element-at-point) 'clock
                              (lambda (cl)
                                (let ((value (org-element-property :value cl)))
                                  (when (and value (string-match "\\[\\([^]]+\\)\\]" value))
                                    (let ((ts (org-time-string-to-time
                                               (match-string 1 value))))
                                      (when (or (null latest)
                                                (time-less-p latest ts))
                                        (setq latest ts))))))
                              nil nil 'clock)
                            latest)))))
          (cons clock it))
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
          :action 'element-with-markers)))))))

(defun hax/org-goto-select-active-subtree ()
  "Interactively select active subtree and return position of the marker for it"
  (interactive)
  (goto-marker (cdr (hax/org-select-subtree
                     (hax/org-collect-active-entries t)
                     nil))))


(cl-defun hax/org-goto-select-subtree (&optional (entries (org-collect-known-entries)))
  "Interactively select subtree and return position of the marker for it"
  (interactive)
  (goto-marker (cdr (hax/org-select-subtree entries))))


(defun hax/org-collect-repeated-entries ()
  (save-window-excursion
    (save-excursion
      (--map
       (car it)
       (--filter
        (and (cdr it) (s-contains? "+" (cdr it)))
        (-map (lambda (tree)
                (goto-marker (cdr tree))
                (cons
                 tree
                 (and
                  (org-get-todo-state)
                  (-contains? '("TODO" "WIP" "POSTPONED" "NEXT")
                              (remove-string-properties (org-get-todo-state)))
                  (or (org-entry-get (point) "DEADLINE")
                      (org-entry-get (point) "SCHEDULED")))))
              (org-collect-known-entries)))))))

(defun hax/org-action-interactively (action &optional target)
  "Execution ACTION (function with no arguments) on the selected subtree position"
  (save-excursion
    (goto-marker (cdr (hax/org-select-subtree
                       (pcase target
                         ('repeated (hax/org-collect-repeated-entries))
                         ('active (hax/org-collect-active-entries t))
                         (_ (org-collect-known-entries))))))
    (funcall action)))

(defun hax/ensure-todo (state)
  (unless (s-equals? (substring-no-properties (org-get-todo-state)) state) (org-todo state)))

(defun hax/org-clock-in-interactively (&optional target)
  "Interactively select target to clock in using
`hax/org-collect-active-entries'"
  (interactive)
  (save-window-excursion
    (save-excursion
      (hax/org-action-interactively
       (lambda () (org-clock-in) (hax/ensure-todo "WIP"))
       target))))

(cl-defun hax/org-complete-interactively (&optional (state "COMPLETED") target)
  "Interactively select target to complete using
`hax/org-collect-active-entries'"
  (interactive)
  (save-window-excursion
    (save-excursion
      (hax/org-action-interactively
       (lambda () (hax/ensure-todo state))
       target))))
