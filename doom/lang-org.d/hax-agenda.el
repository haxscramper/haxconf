;;; -*- lexical-binding: t; -*-


(defun hax/org-ql--entry-last-clocked-in ()
  (org-with-wide-buffer
   (save-excursion
     (let* ((subtree-end (save-excursion (org-end-of-subtree t t)))
            (latest nil)
            (parse-tree
             (org-element-parse-region
              (line-beginning-position)
              subtree-end
              'greater-element)))
       (org-element-map parse-tree 'clock
         (lambda (clock)
           (let* ((value (org-element-property :value clock))
                  (start (and value (car value))))
             (when (and start
                        (or (null latest) (time-less-p latest start)))
               (setq latest start)))))
       latest))))


(defun hax/org-time-to-iso-8601 (time)
  (when time
    (format-time-string "%Y-%m-%dT%H:%M:%S%z")))

(defun hax/org-ql--entry-created-time ()
  (when-let ((created (org-entry-get (point) "CREATED")))
    (org-time-string-to-time created)))

(defun hax/org-ql--entry-path ()
  (org-get-outline-path t t))

(defun hax/org-ql-entry-plist ()
  (let* ((title (substring-no-properties (org-get-heading t t t t)))
         (created (hax/org-ql--entry-created-time))
         (last-clocked-in (hax/org-ql--entry-last-clocked-in))
         (overall-minutes (org-clock-sum-current-item))
         (todo-state (and (org-get-todo-state)
                          (substring-no-properties (org-get-todo-state))))
         (tags (mapcar #'substring-no-properties (org-get-tags nil t)))
         (file (and (buffer-file-name)
                    (substring-no-properties (buffer-file-name))))
         (path (mapcar #'substring-no-properties (hax/org-ql--entry-path))))
    (list
     :title (substring-no-properties title)
     :created (and created (hax/org-time-to-iso-8601 created))
     :last-clocked-in (hax/org-time-to-iso-8601 last-clocked-in)
     :overall-time overall-minutes
     :todo-state (substring-no-properties todo-state)
     :subtree-tags tags
     :file (substring-no-properties file)
     :subtree-path path)))

(defun hax/org-ql-collect-groups (groups)
  (mapcar
   (lambda (group)
     (let* ((header (plist-get group :header))
            (files (plist-get group :files))
            (query (plist-get group :query))
            (sort (plist-get group :sort))
            (entries
             (org-ql-select files query
               :sort sort
               :action #'hax/org-ql-entry-plist)))
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
           :query (todo "TODO"))))
  )
