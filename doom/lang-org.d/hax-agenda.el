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
