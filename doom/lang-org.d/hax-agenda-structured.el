;;; -*- lexical-binding: t; -*-

(require 'org)
(require 'org-element)
(require 'org-clock)
(require 'org-ql)
(require 'json)
(require 'cl-lib)

(defun hax/org-time-to-iso-8601 (time)
  (when time
    (format-time-string "%Y-%m-%dT%H:%M:%S%z" time)))

(defun hax/org--timestamp-repeat (time-string)
  (when (and time-string
             (string-match "\\s-+\\([.+]?\\+[0-9]+[hdwmy]\\)" time-string))
    (match-string 1 time-string)))

(defun hax/org-timestamp-string-to-plist (time-string)
  (when time-string
    (list
     :timestamp (hax/org-time-to-iso-8601 (org-time-string-to-time time-string))
     :repeat (hax/org--timestamp-repeat time-string))))

(defun hax/org-entry-timestamp-plist (marker property)
  (org-with-point-at marker
    (hax/org-timestamp-string-to-plist (org-entry-get nil property))))

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

(defun hax/org-element--clock-end-time (clock)
  (let ((clock-text
         (buffer-substring-no-properties
          (org-element-property :begin clock)
          (org-element-property :end clock))))
    (when (string-match "--\\(\\[[^]]+\\]\\|<[^>]+>\\)" clock-text)
      (org-time-string-to-time (match-string 1 clock-text)))))

(defun hax/org-element--latest-clock-end-value (_headline marker)
  (let ((latest nil))
    (org-with-point-at marker
      (save-excursion
        (org-back-to-heading t)
        (let ((end (save-excursion (org-end-of-subtree t t)))
              (case-fold-search nil))
          (when (re-search-forward "^[ \t]*:LOGBOOK:[ \t]*$" end t)
            (let ((logbook-begin (match-end 0))
                  (logbook-end
                   (save-excursion
                     (when (re-search-forward "^[ \t]*:END:[ \t]*$" end t)
                       (match-beginning 0)))))
              (when logbook-end
                (goto-char logbook-begin)
                (while (re-search-forward
                        "^[ \t]*CLOCK:.*?--\\(\\[[^]\n]+\\]\\)[ \t]*=>"
                        logbook-end
                        t)
                  (let* ((end-string (match-string-no-properties 1))
                         (end-time (org-time-string-to-time end-string)))
                    (when (or (null latest) (time-less-p latest end-time))
                      (setq latest end-time))))))))))
    latest))

(defun hax/org-element-entry-plist (headline)
  (let* ((marker (org-element-property :org-marker headline))
         (priority-value (org-element-property :priority headline)))
    (list
     :title (substring-no-properties
             (or (org-element-property :raw-value headline) ""))
     :created (hax/org-entry-timestamp-plist marker "CREATED")
     :deadline (hax/org-entry-timestamp-plist marker "DEADLINE")
     :scheduled (hax/org-entry-timestamp-plist marker "SCHEDULED")
     :last-clocked-in (hax/org-time-to-iso-8601
                       (hax/org-element--latest-clock-end-value headline marker))
     :overall-time (org-with-point-at marker
                     (org-clock-sum-current-item))
     :todo-state (when-let ((todo (org-element-property :todo-keyword headline)))
                   (substring-no-properties todo))
     :subtree-priority (when priority-value
                         (char-to-string priority-value))
     :subtree-tags (mapcar #'substring-no-properties
                           (org-element-property :tags headline))
     :file (substring-no-properties
            (or (buffer-file-name (marker-buffer marker)) ""))
     :subtree-path (hax/org-element--headline-path headline)
     :subtree-line (org-with-point-at marker
                     (line-number-at-pos)))))

(defun hax/org-ql-collect-groups (groups)
  (mapcar
   (lambda (group)
     (let* ((header (plist-get group :header))
            (files (plist-get group :files))
            (query (plist-get group :query))
            (sort (plist-get group :sort))
            (entries
             (mapcar
              #'hax/org-element-entry-plist
              (org-ql-select files query
                :sort sort
                :action 'element-with-markers))))
       (list
        :header header
        :entries entries)))
   groups))

(defun hax/detail/configure-org-agenda-query ()
  (setq hax/org-ql-dashboard-groups
        ))

(defun hax/json-normalize (value)
  (cond
   ((null value)
    nil)

   ((and (listp value)
         (keywordp (car value)))
    (cl-loop for (key val) on value by #'cddr
             collect
             (cons (substring (symbol-name key) 1)
                   (hax/json-normalize val))))

   ((listp value)
    (mapcar #'hax/json-normalize value))

   (t
    value)))

(defun hax/get-agenda-json ()
  (json-encode
   (hax/json-normalize
    (hax/org-ql-collect-groups
     `((:header "In progress (NEXT/WIP/PAUSED/BLOCKED)"
        :files ,(org-agenda-files)
        :query (todo "NEXT" "WIP" "PAUSED" "BLOCKED"))

       (:header "Staging todo"
        :files (,(expand-file-name hax/staging.org))
        :query (todo "TODO"))

       (:header "Notes & High priority project todos"
        :files (,(expand-file-name hax/notes.org)
                ,(expand-file-name hax/projects.org))
        :query (and
                (todo "TODO")
                (or
                 (and
                  (path ,(expand-file-name hax/notes.org))
                  )
                 (and
                  (path ,(expand-file-name hax/projects.org))
                  (or
                   (priority "X")
                   (priority "A")
                   (priority "S")
                   )
                  ))))

       ;; (:header "2-week preview"
       ;;  :files ,(org-agenda-files)
       ;;  :query (and
       ;;          (not (todo "COMPLETED"))
       ;;          (not (todo "DONE"))
       ;;          (not (todo "CANCELLED"))
       ;;          (or
       ;;           (scheduled :from today :to 14)
       ;;           (deadline :from today :to 14)
       ;;           (ts :from today :to 14)) ))

       (:header "Repeated todos"
        :files (,(expand-file-name hax/repeated.org))
        :query (todo "TODO")))))))

(defun hax/rofi-select-agenda-subtree ()
  (interactive)
  (let* ((json-file "/tmp/hax-agenda-rofi.json")
         (json-object-type 'alist)
         (json-array-type 'list)
         (json-key-type 'string))
    (with-temp-buffer
      (insert (hax/get-agenda-json))
      (json-pretty-print-buffer)
      (write-region nil nil json-file nil 'silent))
    (let ((selected
           (string-trim-right
            (with-temp-buffer
              (call-process "hax_org_agenda.py" nil t nil json-file)
              (buffer-string)))))
      (hax/log "selected [%s]" selected)
      (unless (string-empty-p selected)
        (let* ((item (json-read-from-string selected))
               (file (alist-get "file" item nil nil #'string=))
               (line (alist-get "subtree-line" item nil nil #'string=)))
          (when (and file line)
            (find-file file)
            (goto-char (point-min))
            (forward-line (1- line))
            (org-show-context)
            (org-show-entry)
            (recenter)))))))

(when nil
  (write-to-file-unquoted "/tmp/hax-emacs.log" "")
  (let ((json-object-type 'alist)
        (json-array-type 'list)
        (json-key-type 'string))
    (with-temp-buffer
      (insert (hax/get-agenda-json))
      (json-pretty-print-buffer)
      (write-region nil nil "/tmp/result.json")))
  (hax/rofi-select-agenda-subtree))

