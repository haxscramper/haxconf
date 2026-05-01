;;; -*- lexical-binding: t; -*-

(require 'json)
(require 'subr-x)
(require 'cl-lib)
(require 'org)

(defvar hax/code-link-index-cache (make-hash-table :test #'equal)
  "Cache of parsed code index data keyed by JSON file path.")

(defun hax/code-link-clear-cache (&optional json-file)
  "Clear cached parsed code index data.
If JSON-FILE is non-nil, clear only that entry."
  (interactive)
  (if json-file
      (remhash (expand-file-name json-file) hax/code-link-index-cache)
    (clrhash hax/code-link-index-cache)))

(defun hax/code-link--read-json-file (json-file)
  "Read and parse JSON-FILE."
  (let ((json-object-type 'alist)
        (json-array-type 'list)
        (json-key-type 'symbol)
        (file (expand-file-name json-file)))
    (json-read-file file)))

(defun hax/code-link--cached-index (json-file)
  "Return cached parsed representation for JSON-FILE."
  (let* ((file (expand-file-name json-file))
         (attrs (file-attributes file))
         (mtime (file-attribute-modification-time attrs))
         (cached (gethash file hax/code-link-index-cache)))
    (if (and cached (equal (alist-get 'mtime cached) mtime))
        (alist-get 'data cached)
      (let ((data (hax/code-link--read-json-file file)))
        (puthash file `((mtime . ,mtime) (data . ,data)) hax/code-link-index-cache)
        data))))

(defun hax/code-link--range-string (range)
  "Convert RANGE alist to line range string."
  (let ((start (alist-get 'start_line range))
        (end (alist-get 'end_line range)))
    (cond
     ((and start end (= start end))
      (number-to-string start))
     ((and start end)
      (format "%d-%d" start end))
     (start
      (number-to-string start))
     (t
      ""))))

(defun hax/code-link--display-label (kind file name &optional class)
  "Build human-readable display label."
  (pcase kind
    ("function" (format "%s :: %s()" file name))
    ("class"    (format "%s :: %s" file name))
    ("method"   (format "%s :: %s#%s()" file class name))
    ("field"    (format "%s :: %s#%s" file class name))
    ("signal"   (format "%s :: %s#%s" file class name))
    (_          (format "%s :: %s" file name))))

(defun hax/code-link--make-link (file range entry-spec)
  "Build code: link target."
  (format "code:%s:%s:%s" file range entry-spec))

(defun hax/code-link--collect-from-file (file-entry)
  "Collect completion candidates from a file entry."
  (let ((file (alist-get 'path file-entry))
        (result '()))
    ;; top-level functions
    (dolist (fn (alist-get 'functions file-entry))
      (let* ((name (alist-get 'name fn))
             (range (hax/code-link--range-string (alist-get 'range fn)))
             (entry-spec (format "%s()" name))
             (target (hax/code-link--make-link file range entry-spec))
             (display (hax/code-link--display-label "function" file name)))
        (push (cons display target) result)))
    ;; classes + class members
    (dolist (cls (alist-get 'classes file-entry))
      (let* ((class-name (alist-get 'name cls))
             (class-range (hax/code-link--range-string (alist-get 'range cls)))
             (class-target (hax/code-link--make-link file class-range class-name))
             (class-display (hax/code-link--display-label "class" file class-name)))
        (push (cons class-display class-target) result)

        (dolist (field (alist-get 'fields cls))
          (let* ((field-name (alist-get 'name field))
                 (field-range (hax/code-link--range-string (alist-get 'range field)))
                 (entry-spec (format "%s#%s" class-name field-name))
                 (target (hax/code-link--make-link file field-range entry-spec))
                 (display (hax/code-link--display-label "field" file field-name class-name)))
            (push (cons display target) result)))

        (dolist (signal (alist-get 'signals cls))
          (let* ((signal-name (alist-get 'name signal))
                 (signal-range (hax/code-link--range-string (alist-get 'range signal)))
                 (entry-spec (format "%s#%s" class-name signal-name))
                 (target (hax/code-link--make-link file signal-range entry-spec))
                 (display (hax/code-link--display-label "signal" file signal-name class-name)))
            (push (cons display target) result)))

        (dolist (method (alist-get 'methods cls))
          (let* ((method-name (alist-get 'name method))
                 (method-range (hax/code-link--range-string (alist-get 'range method)))
                 (entry-spec (format "%s#%s()" class-name method-name))
                 (target (hax/code-link--make-link file method-range entry-spec))
                 (display (hax/code-link--display-label "method" file method-name class-name)))
            (push (cons display target) result)))))
    (nreverse result)))

(defun hax/code-link--collect-entries (entry)
  "Recursively collect completion candidates from ENTRY."
  (let ((kind (alist-get 'kind entry))
        (result '()))
    (cond
     ((equal kind "file")
      (setq result (append result (hax/code-link--collect-from-file entry))))
     ((equal kind "directory")
      (dolist (child (alist-get 'children entry))
        (setq result (append result (hax/code-link--collect-entries child))))))
    result))

(defun hax/code-link--all-candidates (json-file)
  "Return all completion candidates from JSON-FILE."
  (let* ((data (hax/code-link--cached-index json-file))
         (entries (alist-get 'entries data))
         (result '()))
    (dolist (entry entries)
      (setq result (append result (hax/code-link--collect-entries entry))))
    result))

(defun hax/code-link-insert-org-link (json-file)
  "Insert an org-mode code: link selected from JSON-FILE.

Interactively prompts for JSON-FILE and then a target using `completing-read`.
If description is left empty, insert a bare org link without link text."
  (interactive "fCode index JSON file: ")
  (let* ((candidates (hax/code-link--all-candidates json-file))
         (choice (completing-read "Code target: " (mapcar #'car candidates) nil t))
         (target (cdr (assoc choice candidates)))
         (desc (read-string "Description (empty for none): ")))
    (insert
     (if (string-empty-p desc)
         (format "[[%s]]" target)
       (format "[[%s][%s]]" target desc)))))
