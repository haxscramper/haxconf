;;; -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'org)
(require 'project)
(require 'sqlite)
(require 'subr-x)

(defvar hax/code-link-active-index-file nil
  "Currently active code index sqlite file path.")

(defvar hax/code-link-index-cache (make-hash-table :test #'equal)
  "Cache of parsed code index rows keyed by sqlite file path.")

(defun hax/code-link-clear-cache (&optional sqlite-file)
  (interactive)
  (if sqlite-file
      (remhash (expand-file-name sqlite-file) hax/code-link-index-cache)
    (clrhash hax/code-link-index-cache)))

(defun hax/code-link--project-root ()
  (when-let ((proj (project-current nil)))
    (expand-file-name (project-root proj))))

(defun hax/code-link--find-index-upward ()
  (let* ((start-dir (expand-file-name
                     (if buffer-file-name
                         (file-name-directory buffer-file-name)
                       default-directory)))
         (project-root (hax/code-link--project-root))
         (limit-dir (if project-root
                        (file-name-as-directory project-root)
                      nil))
         (dir (file-name-as-directory start-dir))
         (found nil)
         (done nil))
    (while (not done)
      (let ((candidate (expand-file-name ".haxscramper-code-index.sqlite" dir)))
        (if (file-exists-p candidate)
            (setq found candidate
                  done t)
          (let ((parent (file-name-directory (directory-file-name dir))))
            (if (or (null parent)
                    (equal parent dir)
                    (and limit-dir (equal dir limit-dir)))
                (setq done t)
              (setq dir (file-name-as-directory parent)))))))
    found))

(defun hax/code-link--project-index-files ()
  (when-let ((root (hax/code-link--project-root)))
    (sort
     (directory-files-recursively
      root
      "\\.haxscramper-code-index.*\\.sqlite\\'")
     #'string<)))

(defun hax/code-link-select-index-file ()
  (interactive)
  (let* ((root (or (hax/code-link--project-root) default-directory))
         (selected (read-file-name "Code index sqlite: " root nil t)))
    (setq hax/code-link-active-index-file (expand-file-name selected))
    hax/code-link-active-index-file))

(defun hax/code-link-select-index-from-project ()
  (interactive)
  (let ((files (hax/code-link--project-index-files)))
    (if (null files)
        nil
      (condition-case nil
          (let* ((root (hax/code-link--project-root))
                 (choices (mapcar (lambda (f) (file-relative-name f root)) files))
                 (picked (completing-read "Project code index: " choices nil t))
                 (resolved (expand-file-name picked root)))
            (setq hax/code-link-active-index-file resolved)
            resolved)
        (quit nil)))))

(defun hax/code-link--find-index-in-known-projects ()
  (let* ((current-root (hax/code-link--project-root))
         (projects
          (delete-dups
           (delq nil
                 (append
                  (when (boundp 'projectile-known-projects)
                    projectile-known-projects)
                  (when (fboundp 'project-known-project-roots)
                    (project-known-project-roots)))))))
    (delq
     nil
     (mapcar
      (lambda (root)
        (let ((root (expand-file-name root)))
          (unless (and current-root
                       (equal (file-name-as-directory root)
                              (file-name-as-directory current-root)))
            (let ((matches
                   (sort
                    (directory-files-recursively
                     root
                     "\\(?:\\.haxscramper-code-index\\.sqlite\\|\\.haxscramper-code-index.*\\.sqlite\\)\\'")
                    #'string<)))
              (when matches
                (cons root matches))))))
      projects))))

(defun hax/code-link-select-index-from-known-projects ()
  (interactive)
  (let ((projects (hax/code-link--find-index-in-known-projects)))
    (when projects
      (condition-case nil
          (if (= (length projects) 1)
              (let ((files (cdar projects)))
                (setq hax/code-link-active-index-file
                      (if (= (length files) 1)
                          (car files)
                        (let* ((root (caar projects))
                               (choices (mapcar (lambda (f) (file-relative-name f root)) files))
                               (picked (completing-read
                                        (format "Code index from %s: "
                                                (file-name-nondirectory
                                                 (directory-file-name root)))
                                        choices nil t)))
                          (expand-file-name picked root)))))
            (let* ((choices
                    (mapcar
                     (lambda (entry)
                       (let* ((root (car entry))
                              (files (cdr entry))
                              (label (file-name-nondirectory
                                      (directory-file-name root))))
                         (cons
                          (if (= (length files) 1)
                              (format "%s: %s"
                                      label
                                      (file-relative-name (car files) root))
                            (format "%s (%d indexes)" label (length files)))
                          entry)))
                     projects))
                   (picked-project
                    (cdr (assoc (completing-read "Project code index: "
                                                 choices nil t)
                                choices)))
                   (root (car picked-project))
                   (files (cdr picked-project)))
              (setq hax/code-link-active-index-file
                    (if (= (length files) 1)
                        (car files)
                      (let* ((file-choices
                              (mapcar (lambda (f) (file-relative-name f root)) files))
                             (picked-file
                              (completing-read
                               (format "Index from %s: "
                                       (file-name-nondirectory
                                        (directory-file-name root)))
                               file-choices nil t)))
                        (expand-file-name picked-file root)))))
            (quit nil))))))

(defun hax/code-link-resolve-active-index ()
  (interactive)
  (let ((upward (hax/code-link--find-index-upward)))
    (setq hax/code-link-active-index-file
          (or upward
              (hax/code-link-select-index-from-project)
              (hax/code-link-select-index-from-known-projects)
              (hax/code-link-select-index-file)))))

(defun hax/code-link--read-sqlite-rows (sqlite-file)
  (let ((db (sqlite-open (expand-file-name sqlite-file))))
    (unwind-protect
        (sqlite-select
         db
         (concat
          "SELECT entry_id, kind, language, path, qualified_name, "
          "flat_representation, doc_brief, start_line "
          "FROM entry_flat_view "
          "ORDER BY path, start_line, entry_id"))
      (sqlite-close db))))

(defun hax/code-link--cached-index (sqlite-file)
  (let* ((file (expand-file-name sqlite-file))
         (attrs (file-attributes file))
         (mtime (file-attribute-modification-time attrs))
         (cached (gethash file hax/code-link-index-cache)))
    (if (and cached (equal (alist-get 'mtime cached) mtime))
        (alist-get 'data cached)
      (let ((data (hax/code-link--read-sqlite-rows file)))
        (puthash file `((mtime . ,mtime) (data . ,data)) hax/code-link-index-cache)
        data))))

(defun hax/code-link--all-candidates (sqlite-file)
  (let ((rows (hax/code-link--cached-index sqlite-file))
        (result '()))
    (dolist (row rows (nreverse result))
      (pcase-let ((`(,entry-id ,kind ,language ,path ,qualified-name
                     ,flat-representation ,doc-brief ,start-line)
                   row))
        (let* ((line-str (if start-line (number-to-string start-line) ""))
               (display (format "%s:%s :: %s" path line-str flat-representation))
               (target (format "%s:%s:%s" path line-str flat-representation)))
          (push (list :entry-id entry-id
                      :kind kind
                      :language language
                      :qualified-name qualified-name
                      :display display
                      :target target
                      :doc-brief (or doc-brief ""))
                result))))))

(defun hax/code-link-get-org-link (&optional sqlite-file)
  (interactive)
  (let* ((index-file (or hax/code-link-active-index-file (hax/code-link-resolve-active-index)))
         (candidates (hax/code-link--all-candidates index-file))
         (doc-by-display (make-hash-table :test #'equal))
         (target-by-display (make-hash-table :test #'equal))
         (display-values '()))
    (dolist (cand candidates)
      (let ((display (plist-get cand :display))
            (doc (plist-get cand :doc-brief))
            (target (plist-get cand :target)))
        (push display display-values)
        (puthash display doc doc-by-display)
        (puthash display target target-by-display)))
    (setq display-values (nreverse display-values))
    (let* ((completion-extra-properties
            `(:annotation-function
              ,(lambda (cand)
                 (let ((doc (gethash cand doc-by-display "")))
                   (if (string-empty-p doc)
                       ""
                     (concat "  "
                             (truncate-string-to-width
                              (replace-regexp-in-string "[\n\t ]+" " " doc)
                              120 nil nil t)))))))
           (choice (completing-read "Code target: " display-values nil t))
           (target (gethash choice target-by-display)))
      target)))




(defun hax/--get-language ()
  "Extract the language name from the current `major-mode'.
Strips standard modes (-mode) and Doom's Tree-sitter modes (-ts-mode)."
  (let* ((mode-str (symbol-name major-mode))
         (lang (replace-regexp-in-string "-ts-mode$" "" mode-str))
         (lang (replace-regexp-in-string "-mode$" "" lang)))
    lang))

(defun hax/goto-end-of-last-non-empty-line ()
  "Move point to the end of the last non-empty line in the buffer.
A non-empty line is defined as a line containing at least one non-whitespace character."
  (interactive)
  (goto-char (point-max))
  (when (re-search-backward "\\S-" nil t) (end-of-line)))

(defun hax/--context-log-data ()
  "Collect the current source context and its metadata."
  (let* ((has-selection (use-region-p))
         (region-beg (when has-selection (region-beginning)))
         (region-end (when has-selection (region-end)))
         (selected-text
          (when has-selection
            (buffer-substring-no-properties region-beg region-end)))
         (leading-newline-length
          (if (and selected-text
                   (string-match "\\`[\n\r]+" selected-text))
              (match-end 0)
            0))
         (normalized-selection
          (when selected-text
            (string-trim selected-text "[\n\r]+" "[\n\r]+")))
         (multiline-selection
          (and normalized-selection
               (string-match-p "[\n\r]" normalized-selection)))
         (context-text
          (if has-selection
              normalized-selection
            (string-trim-left
             (buffer-substring-no-properties
              (line-beginning-position)
              (line-end-position)))))
         (content-beg
          (when region-beg
            (+ region-beg leading-newline-length)))
         (lang (hax/--get-language))
         (full-path (buffer-file-name))
         (fname
          (if full-path
              (let ((dir
                     (file-name-nondirectory
                      (directory-file-name
                       (file-name-directory full-path))))
                    (file (file-name-nondirectory full-path)))
                (concat dir "/" file))
            "unnamed-buffer"))
         (fline
          (if has-selection
              (line-number-at-pos content-beg)
            (line-number-at-pos)))
         (full-sha
          (condition-case nil
              (if (fboundp 'magit-rev-parse)
                  (magit-rev-parse "HEAD")
                (vc-git-working-revision full-path))
            (error nil)))
         (sha (if full-sha (substring full-sha 0 8) "N/A"))
         (timestamp (format-time-string "[%Y-%m-%d %a %H:%M:%S %Z]")))
    (list :text context-text
          :multiline multiline-selection
          :language lang
          :filename fname
          :line fline
          :sha sha
          :timestamp timestamp)))

(defun hax/--format-context-log-entry (context)
  "Format CONTEXT as an Org log entry."
  (let ((text (plist-get context :text))
        (multiline (plist-get context :multiline))
        (lang (plist-get context :language))
        (fname (plist-get context :filename))
        (fline (plist-get context :line))
        (sha (plist-get context :sha))
        (timestamp (plist-get context :timestamp)))
    (if multiline
        (format
         "- %s\n  #+caption: =%s:%s= at ~%s~\n  #+begin_src %s\n%s\n  #+end_src"
         timestamp fname fline sha lang text)
      (format
       "- %s src_%s{%s} in =%s:%s= at ~%s~"
       timestamp lang text fname fline sha))))

(defun hax/--append-context-to-scratch (entry)
  "Append formatted context ENTRY to the scratch Org file."
  (let* ((state-dir (expand-file-name "~/.local/state/hax/"))
         (org-file (expand-file-name "scratch.org" state-dir))
         (org-buffer
          (progn
            (make-directory state-dir t)
            (find-file-noselect org-file))))
    (with-current-buffer org-buffer
      (goto-char (point-max))
      (unless (or (= (point-min) (point-max))
                  (bolp))
        (insert "\n"))
      (insert entry "\n  - ")
      (save-buffer)
      (evil-insert 0))
    (pop-to-buffer org-buffer)))

(defun hax/copy-context-log-entry ()
  "Copy the formatted current context to the kill ring."
  (interactive)
  (kill-new
   (hax/--format-context-log-entry
    (hax/--context-log-data))))

(defun hax/log-context-to-scratch ()
  "Format the current context and append it to the scratch Org file."
  (interactive)
  (hax/--append-context-to-scratch
   (hax/--format-context-log-entry
    (hax/--context-log-data))))
