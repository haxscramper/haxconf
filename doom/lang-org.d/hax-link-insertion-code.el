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

(defun hax/code-link-resolve-active-index ()
  (interactive)
  (let ((upward (hax/code-link--find-index-upward)))
    (setq hax/code-link-active-index-file
          (or upward
              (hax/code-link-select-index-from-project)
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
               (target (format "code:%s:%s:%s" path line-str flat-representation)))
          (push (list :entry-id entry-id
                      :kind kind
                      :language language
                      :qualified-name qualified-name
                      :display display
                      :target target
                      :doc-brief (or doc-brief ""))
                result))))))

(defun hax/code-link-insert-org-link (&optional sqlite-file)
  (interactive)
  (let* ((index-file (or sqlite-file (hax/code-link-resolve-active-index)))
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
      (insert (format "[[%s]]" target)))))




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

(defun hax/log-context-to-scratch ()
  "Capture current context and append it to the *scratch* buffer.

If nothing is selected, use the full current line.
If a single-line range is selected, use the selected text inline.
If a multi-line range is selected, insert it as an Org source block."
  (interactive)
  (let* ((has-selection (use-region-p))
         (region-beg (when has-selection (region-beginning)))
         (region-end (when has-selection (region-end)))
         (selected-text
          (when has-selection
            (buffer-substring-no-properties region-beg region-end)))
         (multiline-selection
          (and selected-text (string-match-p "\n" selected-text)))
         (line-text
          (string-trim-left
           (buffer-substring-no-properties
            (line-beginning-position) (line-end-position))))
         (context-text
          (cond
           ((not has-selection) line-text)
           (multiline-selection selected-text)
           (t selected-text)))
         (lang (hax/--get-language))
         (fname (let* ((full-path (or (buffer-file-name) "unnamed-buffer"))
                       (dir (file-name-nondirectory
                             (directory-file-name (file-name-directory full-path))))
                       (file (file-name-nondirectory full-path)))
                  (concat dir "/" file)))
         (fline (if has-selection
                    (line-number-at-pos region-beg)
                  (line-number-at-pos)))
         (full-sha (condition-case nil
                       (if (fboundp 'magit-rev-parse)
                           (magit-rev-parse "HEAD")
                         (vc-git-working-revision (buffer-file-name)))
                     (error nil)))
         (sha (if full-sha (substring full-sha 0 8) "N/A"))
         (timestamp (format-time-string "[%Y-%m-%d %a %H:%M:%S %Z]"))
         (state-dir (expand-file-name "~/.local/state/hax/"))
         (org-file (expand-file-name "scratch.org" state-dir))
         (org-buffer (progn
                       (make-directory state-dir t)
                       (find-file-noselect org-file))))

    (with-current-buffer org-buffer
      (goto-char (point-max))
      (if multiline-selection
          (progn
            (insert (format "- %s\n" timestamp))
            (insert (format "  #+caption: =%s:%s= at ~%s~\n" fname fline sha))
            (insert (format "  #+begin_src %s\n" lang))
            (insert context-text)
            (unless (string-suffix-p "\n" context-text)
              (insert "\n"))
            (insert "  #+end_src\n"))
        (insert
         (format "- %s src_%s{%s} in =%s:%s= at ~%s~\n"
                 timestamp lang context-text fname fline sha)))
      (goto-char (point-max))
      (let ((line (buffer-substring-no-properties
                   (line-beginning-position) (line-end-position))))
        (unless (string= line "  - ")
          (delete-region (line-beginning-position) (line-end-position))
          (insert "  - ")))
      (evil-insert 0)
      (hax/goto-end-of-last-non-empty-line)
      (save-buffer))

    (pop-to-buffer org-buffer)))
