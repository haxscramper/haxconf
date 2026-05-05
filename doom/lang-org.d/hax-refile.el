;;; -*- lexical-binding: t; -*-

(defvar hax/org-refile-refiled-from-id nil)
(defvar hax/org-refile-refiled-from-header nil)
(defvar hax/org-refile-refiled-from-mark nil)
(defvar hax/org-refile-refiled-from-file nil)

(defun hax/org-save-source-id-and-header (&optional name file xxx pos)
  "Saves refile's source entry's id and header name to
  `hax/org-refile-refiled-from-id' and
  `hax/org-refile-refiled-from-header'. If refiling entry is
  first level entry then it stores file path and buffer name
  respectively."
  (interactive)
  (save-excursion
    (setq hax/org-refile-refiled-from-mark (point-marker))
    (setq hax/org-refile-refiled-from-file (buffer-file-name (buffer-base-buffer)))
    (if (org-up-heading-safe)
        ;; If we are in some outline, assign
        (progn
          (setq org-adapt-indentation t)
          ;; Store original node ID, optionally creating it if missing
          (setq hax/org-refile-refiled-from-id (cons 'id (org-id-get-create)))
          ;; Get original heading path, without tags, todo, priority elements
          (setq hax/org-refile-refiled-from-header
                (org-format-outline-path (org-get-outline-path t))))
      (setq hax/org-refile-refiled-from-id
            (cons 'file (buffer-file-name)))
      (setq hax/org-refile-refiled-from-header (buffer-name)))))

(defun hax/org-refile-add-refiled-from-note ()
  "Adds a note to entry at point on where the entry was refiled
  from using the org ID from `hax/org-refile-refiled-from-id'
  and `hax/org-refile-refiled-from-header' variables."
  (interactive)
  (save-excursion
    (when (and hax/org-refile-refiled-from-id
               hax/org-refile-refiled-from-header)
      (let* ((time-format (substring (cdr org-time-stamp-formats) 1 -1))
             (kind (car hax/org-refile-refiled-from-id))
             (src (cdr hax/org-refile-refiled-from-id))
             (time-stamp (format-time-string time-format (current-time))))
        (org-add-log-entry
         (format
          "- Refiled on [%s] from [[%s][%s:%s]]"
          time-stamp
          (if (eq kind 'id)
              (format "id:%s" src)
            (format "file:%s" (f-relative src (f-dirname (buffer-file-name)))))
          (f-base hax/org-refile-refiled-from-file)
          hax/org-refile-refiled-from-header)))
      (when hax/org-refile-refiled-from-mark
        (goto-marker hax/org-refile-refiled-from-mark)
        (setq hax/org-refile-refiled-from-mark nil))

      (setq hax/org-refile-refiled-from-file nil)
      (setq hax/org-refile-refiled-from-id nil)
      (setq hax/org-refile-refiled-from-header nil))))

(add-hook
 'org-after-refile-insert-hook
 #'hax/org-refile-add-refiled-from-note)

(advice-add
 'org-refile :before #'hax/org-save-source-id-and-header)

