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

(defun hax/disable-adapt-indentation (&rest args) (setq org-adapt-indentation nil))
(defun hax/enable-adapt-indentation (&rest args) (setq org-adapt-indentation t))

(advice-add 'org-refile :before #'hax/disable-adapt-indentation)
(advice-add 'org-refile :after #'hax/enable-adapt-indentation)


(defun hax/org-refile-marker-position (target at-start)
  (save-window-excursion
    (save-excursion
      ;; (hax/log "Marker target is %s" target)
      ;; (marker-position target)
      ;; (hax/log "Target original target buffer and position is %s:%s"
      ;;          (marker-buffer target) (marker-position target))
      (with-current-buffer (marker-buffer target)
        (goto-char (marker-position target))
        (hax/dbg/looking-around)
        (point)
        ;; (let* ((subtree (org-element-at-point))
        ;;        (final-position (org-element-property :contents-begin subtree)))
        ;;   (save-excursion
        ;;     (goto-char final-position)
        ;;     (hax/dbg/looking-around)
        ;;     final-position))
        ))))

(defun hax/org-refile-under-marker (target at-start)
  (let ((refile-list (list
                      (get-headline-from-marker target)
                      (buffer-file-name (marker-buffer target))
                      nil
                      (hax/org-refile-marker-position target at-start))))
    (hax/log "Reile list target %s" refile-list)
    (org-refile nil nil refile-list nil)))


(defun hax/org-refile-to-staging ()
  (interactive)
  (hax/org-refile-under-marker
   (get-capture-target-marker '(file hax/staging.org)) t))

(defun hax/org-refile-to-daily ()
  (interactive)
  (hax/org-refile-under-marker
   (get-capture-target-marker '(file+olp+datetree hax/notes.org)) t))



(defun org-refile-targets-all-files ()
  "Use all currently opened Org buffer files as org-refile targets."
  (mapcar 'buffer-file-name
          (seq-filter ; filter Org buffers
           (lambda (buffer)
             (if-let (file (buffer-file-name buffer))
                 (string-equal (file-name-extension file) "org")))
           (buffer-list))))

