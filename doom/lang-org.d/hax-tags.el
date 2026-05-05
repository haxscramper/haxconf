;;; -*- lexical-binding: t; -*-

(defvar hax/org-tags-docs (make-hash-table) "Documentation for some of the used org-mode tags")



(defun hax/select-tag (action)
  (interactive)
  (let* ((org-last-tags-completion-table
          (append (and (or org-complete-tags-always-offer-all-agenda-tags
                           (eq major-mode 'org-agenda-mode))
                       (org-global-tags-completion-table
                        (org-agenda-files)))
                  (unless (boundp 'org-current-tag-alist)
                    org-tag-persistent-alist)
                  org-tag-alist))
         (selected
          (completing-read
           (counsel-org-tag-prompt)
           (delete-dups
            (all-completions "" #'org-tags-completion-function))
           nil
           nil
           nil
           'org-tags-history)))
    (when action
      (hax/log "Running tag action %s" action)
      (funcall action selected))
    (unless (--any (s-equals? (car it) selected) org-tag-alist)
      (let* ((is-private (--any (s-prefix? it selected) hax/private-tags-prefix-list))
             (file (if is-private hax/private-tags-file hax/tags-file)))
        (setq org-tag-alist (push (cons selected ??) org-tag-alist))
        (f-write-text
         (s-join "\n" (sort (mapcar (lambda (it) (concat "#" (car it))) org-tag-alist) 's-less?))
         'utf-8
         file)
        (hax/log
         "New %s tag %s"
         (if is-private "private" "public")
         (propertize selected 'face `(:foreground ,(doom-color 'red))))))
    selected))

(when t
  (setq
   tmp
   '(("code" . "Markup and programming languages")
     ("idea" . "Might be turned into todo, but probably won't be")
     ("programming" .
      ("Programming-related activities" .
       (("gui" . "Writing graphical applications"))))
     ("software" . "About different programs")))
  (setq hax/org-tags-docs (make-hash-table :test 'equal))
  (defun hax/tmp/rec-build-org-tag-docs (item &optional prefix)
    (dolist (i item)
      (let ((value (cdr i))
            (key (if prefix (concat prefix (car i)) (car i))))
        (if (listp value)
            (progn (puthash key (car value) hax/org-tags-docs)
                   (hax/tmp/rec-build-org-tag-docs
                    (cdr value)
                    (format "%s##" (car i))))
          (puthash key value hax/org-tags-docs)))))

  (hax/tmp/rec-build-org-tag-docs tmp)
  (gethash "programming##gui" hax/org-tags-docs))

(defun hax/insert-logbook-tag-entry (tag-name action)
  "Insert a logbook entry with TAG-NAME and ACTION ('added or 'removed) into the subtree logbook."
  (let ((current-time (format-time-string (org-time-stamp-format t t))))
    (hax/org-add-log-entry
     (format "- Tag \"%s\" %s on %s"
             (if (s-starts-with? "@" tag-name) tag-name (s-concat "#" tag-name))
             (if (eq action 'added) "Added" "Removed")
             current-time) 
     "LOGBOOK")))

(defun hax/org-assign-tag ()
  "Add or remove tags in `org-mode'. If new tag is added, store
it in the persistent list of tags, and update current list of tags"
  (interactive)
  (save-excursion
    (if (eq major-mode 'org-agenda-mode)
        (if org-agenda-bulk-marked-entries
            (setq counsel-org-tags nil)
          (let ((hdmarker (or (org-get-at-bol 'org-hd-marker)
                              (org-agenda-error))))
            (with-current-buffer (marker-buffer hdmarker)
              (goto-char hdmarker)
              (setq counsel-org-tags (counsel--org-get-tags)))))
      (unless (org-at-heading-p)
        (org-back-to-heading t))
      (setq counsel-org-tags (counsel--org-get-tags)))
    (hax/select-tag #'hax/counsel-org-tag-action)))


