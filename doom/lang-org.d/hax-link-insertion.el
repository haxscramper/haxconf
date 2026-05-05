;;; -*- lexical-binding: t; -*-

(defun hax/insert-subtree-link-cb
    (tree-car description last-n with-tags cleanup-name-formatting)
  ;; If function is called with prefix value
  (let* ((name (hax/org-outline-path-at-marker
                (cdr tree-car)
                last-n
                with-tags
                cleanup-name-formatting))
         ;; Go to position of the found entry, get IT's id (creating
         ;; one if it is missing), and then insert result into the
         ;; final output
         (id (hax/get-subtree-id-for-marker (cdr tree-car))))
    (insert (format "[[id:%s][%s]]" id (if description description name)))))

(cl-defun hax/org-insert-link-to-subtree
    (&key description
          last-n
          (entries (org-collect-known-entries))
          (with-tags nil)
          (cleanup-name-formatting t))
  "Insert link to any subtree in any org file, using
  `[[path][ID]]' link. If DESCRIPTION is `nil', use the heading
  name, otherwise use description."
  (interactive)
  (hax/org-select-subtree-callback
   "Goto: "
   (lambda (x) (hax/insert-subtree-link-cb
                x description last-n with-tags cleanup-name-formatting))
   'hax/org-insert-link-to-heading
   entries))

(defun hax/org-insert-clipboard-link (&optional description)
  ;; FIXME https://astralcodexten.substack.com/p/heuristics-that-almost-always-work?s=r
  (interactive)
  (let ((run-again t))
    (while run-again
      (let* ((link (simpleclip-get-contents))
             (name (if description description
                     (when link (www-get-page-title link))))
             (org-link (format "[[%s][%s]]" link name))
             (read-answer-short t)
             (selected
              (if name
                  (read-answer
                   (format "Insert '%s" org-link)
                   '(("yes" ?y "insert link")
                     ("no" ?n "do not insert link")
                     ("update" ?u "update link from clipboard")
                     ("help" ?h "show help")
                     ("edit" ?e "prompt for new link name")
                     ("quit" ?q "quit")))
                (warn "Bad url: %s" link)
                (setq run-again nil)
                nil)))
        (setq description name)
        (cond
         ((string= selected "yes") (insert org-link) (setq run-again nil))
         ((string= selected "no") (setq run-again nil))
         ((string= selected "edit") (setq description
                                          (read-string "New link name: ")))
         ((string= selected "update") t)
         ((string= selected "help") (setq run-again nil))
         ((string= selected "quit") (setq run-again nil)))))))

(defun hax/org-insert-footnote-link (footnote)
  "Interactively insert trailing footnote and link with specified
  FOOTNOTE as a name"
  (interactive "sfootnote name: ")
  (insert (format "[fn:%s]" footnote))
  (let* ((at (org-element-at-point))
         (tree (if (eq 'headline (car at))
                   at
                 (org-element-property :parent at))))
    (save-excursion
      (goto-char (org-element-property :end tree))
      (insert (format "\n[fn:%s] " footnote))
      (hax/org-insert-clipboard-link)
      (insert "\n\n"))))
