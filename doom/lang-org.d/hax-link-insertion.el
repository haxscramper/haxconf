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

(cl-defun hax/org-insert-footnote (footnote &optional (goto-created t))
  (interactive "sfootnote name: ")
  (insert (format "[fn:%s]" footnote))
  (let* ((real-start (point)))
    (goto-char (hax/org-before-logical-end))
    (insert (format "[fn:%s] " footnote))
    (evil-insert-state)
    (save-excursion (insert "\n\n"))
    (when (not goto-created) (goto-char real-start))))

(defun hax/org-insert-link (type with-description)
  (let* ((target-base (pcase type
                        ('file (counsel-find-file))
                        ('attachment (counsel-find-file))
                        ('id (hax/org-select-subtree))
                        ('tag (hax/select-tag nil))
                        ('person (hax/select-from-list-or-add "person"))
                        ('organization (hax/select-from-list-or-add "organization"))
                        (_ (error (format "Unexpected type %s" type)))))

         (target (pcase type
                   ('id (hax/get-subtree-id-for-marker (cdr target-base)))
                   (_ target-base)))

         (default-desc (pcase type
                         ('id (save-window-excursion
                                (save-excursion
                                  (org-goto-marker-or-bmk (cdr target-base))
                                  (substring-no-properties (org-get-heading t t t t)))))
                         ('person "")
                         ('tag "")
                         (_ (file-name-nondirectory target))))

         (description (if with-description
                          (read-string "Description: "
                                       default-desc)
                        "")))

    (pcase type
      ('tag (insert target))
      (_ (if (string-empty-p description)
             (insert (format "[[%s:%s]]" (symbol-name type) target))
           (insert (format "[[%s:%s][%s]]" (symbol-name type) target description)))))))


(defhydra hydra-insert-link (:color blue :hint nil)
  "
  Insert Link:
  _f_: file         _F_: file
  _a_: attachment   _A_: attachment
  _i_: ID           _I_: ID
  _t_: Tag          _T_: Tag
  _p_: Person       _P_: Person
  _o_: Organization _O_: Organization
  "
  ("f" (hax/org-insert-link 'file nil))
  ("F" (hax/org-insert-link 'file t))
  ("a" (hax/org-insert-link 'attachment nil))
  ("A" (hax/org-insert-link 'attachment t))
  ("i" (hax/org-insert-link 'id nil))
  ("I" (hax/org-insert-link 'id t))
  ("t" (hax/org-insert-link 'tag nil))
  ("T" (hax/org-insert-link 'tag t))
  ("p" (hax/org-insert-link 'person nil))
  ("P" (hax/org-insert-link 'person t))
  ("o" (hax/org-insert-link 'organization nil))
  ("O" (hax/org-insert-link 'organization t))
  ("q" nil "cancel"))
