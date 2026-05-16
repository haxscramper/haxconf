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

(cl-defun www-get-page-title (url &optional (timeout 15))
  "Return title of the URL page, or if not found the page's
  filename (as an approximation)"
  (hax/log "%s" timeout)
  (let ((title)
        (content (url-retrieve-synchronously url nil nil timeout)))
    (if (not content)
        (error "Failed to retrieve URL content")
      (with-current-buffer content
        (goto-char (point-min))
        (re-search-forward "<title>\\([^<]*\\)</title>" nil t 1)
        (setq title (match-string 1))
        (goto-char (point-min))
        (re-search-forward "charset=\\([-0-9a-zA-Z]*\\)" nil t 1)
        (if title
            (decode-coding-string
             title
             'utf-8
             ;; (pcase (match-string 1)
             ;;   ;; Safeguard against CAPS in names, maybe other weird coding
             ;;   ;; schemes.
             ;;   ("UTF-8" 'utf-8)
             ;;   (_ (intern (match-string 1))))
             )
          (url-filename (url-generic-parse-url url)))))))



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

(defun hax/org-gen-footnote-name ()
  (interactive)
  (point)
  (format
   "%s-%s"
   (s-replace-regexp
    (rx (not word))
    ""
    (s-trim (buffer-substring (point) (save-excursion (backward-word) (point)))))
   (line-number-at-pos)))

(cl-defun hax/org-insert-footnote (footnote &optional (goto-created t))
  (interactive "sfootnote name: ")
  (insert (format "[fn:%s]" footnote))
  (let* ((real-start (point)))
    (goto-char (hax/org-before-logical-end))
    (insert (format "[fn:%s] " footnote))
    (evil-insert-state)
    (save-excursion (insert "\n\n"))
    (when (not goto-created) (goto-char real-start))))

(defun hax/org-capture-vc-root-dir ()
  (if (bound-and-true-p org-capture-mode)
      (let ((buffer (org-capture-get :original-buffer)))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (vc-root-dir))))
    (vc-root-dir)))

(defun hax/select-from-list-or-add (list-name)
  "Select an item from a list of alternatives stored in ~/.config/targets/list-name.txt.
If the user inputs a new value, update the file and return it."
  (let* ((git-root (hax/org-capture-vc-root-dir))
         (filename (expand-file-name (f-join git-root (concat list-name ".txt"))))
         (existing-items
          (if (file-exists-p filename)
              (with-temp-buffer
                (insert-file-contents filename)
                (split-string (buffer-string) "\n" t))
            nil))
         (choice
          (completing-read
           (concat "Select " list-name ": ")
           existing-items
           nil
           nil
           nil
           'hax/select-from-list-or-add-history)))
    (if (member choice existing-items)
        (hax/log "%s selected from existing items" choice)
      (with-temp-buffer
        (when existing-items
          (insert (string-join existing-items "\n"))
          (insert "\n"))
        (insert choice)
        (write-file filename)
        (hax/log "%s added to the list and selected" choice)))
    choice))

(defun hax/org-insert-link (type with-description)
  (let* ((target-base (pcase type
                        ('file (counsel-find-file))
                        ('attachment (counsel-find-file))
                        ('id (hax/org-select-subtree))
                        ('tag (hax/select-tag nil))
                        ('person (hax/select-from-list-or-add "person"))
                        ('organization (hax/select-from-list-or-add "organization"))
                        ('code (hax/code-link-get-org-link))
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
                         ('code "")
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
  ("c" (hax/org-insert-link 'code nil))
  ("C" (hax/org-insert-link 'code t))
  ("q" nil "cancel"))


