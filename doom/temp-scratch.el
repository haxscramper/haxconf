;;; -*- lexical-binding: t; -*-

(defun json-parse-file (file)
  (json-parse-string (f-read file)))




(s-replace-regexp
 (rx (group (1+ digit)) " "
     (group rx-month-name-or-digit) " "
     (group (1+ digit)))
 "\\3-\\2-\\1"
 "27 March 2020"
 )

(require 'dbus)

(setq shell-file-name (executable-find "sh"))

(defun my-dbus-method-handler (logical physical quote)
  (insert (format "- %s (%s) /\"%s\"/" logical physical quote))
  '(:boolean t))

(defun hax/dbus-register-methods (method-list)
  (dolist (method method-list)
    (dbus-register-method
     :session "hax.haxconf.Emacs" "/hax/haxconf/Emacs"
     "hax.haxconf.Emacs"
     (car method)
     (cdr method))))



(hax/dbus-register-methods
 (list
  (cons "InsertPdfQuoteBlock2"
        (lambda (logical physical quote)
          (insert (format "#+caption: %s (%s)\n#+begin_quote\n%s\n#+end_quote\n"
                          logical physical quote))))
  (cons "InsertPdfQuoteItem2"
        (lambda (logical physical quote)
          (insert (format "- %s (%s) /\"%s\"/"
                          logical physical quote))))
  (cons "PrintTest" (lambda (input) (message "%s" input)))))


(set-face-attribute 'winum-face nil :weight 'bold :foreground "green")


(defun hax/select-from-list-or-add (list-name)
  "Select an item from a list of alternatives stored in ~/.config/targets/list-name.txt
   If the user inputs a new value (not already in the list), update the file and print that the new value has been selected."
  (let* ((git-root (vc-root-dir))
         (filename (expand-file-name (f-join git-root (concat list-name ".txt"))))
         (existing-items (if (file-exists-p filename)
                             (with-temp-buffer
                               (insert-file-contents filename)
                               (split-string (buffer-string) "\n" t))
                           nil)))
    (ivy-read (concat "Select " list-name ": ")
              (lambda (str pred action)
                (if (eq action 'metadata)
                    nil
                  (complete-with-action action existing-items str pred)))
              :require-match nil
              :sort t
              :caller 'hax/select-from-list-or-add
              :action (lambda (x)
                        (if (member x existing-items)
                            (message "%s selected from existing items" x)
                          (with-temp-buffer
                            (when existing-items
                              (insert (string-join existing-items "\n"))
                              (insert "\n"))
                            (insert x)
                            (write-file filename)
                            (message "%s added to the list and selected" x)))))))

(defun hax/org-insert-link (type)
  (let* ((target-base (pcase type
                        ('file (counsel-find-file))
                        ('attachment (counsel-find-file))
                        ('id (hax/org-select-subtree))
                        ('tag (hax/select-tag nil))
                        ('person (hax/select-from-list-or-add "person"))
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

         (description (read-string "Description: "
                                   default-desc)))

    (pcase type
      ('tag (insert target))
      (_ (if (string-empty-p description)
             (insert (format "[[%s:%s]]" (symbol-name type) target))
           (insert (format "[[%s:%s][%s]]" (symbol-name type) target description)))))))


(defhydra hydra-insert-link (:color blue :hint nil)
  "
  Insert Link:
  _f_: file
  _a_: attachment
  _i_: ID
  _t_: Tag
  _p_: Person
  "
  ("f" (hax/org-insert-link 'file))
  ("a" (hax/org-insert-link 'attachment))
  ("i" (hax/org-insert-link 'id))
  ("t" (hax/org-insert-link 'tag))
  ("p" (hax/org-insert-link 'person))
  ("q" nil "cancel"))
