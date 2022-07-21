(setq
 nim-imenu-generic-expression
 '(
   ("object" "^  \\(.*?\\) = .*?object" 1)
   ("object" "^  \\(.*?\\) = .*?enum" 1)
   ("proc" "^ *proc *\\(.*\\)" 1)
   ("func" "^ *func *\\(.*\\)" 1)
   ("macro" "^ *macro*\\(.*\\)" 1)
   ("method" "^ *method*\\(.*\\)" 1)
   ("converter" "^ *converter*\\(.*\\)" 1)
   ("template" "^ *template *\\(.*\\)" 1)
   ("iterator" "^ *iterator *\\(.*\\)" 1)
   ("separator" "^ *\\(#=* [^#]* =*#\\)$" 1)
   ("separator" "^ *\\(#-* [^#]* -*#\\)$" 1)
   ("separator" "^#\\(\\** [^#]* \\**#\\)$" 1)
   ("suite" "suite \\(\".*\"\\):" 1)
   ("test" "  test \\(\".*\"\\):" 1)
   ("test" "  multitest \\(\".*\"\\):" 1)
   ("task" "task \\(\".*\"\\):" 1)
   ("const" "^const \\(.*\\)" 1)
   ("let" "^let \\(.*\\)" 1)
   ("var" "^var \\(.*\\)" 1)))

(defun hax/wrap-with-current-indent ()
  (interactive)
  (newline-and-indent-same-level)
  (insert "## ")
  (fill-paragraph))

(defun hax/nim-mode-hook ()
  (interactive)
  (flycheck-mode 0)
  ;; Disable completion from nimsuggest, it is mostly useless
  (nimsuggest-mode 0)
  ;; Flycheck relies on nimsuggest, so disable it as well
  (electric-indent-mode 0)
  (company-mode t)
  (setq company-backends '(company-capf :with company-etags))
  (setq imenu-generic-expression nim-imenu-generic-expression)
  (map!
   :map nim-mode-map
   :nv [C-M-q] #'hax/wrap-with-current-indent
   :desc "insert-inline-comment"
   :n ",i#"  (cmd! (insert "#[ ")
                   (save-excursion (insert " ]#"))
                   (evil-insert-state))))

(font-lock-replace-keywords
 'nim-mode
 `(
   ("notNil" . 'font-lock-builtin-face)
   ;; Highlight function calls as well
   (,(rx
      (| " " "." "]" ")" "(" "[") (group (in lower) (+ word)) "(") .
      (1 font-lock-function-name-face))
   ))

(add-hook! 'nim-mode-hook 'hax/nim-mode-hook)


(defun org-babel-eval-maybe-error (command query &optional notify)
  "Run COMMAND on QUERY.
Writes QUERY into a temp-buffer that is processed with
`org-babel--shell-command-on-region'.  If COMMAND succeeds then return
its results, otherwise display STDERR with
`org-babel-eval-error-notify'."
  (message "executing [%s]" command)
  (let ((error-buffer (get-buffer-create " *Org-Babel Error*")) exit-code)
    (with-current-buffer error-buffer (erase-buffer))
    (with-temp-buffer
      (insert query)
      (setq exit-code
            (org-babel--shell-command-on-region
             command error-buffer))
      (if (or (not (numberp exit-code)) (> exit-code 0))
          (if (not notify)
              ;; Optionally notify user on the code failure, but do not
              ;; enforce it.
              (with-current-buffer error-buffer (buffer-string))
            (progn
              (with-current-buffer error-buffer
                (org-babel-eval-error-notify exit-code (buffer-string)))
              (save-excursion
                (when (get-buffer org-babel-error-buffer-name)
                  (with-current-buffer org-babel-error-buffer-name
                    (unless (derived-mode-p 'compilation-mode)
                      (compilation-mode))
                    ;; Compilation-mode enforces read-only, but Babel expects
                    ;; the buffer modifiable.
                    (setq buffer-read-only nil))))
              nil))
        (buffer-string)))))


(setq org-babel-default-header-args:nim
      `((:import . "macros")
        (:cache . nil)
        (:flags . "--hints=off --verbosity=0 --filenames=canonical")
        (:results . "output")))

(defun org-babel-nim-execute (body params)
  "Overide for default org-babel execute for nim. Use `nim c -r'
and return both compiler output and regular text"
  (message "params:%s" params)
  (let* ((tmp-bin-file (org-babel-nim--sanitize-file-name
                        (org-babel-temp-file "nim_src_" )))
         (tmp-src-file (concat tmp-bin-file ".nim"))
         (cmdline (cdr (assoc :cmdline params)))
         (cmdline (if cmdline (concat " " cmdline) ""))
         (define (org-babel-read
                  (or (cdr (assoc :define params))
                      (org-entry-get nil "define" t))
                  nil))
         (define (if (stringp define) (split-string define " ") nil))
         (define (if define
                     (mapconcat (lambda (inc) (format "-d:%s" inc)) define " ")
                   ""))
         (flags (cdr (assoc :flags params)))
         (flags (mapconcat 'identity (if (listp flags) flags (list flags)) " "))
         (libs (org-babel-read
                (or (cdr (assq :libs params))
                    (org-entry-get nil "libs" t))
                nil))
         (libs (mapconcat #'identity (if (listp libs) libs (list libs)) " "))
         (full-body
          (org-babel-nim-expand-nim   body params)))
    (with-temp-file tmp-src-file (insert full-body))
    (org-babel-eval-maybe-error
     (format "nim c -r %s %s -o:%s %s %s"
             define
             flags
             (org-babel-process-file-name tmp-bin-file)
             (org-babel-process-file-name tmp-src-file)
             libs)
     "")))
