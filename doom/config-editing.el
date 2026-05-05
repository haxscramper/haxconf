;;; -*- lexical-binding: t; -*-

(setq scroll-margin 2)
(setq whitespace-line-column 75)
(setq-default fill-column 75)
(global-visual-line-mode t)

(after! olivetti
  (setq olivetti-body-width (+ 75 7)))

;; Don't account for folded section lines
(setq display-line-numbers-type 'visual)
;; Editing operations such as deleting work only on parts of the word instead of
;; cutting off whole identifier at once.
;; https://www.gnu.org/software/emacs/manual/html_node/ccmode/Subword-Movement.html
(global-subword-mode t)

;; https://tecosaur.github.io/emacs-config/config.html#company
(after! company
  (setq company-idle-delay 0.2
        company-minimum-prefix-length 2)
  (setq company-show-numbers t)
  (add-hook 'evil-normal-state-entry-hook #'company-abort))


(setq-default history-length 1000)
(setq-default prescient-history-length 1000)

;; Disable prompts from the file-local variables by default.
(setq enable-local-variables :all)

(map!
 ;; M-> or M-< to indent code blocks in all modes
 :nv "M->" (cmd! (code-indent-shift-right
                  (get-selected-region-start)
                  (get-selected-region-end) 2))
 :nv "M-<" (cmd! (code-indent-shift-left
                  (get-selected-region-start)
                  (get-selected-region-end) 2))
 :nv [s-pause] (cmd! (shell-command "entr-space.sh")))

;; Text case conversion

(use-package! string-inflection
  :commands (string-inflection-all-cycle
             string-inflection-toggle
             string-inflection-camelcase
             string-inflection-lower-camelcase
             string-inflection-kebab-case
             string-inflection-underscore
             string-inflection-capital-underscore
             string-inflection-upcase)
  :init
  (map! :leader :prefix ("c~" . "naming convention")
        :desc "cycle" "~" #'string-inflection-all-cycle
        :desc "toggle" "t" #'string-inflection-toggle
        :desc "CamelCase" "c" #'string-inflection-camelcase
        :desc "downCase" "d" #'string-inflection-lower-camelcase
        :desc "kebab-case" "k" #'string-inflection-kebab-case
        :desc "under_score" "_" #'string-inflection-underscore
        :desc "Upper_Score" "u" #'string-inflection-capital-underscore
        :desc "UP_CASE" "U" #'string-inflection-upcase)
  (after! evil
    (evil-define-operator evil-operator-string-inflection (beg end _type)
      "Define a new evil operator that cycles symbol casing."
      :move-point nil
      (interactive "<R>")
      (string-inflection-all-cycle)
      (setq evil-repeat-info '([?g ?~])))
    (define-key evil-normal-state-map (kbd "g~") 'evil-operator-string-inflection)))

;; Highlight symbol under cursor using saner keybinding
(map! :nv ",hs" 'highlight-symbol)

(defun current-line-indent ()
  (interactive)
  (save-excursion (back-to-indentation) (current-column)))

(defun previous-line-indent ()
  (interactive)
  (save-excursion (forward-line -1) (current-column)))

(cl-defun newline-and-indent-same-level (&optional
                                         (indent-addition nil)
                                         (goto-eol nil))
  "Insert a newline, then indent to the same column as the current line."
  (interactive)
  (when goto-eol (goto-char (line-end-position)))
  (let ((col (save-excursion
               (back-to-indentation)
               (current-column))))
    (newline)
    (indent-to-column (if indent-addition
                          (+ indent-addition col)
                        col))))

(map! :i "<C-return>" 'newline-and-indent-same-level)

;; Modified answer from http://emacs.stackexchange.com/questions/47/align-vertical-columns-of-numbers-on-the-decimal-point
(defun align-repeat (start end regexp &optional justify-right after)
  "Repeat alignment with respect to the given regular expression.
If JUSTIFY-RIGHT is non nil justify to the right instead of the
left. If AFTER is non-nil, add whitespace to the left instead of
the right."
  (interactive "r\nsAlign regexp: ")
  (let* ((ws-regexp (if (string-empty-p regexp)
                        "\\(\\s-+\\)"
                      "\\(\\s-*\\)"))
         (complete-regexp (if after
                              (concat regexp ws-regexp)
                            (concat ws-regexp regexp)))
         (group (if justify-right -1 1)))

    (unless (use-region-p)
      (save-excursion
        (while (and
                (string-match-p complete-regexp (thing-at-point 'line))
                (= 0 (forward-line -1)))
          (setq start (point-at-bol))))
      (save-excursion
        (while (and
                (string-match-p complete-regexp (thing-at-point 'line))
                (= 0 (forward-line 1)))
          (setq end (point-at-eol)))))

    (align-regexp start end complete-regexp group 1 t)))

(defun align-repeat-decimal (start end)
  "Align a table of numbers on decimal points and dollar signs (both optional)"
  (interactive "r")
  (require 'align)
  (align-region start end nil
                '((nil (regexp . "\\([\t ]*\\)\\$?\\([\t ]+[0-9]+\\)\\.?")
                   (repeat . t)
                   (group 1 2)
                   (spacing 1 1)
                   (justify nil t)))
                nil))

(defmacro create-align-repeat-x (name regexp &optional justify-right default-after)
  "Helper macro to declare alignment functions for different regex patterns"
  (let* ((new-func (intern (concat "align-repeat-" name)))
         (new-func-defn
          `(defun ,new-func (start end switch)
             (interactive "r\nP")
             (let ((after (not (eq (if switch t nil) (if ,default-after t nil)))))
               (align-repeat start end ,regexp ,justify-right after)))))
    (put new-func 'function-documentation "Created by `create-align-repeat-x'.")
    new-func-defn))

(create-align-repeat-x "comma" "," nil t)
(create-align-repeat-x "semicolon" ";" nil t)
(create-align-repeat-x "colon" ":" nil t)
(create-align-repeat-x "equal" "=")
(create-align-repeat-x "math-oper" "[+\\-*/]")
(create-align-repeat-x "percent" "%")
(create-align-repeat-x "ampersand" "&")
(create-align-repeat-x "bar" "|")
(create-align-repeat-x "left-paren" "(")
(create-align-repeat-x "right-paren" ")" t)
(create-align-repeat-x "left-curly-brace" "{")
(create-align-repeat-x "right-curly-brace" "}" t)
(create-align-repeat-x "left-square-brace" "\\[")
(create-align-repeat-x "right-square-brace" "\\]" t)
(create-align-repeat-x "backslash" "\\\\")
(create-align-repeat-x "quote" "\"")
(create-align-repeat-x "hash" "#+")


(map!
 :v ",a," #'align-repeat-comma
 :v ",a:" #'align-repeat-colon
 :v ",a;" #'align-repeat-semicolon
 :v ",a]" #'align-repeat-right-square-brace
 :v ",a}" #'align-repeat-right-curly-brace
 :v ",a)" #'align-repeat-right-paren
 :v ",a[" #'align-repeat-left-square-brace
 :v ",a{" #'align-repeat-left-curly-brace
 :v ",a(" #'align-repeat-left-paren
 :v ",a=" #'align-repeat-equal
 :v ",a#" #'align-repeat-hash
 :v ",a'" #'align-repeat-quote
 )

(defun set-local-abbrevs (abbrevs)
  "Add ABBREVS to `local-abbrev-table' and make it buffer local.
     ABBREVS should be a list of abbrevs as passed to `define-abbrev-table'.
     The `local-abbrev-table' will be replaced by a copy with the new
     abbrevs added, so that it is not the same as the abbrev table used
     in other buffers with the same `major-mode'."
  (let* ((bufname (buffer-name))
         (prefix (substring (md5 bufname) 0 (length bufname)))
         (tblsym (intern (concat prefix "-abbrev-table"))))
    (set tblsym (copy-abbrev-table local-abbrev-table))
    (dolist (abbrev abbrevs)
      (define-abbrev (eval tblsym)
        (car abbrev)
        (cadr abbrev)
        (caddr abbrev)))
    (setq-local local-abbrev-table (eval tblsym))))



;; Claude, please write a code that un-fucks default emacs behavior that
;; makes absolutely no sense in any context whatsoever, but that somehow
;; made its way into other configurations. I don't understand, is it too
;; much to ask for the editor to not shit into the clipboard each time I'm
;; doing backspace or something?

(defun hax/delete-backward-char-no-kill (&optional n)
  "Delete N characters backward without adding to kill ring.
Works in all contexts including minibuffer and prompts."
  (interactive "p")
  (let ((n (or n 1)))
    (delete-char (- n))))

(defun hax/delete-forward-char-no-kill (&optional n)
  "Delete N characters forward without adding to kill ring."
  (interactive "p")
  (let ((n (or n 1)))
    (delete-char n)))

(defun hax/backward-delete-word-no-kill (&optional n)
  "Delete word backward without adding to kill ring."
  (interactive "p")
  (let ((n (or n 1)))
    (dotimes (_ n)
      (let ((start (point)))
        (backward-word 1)
        (delete-region (point) start)))))

(defun hax/delete-word-no-kill (&optional n)
  "Delete word forward without adding to kill ring."
  (interactive "p")
  (let ((n (or n 1)))
    (dotimes (_ n)
      (let ((start (point)))
        (forward-word 1)
        (delete-region start (point))))))

(defun hax/delete-line-backward-no-kill ()
  "Delete from point to beginning of line without adding to kill ring."
  (interactive)
  (delete-region (line-beginning-position) (point)))

(defun hax/delete-line-forward-no-kill ()
  "Delete from point to end of line without adding to kill ring."
  (interactive)
  (delete-region (point) (line-end-position)))

;; Override backspace in all keymaps
(defun hax/setup-no-kill-delete-keys ()
  (interactive)
  "Set up delete keys that don't add to kill ring in all contexts."

  ;; Global bindings
  (global-set-key (kbd "DEL") #'hax/delete-backward-char-no-kill)
  (global-set-key (kbd "<backspace>") #'hax/delete-backward-char-no-kill)
  (global-set-key (kbd "C-<backspace>") #'hax/backward-delete-word-no-kill)
  (global-set-key (kbd "M-<backspace>") #'hax/backward-delete-word-no-kill)
  (global-set-key (kbd "M-DEL") #'hax/backward-delete-word-no-kill)

  ;; Optional: Also override forward delete keys
  (global-set-key (kbd "<delete>") #'hax/delete-forward-char-no-kill)
  (global-set-key (kbd "M-d") #'hax/delete-word-no-kill)
  (global-set-key (kbd "C-k") #'hax/delete-line-forward-no-kill)
  (global-set-key (kbd "C-u") #'hax/delete-line-backward-no-kill)

  ;; Minibuffer specific
  (define-key minibuffer-local-map (kbd "DEL") #'hax/delete-backward-char-no-kill)
  (define-key minibuffer-local-map (kbd "<backspace>") #'hax/delete-backward-char-no-kill)
  (define-key minibuffer-local-map (kbd "C-<backspace>") #'hax/backward-delete-word-no-kill)
  (define-key minibuffer-local-map (kbd "M-<backspace>") #'hax/backward-delete-word-no-kill)
  (define-key minibuffer-local-map (kbd "M-DEL") #'hax/backward-delete-word-no-kill)

  ;; Completion maps
  (with-eval-after-load 'minibuffer
    (define-key minibuffer-local-completion-map (kbd "DEL") #'hax/delete-backward-char-no-kill)
    (define-key minibuffer-local-completion-map (kbd "<backspace>") #'hax/delete-backward-char-no-kill)
    (define-key minibuffer-local-must-match-map (kbd "DEL") #'hax/delete-backward-char-no-kill)
    (define-key minibuffer-local-must-match-map (kbd "<backspace>") #'hax/delete-backward-char-no-kill))

  ;; Read-string and similar prompts
  (define-key read-expression-map (kbd "DEL") #'hax/delete-backward-char-no-kill)
  (define-key read-expression-map (kbd "<backspace>") #'hax/delete-backward-char-no-kill)

  ;; Ivy (if you use it)
  (with-eval-after-load 'ivy
    (define-key ivy-minibuffer-map (kbd "DEL") #'hax/delete-backward-char-no-kill)
    (define-key ivy-minibuffer-map (kbd "<backspace>") #'hax/delete-backward-char-no-kill)
    (define-key ivy-minibuffer-map (kbd "M-DEL") #'hax/backward-delete-word-no-kill))

  ;; Helm (if you use it)
  (with-eval-after-load 'helm
    (define-key helm-map (kbd "DEL") #'hax/delete-backward-char-no-kill)
    (define-key helm-map (kbd "<backspace>") #'hax/delete-backward-char-no-kill)
    (define-key helm-map (kbd "M-DEL") #'hax/backward-delete-word-no-kill))

  ;; Company completion (if you use it)
  (with-eval-after-load 'company
    (define-key company-active-map (kbd "DEL") #'hax/delete-backward-char-no-kill)
    (define-key company-active-map (kbd "<backspace>") #'hax/delete-backward-char-no-kill))

  ;; Vertico (if you use it)
  (with-eval-after-load 'vertico
    (define-key vertico-map (kbd "DEL") #'hax/delete-backward-char-no-kill)
    (define-key vertico-map (kbd "<backspace>") #'hax/delete-backward-char-no-kill)
    (define-key vertico-map (kbd "M-DEL") #'hax/backward-delete-word-no-kill)))

;; Hook to ensure these bindings work in all new buffers
(defun hax/setup-buffer-delete-keys ()
  "Set up delete keys for current buffer."
  (local-set-key (kbd "DEL") #'hax/delete-backward-char-no-kill)
  (local-set-key (kbd "<backspace>") #'hax/delete-backward-char-no-kill)
  (local-set-key (kbd "C-<backspace>") #'hax/backward-delete-word-no-kill)
  (local-set-key (kbd "M-<backspace>") #'hax/backward-delete-word-no-kill)
  (local-set-key (kbd "M-DEL") #'hax/backward-delete-word-no-kill))

;; Apply to all new buffers
(add-hook 'after-change-major-mode-hook #'hax/setup-buffer-delete-keys)
(add-hook 'minibuffer-setup-hook #'hax/setup-buffer-delete-keys)

;; Special handling for eval-expression and other read functions
(defun hax/advice-read-functions (orig-fun &rest args)
  "Advice to ensure our delete keys work in read functions."
  (let ((minibuffer-local-map (copy-keymap minibuffer-local-map)))
    (define-key minibuffer-local-map (kbd "DEL") #'hax/delete-backward-char-no-kill)
    (define-key minibuffer-local-map (kbd "<backspace>") #'hax/delete-backward-char-no-kill)
    (define-key minibuffer-local-map (kbd "M-DEL") #'hax/backward-delete-word-no-kill)
    (apply orig-fun args)))

;; Apply advice to common read functions
(advice-add 'read-string :around #'hax/advice-read-functions)
(advice-add 'read-from-minibuffer :around #'hax/advice-read-functions)

;; Initialize the setup
(hax/setup-no-kill-delete-keys)

;; Optional: If you want to completely disable kill-ring for delete operations
;; you can also override the functions that add to kill-ring
(defun hax/disable-kill-ring-advice (orig-fun &rest args)
  "Advice to prevent functions from adding to kill-ring during delete operations."
  (let ((kill-ring kill-ring)
        (kill-ring-yank-pointer kill-ring-yank-pointer))
    (cl-letf (((symbol-function 'kill-new) (lambda (&rest _) nil))
              ((symbol-function 'kill-append) (lambda (&rest _) nil)))
      (apply orig-fun args))))

;; Uncomment these if you want even more aggressive kill-ring prevention
;; (advice-add 'backward-delete-char-untabify :around #'hax/disable-kill-ring-advice)
;; (advice-add 'delete-backward-char :around #'hax/disable-kill-ring-advice)

(defun hax/rename-file-with-timestamp ()
  "Rename current buffer's file, appending or updating an ISO 8601 timestamp."
  (interactive)
  (let* ((filename (buffer-file-name))
         (timestamp (format-time-string "%Y-%m-%dT%H%M%S"))
         (dir (file-name-directory filename))
         (ext (file-name-extension filename))
         (name (file-name-sans-extension (file-name-nondirectory filename)))
         (ts-pattern "--[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}T[0-9]\\{6\\}")
         (new-name
          (if (string-match ts-pattern name)
              (replace-match (concat "--" timestamp) t t name)
            (concat name "--" timestamp)))
         (new-filename (expand-file-name
                        (if (string-empty-p ext)
                            new-name
                          (concat new-name "." ext))
                        dir)))
    (rename-file filename new-filename)
    (set-visited-file-name new-filename)))

(defun hax/org-archive-subtree (&optional find-done)
  "Move the current subtree to the archive.

Like `org-archive-subtree', but it inlines the context-saving logic
instead of depending on `org-archive-save-context-info'.

Additionally, save the original parent heading ID (if any) into
ARCHIVE_OLPATH_PARENT_ID."
  (interactive "P")
  (if (and (org-region-active-p) org-loop-over-headlines-in-active-region)
      (let ((cl (if (eq org-loop-over-headlines-in-active-region 'start-level)
                    'region-start-level 'region))
            org-loop-over-headlines-in-active-region)
        (org-map-entries
         `(progn (setq org-map-continue-from (progn (org-back-to-heading) (point)))
                 (org-archive-subtree ,find-done))
         org-loop-over-headlines-in-active-region
         cl (if (org-invisible-p) (org-end-of-subtree nil t))))
    (cond
     ((equal find-done '(4))  (org-archive-all-done))
     ((equal find-done '(16)) (org-archive-all-old))
     (t
      ;; Save all relevant TODO keyword-related variables.
      (let* ((tr-org-todo-keywords-1 org-todo-keywords-1)
             (tr-org-todo-kwd-alist org-todo-kwd-alist)
             (tr-org-done-keywords org-done-keywords)
             (tr-org-todo-regexp org-todo-regexp)
             (tr-org-todo-line-regexp org-todo-line-regexp)
             (tr-org-odd-levels-only org-odd-levels-only)
             (this-buffer (current-buffer))
             (time (format-time-string
                    (org-time-stamp-format 'with-time 'no-brackets)))
             (file (abbreviate-file-name
                    (or (buffer-file-name (buffer-base-buffer))
                        (error "No file associated to buffer"))))
             (location (org-archive--compute-location
                        (or (org-entry-get nil "ARCHIVE" 'inherit)
                            org-archive-location)))
             (afile (car location))
             (heading (cdr location))
             (infile-p (equal file (abbreviate-file-name (or afile ""))))
             (newfile-p (and (org-string-nw-p afile)
                             (not (file-exists-p afile))))
             (buffer (cond ((not (org-string-nw-p afile)) this-buffer)
                           ((find-file-noselect afile 'nowarn))
                           (t (error "Cannot access file \"%s\"" afile))))
             (org-odd-levels-only
              (if (local-variable-p 'org-odd-levels-only (current-buffer))
                  org-odd-levels-only
                tr-org-odd-levels-only))
             level datetree-date datetree-subheading-p
             ;; Suppress on-the-fly headline updates.
             (org-element--cache-avoid-synchronous-headline-re-parsing t))
        (when (string-match "\\`datetree/\\(\\**\\)" heading)
          ;; "datetree/" corresponds to 3 levels of headings.
          (let ((nsub (length (match-string 1 heading))))
            (setq heading (concat (make-string
                                   (+ (if org-odd-levels-only 5 3)
                                      (* (org-level-increment) nsub))
                                   ?*)
                                  (substring heading (match-end 0))))
            (setq datetree-subheading-p (> nsub 0)))
          (setq datetree-date (org-date-to-gregorian
                               (or (org-entry-get nil "CLOSED" t) time))))
        (if (and (> (length heading) 0)
                 (string-match "^\\*+" heading))
            (setq level (match-end 0))
          (setq heading nil level 0))
        (save-excursion
          (org-back-to-heading t)
          ;; Get context information that will be lost by moving the
          ;; tree (inlined `org-archive-save-context-info' logic),
          ;; plus original parent ID.
          (let* ((all-tags (org-get-tags))
                 (local-tags
                  (cl-remove-if (lambda (tag)
                                  (get-text-property 0 'inherited tag))
                                all-tags))
                 (inherited-tags
                  (cl-remove-if-not (lambda (tag)
                                      (get-text-property 0 'inherited tag))
                                    all-tags))
                 (parent-id
                  (save-excursion (when (org-up-heading-safe) (org-id-get-create))))
                 (context
                  `((category . ,(org-get-category nil 'force-refresh))
                    (file . ,file)
                    (itags . ,(mapconcat #'identity inherited-tags " "))
                    (ltags . ,(mapconcat #'identity local-tags " "))
                    (olpath . ,(mapconcat #'identity
                                          (org-get-outline-path)
                                          "/"))
                    (olpath_parent_id . ,parent-id)
                    (time . ,time)
                    (todo . ,(org-entry-get (point) "TODO")))))
            ;; We first only copy, in case something goes wrong.
            ;; We need to protect `this-command', to avoid kill-region sets it,
            ;; which would lead to duplication of subtrees.
            (let (this-command) (org-copy-subtree 1 nil t))
            (set-buffer buffer)
            ;; Enforce Org mode for the archive buffer.
            (if (not (derived-mode-p 'org-mode))
                ;; Force the mode for future visits.
                (let ((org-insert-mode-line-in-empty-file t)
                      (org-inhibit-startup t))
                  (call-interactively 'org-mode)))
            (when (and newfile-p org-archive-file-header-format)
              (goto-char (point-max))
              (insert (format org-archive-file-header-format
                              (buffer-file-name this-buffer))))
            (when datetree-date
              (require 'org-datetree)
              (org-datetree-find-date-create datetree-date)
              (org-narrow-to-subtree))
            ;; Force the TODO keywords of the original buffer.
            (let ((org-todo-line-regexp tr-org-todo-line-regexp)
                  (org-todo-keywords-1 tr-org-todo-keywords-1)
                  (org-todo-kwd-alist tr-org-todo-kwd-alist)
                  (org-done-keywords tr-org-done-keywords)
                  (org-todo-regexp tr-org-todo-regexp)
                  (org-todo-line-regexp tr-org-todo-line-regexp))
              (goto-char (point-min))
              (org-fold-show-all '(headings blocks))
              (if (and heading (not (and datetree-date (not datetree-subheading-p))))
                  (progn
                    (if (re-search-forward
                         (concat "^" (regexp-quote heading)
                                 "\\([ \t]+:\\(" org-tag-re ":\\)+\\)?[ \t]*$")
                         nil t)
                        (goto-char (match-end 0))
                      ;; Heading not found, just insert it at the end.
                      (goto-char (point-max))
                      (or (bolp) (insert "\n"))
                      ;; Datetrees don't need too much spacing.
                      (insert (if datetree-date "" "\n") heading "\n")
                      (end-of-line 0))
                    ;; Make the subtree visible.
                    (org-fold-show-subtree)
                    (if org-archive-reversed-order
                        (progn
                          (org-back-to-heading t)
                          (outline-next-heading))
                      (org-end-of-subtree t))
                    (skip-chars-backward " \t\r\n")
                    (and (looking-at "[ \t\r\n]*")
                         ;; Datetree archives don't need so much spacing.
                         (replace-match (if datetree-date "\n" "\n\n"))))
                ;; No specific heading, just go to end of file, or to the
                ;; beginning, depending on `org-archive-reversed-order'.
                (if org-archive-reversed-order
                    (progn
                      (goto-char (point-min))
                      (unless (org-at-heading-p) (outline-next-heading)))
                  (goto-char (point-max))
                  ;; Subtree narrowing can let the buffer end on a headline.
                  ;; `org-paste-subtree' then deletes it.  Prevent that.
                  (unless (and datetree-date (bolp)) (insert "\n"))))
              ;; Paste.
              (org-paste-subtree (org-get-valid-level level (and heading 1)))
              ;; Shall we append inherited tags?
              (and inherited-tags
                   (or (and (eq org-archive-subtree-add-inherited-tags 'infile)
                            infile-p)
                       (eq org-archive-subtree-add-inherited-tags t))
                   (org-set-tags all-tags))
              ;; Mark the entry as done.
              (when (and org-archive-mark-done
                         (let ((case-fold-search nil))
                           (looking-at org-todo-line-regexp))
                         (or (not (match-end 2))
                             (not (member (match-string 2) org-done-keywords))))
                (let (org-log-done org-todo-log-states)
                  (org-todo
                   (car (or (member org-archive-mark-done org-done-keywords)
                            org-done-keywords)))))

              ;; Inline context info saving (no `org-archive-save-context-info').
              (cl-labels ((put-archive-prop (key sym)
                            (let ((value (cdr (assq sym context))))
                              (when (org-string-nw-p value)
                                (org-entry-put (point) key value)))))
                (put-archive-prop "ARCHIVE_TIME"     'time)
                (put-archive-prop "ARCHIVE_FILE"     'file)
                (put-archive-prop "ARCHIVE_OLPATH"   'olpath)
                (put-archive-prop "ARCHIVE_CATEGORY" 'category)
                (put-archive-prop "ARCHIVE_TODO"     'todo)
                (put-archive-prop "ARCHIVE_ITAGS"    'itags)
                ;; New: original parent ID (if any).
                (put-archive-prop "ARCHIVE_OLPATH_PARENT_ID" 'olpath_parent_id))

              ;; Save the buffer, if it is not the same buffer and
              ;; depending on `org-archive-subtree-save-file-p'.
              (unless (eq this-buffer buffer)
                (when (or (eq org-archive-subtree-save-file-p t)
                          (eq org-archive-subtree-save-file-p
                              (if (boundp 'org-archive-from-agenda)
                                  'from-agenda
                                'from-org)))
                  (save-buffer)))
              (widen))))
        ;; Back in original buffer: cut the tree and finish up.
        (run-hooks 'org-archive-hook)
        (let (this-command) (org-cut-subtree))
        (when (featurep 'org-inlinetask)
          (org-inlinetask-remove-END-maybe))
        (setq org-markers-to-move nil)
        (when org-provide-todo-statistics
          (save-excursion
            (org-up-heading-safe)
            (org-update-statistics-cookies nil)))
        (hax/log "Subtree archived %s"
                 (if (eq this-buffer buffer)
                     (concat "under heading: " heading)
                   (concat "in file: " (abbreviate-file-name afile)))))))
    (org-fold-reveal)
    (if (looking-at "^[ \t]*$")
        (outline-next-visible-heading 1))))

(defun +default/yank-buffer-path (&optional root)
  "Copy the current buffer's path to the kill ring."
  (interactive)
  (if-let (filename (or (buffer-file-name (buffer-base-buffer))
                        (bound-and-true-p list-buffers-directory)))
      (let ((path (if root
                      (file-relative-name filename root)
                    filename)))
        (kill-new path)
        (if (string= path (car kill-ring))
            (hax/log "Copied path: %s" path)
          (user-error "Couldn't copy filename in current buffer")))
    (error "Couldn't find filename in current buffer")))

(defvar hax/delete-region-as-kill t)
(defun hax/delete-region (fun &rest args)
  (if hax/delete-region-as-kill
      (apply fun args)
    (apply 'delete-region args)))

(advice-add 'kill-region :around #'hax/delete-region)
(advice-add 'org-capture :before #'hax/org-pre-capture-hook)
