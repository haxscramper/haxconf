;;; -*- lexical-binding: t; -*-

(require 'f)
(require 'ivy)
(require 'ts)
(require 'org-capture)
(require 'org-download)
(require 'org-expiry)
(require 'org-clock)
(require 'uuidgen)

(load! "lang-org.d/hax-agenda.el")
(load! "lang-org.d/hax-link-insertion.el")
(load! "lang-org.d/hax-link-insertion-code.el")
(load! "lang-org.d/hax-org-utils.el")
(load! "lang-org.d/hax-refile.el")
(load! "lang-org.d/subtree-navigation.el")

(defun hax/org-update-all-cookies ()
  (interactive)
  (let ((current-prefix-arg '(4)))
    (org-update-statistics-cookies "ALL")))

(defun hax/popup-mode-hook ()
  (interactive)
  (hax/log "Evil insertt state triggered"))

(defun font-lock-replace-keywords (mode keyword-list)
  "Use `font-lock-remove-keywords' on each keyword and then add
  `keyword-list' to font lock."
  (font-lock-remove-keywords
   mode
   (-map 'car keyword-list))
  (font-lock-add-keywords mode keyword-list))

(setq abbrev-expand-function
      (lambda ()
        (unless (org-in-src-block-p)
          (abbrev--default-expand))))

(font-lock-add-keywords
 'org-mode
 `((,(rx bol (any space) "- " (group "[X]")) 1 'org-done prepend)
   (,(rx bol (any space) "- " (group "[ ]")) 1 'org-todo prepend))
 'append)

(defun hax/replace-next-placeholder ()
  "Find placeholder string, ('{{{replace-target}}}') delete it and enter insert
mode"
  (interactive)
  (let ((target "{{{replace}}}"))
    (search-forward target)
    (delete-backward-char (length target))
    (evil-insert-state)))

(defun hax/org-download-display-inline-images (func))

(load! "lang-org.d/hax-org-download-and-paste.el")



(load! "lang-org.d/hax-tags.el")

(defvar hax/select-from-list-or-add-history nil)
(defun at-empty-line () (and (not (bobp)) (looking-at-p "^\\s-*$")))

(defun backward-to-empty-line-after-non-empty ()
  "Move the cursor backwards to the empty line directly after the first non-empty line encountered."
  (interactive)
  (while (at-empty-line) (forward-line -1))
  (unless (bobp) (forward-line 1)))


(defun goto-marker (marker)
  (switch-to-buffer (marker-buffer marker))
  (goto-char (marker-position marker)))

(defun hax/org-show-all-drawers ()
  "Show all property drawers in current buffer."
  (interactive)
  (let ((data (org-element-parse-buffer)))
    (org-element-map
        data
        'property-drawer
      (lambda (drawer)
        (let ((b (org-element-property :begin drawer))
              (e (org-element-property :end drawer)))
          (org-flag-region b e nil 'org-hide-drawer))))))

(defun hax/add-subtree-refs ()
  (interactive)
  (let* ((newref (with-temp-buffer
                   (hax/org-insert-link-to-subtree :description "ref")
                   (buffer-substring-no-properties (point-min) (point-max))))
         (content (org-entry-get nil "REFS"))
         (refs (when content (--filter (not (string-empty-p it))
                                       (s-split "," content)))))
    (org-entry-put
     nil
     "REFS"
     (if refs
         (if (and (-contains-p refs newref) (length< refs 0))
             (s-join "," (--filter (not (or
                                         (string-empty-p it)
                                         (s-equals? newref it))) refs))
           (s-join "," (-concat (list newref) refs)))
       newref))))

;; (www-get-page-title "https://www.studyinjapan.go.jp/en/planning/about-scholarship/")

(defun org-wrapping-subtree (&optional tree)
  (if (or (eq 'headline (car tree)) (not tree))
      tree
    (org-wrapping-subtree (org-element-property :parent tree))))

(defun hax/org-before-logical-end ()
  (interactive)
  (let* ((wrap (org-wrapping-subtree (org-element-at-point)))
         (end (org-element-property :end wrap))
         (level (org-element-property :level wrap))
         (result (if end
                     (save-excursion
                       (if (re-search-forward
                            (rx-to-string `(and bol ,(s-repeat (+ 1 level) "*")))
                            end t)
                           (progn (backward-char (+ 1 level)) (point))
                         end))
                   (point-max)))
         (with-newline (save-excursion
                         (goto-char result)
                         (if (eq result (line-beginning-position))
                             result
                           (progn (insert "\n") (point))))))
    with-newline))

(defun hax/back-until-content ()
  "Move cursor backwards until a non-empty line is found"
  (while (eq (line-beginning-position) (point)) (backward-char))
  (forward-char))

(defun hax/back-to-first-footnote ()
  (interactive)
  (let ((wrap (org-wrapping-subtree (org-element-at-point))))
    (while (search-backward-regexp
            (rx bol "[fn:")
            (org-element-property :begin wrap) t))))

(defun hax/org-add-trailing-note ()
  (interactive)
  (goto-char (hax/org-before-logical-end))
  (backward-char)
  (hax/back-to-first-footnote)
  (hax/back-until-content)
  (insert (format "- %s " (hax/org-current-timestamp)))
  (save-excursion (insert "\n")))

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

(load! "lang-org.d/hax-org-data-query.el")
(load! "lang-org.d/hax-org-subtree-edit.el")

(defvar hax/org-src-use-full t
  "Org-src buffer edits should use full buffer, not small popup")

(set-popup-rule!
  (lambda (name action) (and (s-starts-with? "*Org Src" name) (not hax/org-src-use-full)))
  :size 0.5  :quit nil :select t :autosave t :modeline t :ttl nil)

(set-popup-rule!
  (lambda (name action) (and (s-starts-with? "*Org Src" name) hax/org-src-use-full))
  :ignore t)


(defun hax/goto-list-end ()
  (interactive)
  (let* ((elt (org-element-at-point))
         (item (org-element-property :parent elt)))
    (when (eq (car item) 'item)
      (goto-char (org-element-property :end item)))))

(defun pop-selection ()
  (when (use-region-p)
    (let ((res (buffer-substring-no-properties (region-beginning) (region-end))))
      (delete-region (region-beginning) (region-end))
      res)))

(defun hax/calibre-follow (path)
  (call-process "xdg-open" nil 0 nil (concat "calibre:" path)))

(defun hax/org-mode-flyspell ()
  "Ignore text between begin_quote and end_quote."
  (interactive)
  (setq ispell-skip-region-alist
        (cl-remove-if (lambda (region) (equal region '("begin_quote" . "end_quote")))
                      ispell-skip-region-alist))
  (add-to-list 'ispell-skip-region-alist '("begin_quote" . "end_quote")))

(defun hax/org-flyspell-hook ()
  (interactive)
  (hax/org-mode-flyspell)
  (setq flyspell-generic-check-word-predicate 'hax/flyspell-org-mode-verify))

(add-hook! 'org-mode-hook 'hax/org-mode-hook)

(defmacro push-tmp-value! (variable value body)
  `(let ((tmp-value ,variable))
     (setq ,variable ,value)
     ,body
     (setq ,variable tmp-value)))

(setq hax/fullscreen-capture nil)
(setq hax/fullscreen-client-name nil)

(setq warning-minimum-level :error)

(defmacro hax/defaccept! (name body)
  `(defun ,name ()
     (let* (;; Use org-element API instead of cave-tier hacks with forward
            ;; regex searches.
            (tree (org-element-at-point))
            ;; Store the position of the tree end
            (subtree-end (save-excursion (org-end-of-subtree t))))
       ;; If element is not scheduled, skip it
       (unless ,body
         subtree-end))))

(hax/defaccept! accept-distant-scheduled/deadlined
                (let* ((dead (org-element-property :deadline tree))
                       (shed (org-element-property :scheduled tree))
                       (now (time-to-days (current-time))))
                  (when (or dead shed)
                    (let* ((dead-days
                            (if dead (- (time-to-days (org-timestamp-to-time dead)) now) nil))
                           (shed-days
                            (if shed (- (time-to-days (org-timestamp-to-time shed)) now) nil)))
                      ;; (hax/log "dead: %s shed: %s" dead-days shed-days)
                      (or (and dead-days (< 15 dead-days 90))
                          (and shed-days (< 15 shed-days 90)))))))

(defun hax/parent-subtrees()
  "test"
  (let* ((len 18)
         (str (s-pad-left len " " (s-join "." (org-get-outline-path)))))
    (substring str (- (length str) len) (length str))))


(defvar hax/delete-region-as-kill t)
(defun hax/delete-region (fun &rest args)
  (if hax/delete-region-as-kill
      (apply fun args)
    (apply 'delete-region args)))

(advice-add 'kill-region :around #'hax/delete-region)
(advice-add 'org-capture :before #'hax/org-pre-capture-hook)


(defun hax/capture-location ()
  "Get formatted string about capture location"
  (let ((orig (org-capture-get :original-file))
        (uuid
         (substring
          (s-replace "-" "" (s-upcase (uuidgen-4))) 0 8)))
    (hax/log "[%s]" hax/fullscreen-client-name)
    (if (magit-toplevel)
        (let* ((top (f-filename (magit-toplevel)))
               (file (magit-file-relative-name orig)))
          (format
           "from ~%s:%s:%s~"
           top file hax/org-capture-from-line))
      (if orig
          (format
           "from ~%s:%s~"
           (f-filename orig) hax/org-capture-from-line)
        (format
         "from ~%s~"
         (s-replace "~" "∼" hax/fullscreen-client-name))))))

(load! "lang-org.d/hax-tg-message-edit.el")

(progn
  (cl-defun org-note-insert-at-time+date
      (text second minute hour day month year &optional (tags '()))

    (let ((org-overriding-default-time
           (encode-time second minute hour day month year)))
      (setq hax/immediate-note-tags tags)
      (setq hax/immediate-note-content text)
      (org-capture nil "@")
      (setq hax/immediate-note-content "")
      (setq hax/immediate-note-tags '())))

  (defun hax/tg-extract-date (msg)
    (with-temp-buffer
      (insert (fixup-text-dates msg))
      (goto-char 0)
      (re-search-forward (rx "["
                             (group (1+ digit)) "-" ;; 1 year
                             (group (1+ digit)) "-" ;; 2 month
                             (group (1+ digit)) " " ;; 3 day
                             (group (1+ digit)) ":" ;; 4 hour
                             (group (1+ digit)) ;; 5 minute
                             "]"))
      (list
       0 ;; sec
       (string-to-number (match-string 5)) ;; min
       (string-to-number (match-string 4)) ;; hour
       (string-to-number (match-string 3)) ;; day
       (string-to-number (match-string 2)) ;; mon
       (string-to-number (match-string 1)) ;; year
       )))

  (defun hax/tg-extract-tags (msg)
    (with-temp-buffer
      (insert msg)
      (goto-char 0)
      (-distinct (matches-in-buffer
                  (rx "#" (group (1+ (| word "_" "#"))))
                  (lambda () (match-string 1))))))
  (defun hax/tg-insert-note (msg)
    (interactive)
    (pcase (hax/tg-extract-date msg)
      (`(,sec ,min ,hour ,day ,month ,year)
       (hax/log
        "Inserted note with sec:%s min:%s hour:%s day:%s month:%s year:%s"
        sec min hour day month year)
       ;; (calendar-gregorian-from-absolute
       ;;  (time-to-days (encode-time sec min hour day month year))
       ;;  )
       (org-note-insert-at-time+date
        (s-trim msg) sec min hour day month year
        (append '("from_tg") (hax/tg-extract-tags msg)))
       )))

  (defun hax/tg-insert-selected-note (beginning end)
    (interactive "r")
    (hax/tg-insert-note (s-replace "𝖍𝖆𝖝𝖘𝖈𝖗𝖆𝖒𝖕𝖊𝖗"
                                   "haxscramper"
                                   (buffer-substring beginning end)))
    (kill-region beginning end)
    (hax/log (propertize "inserted note" 'face
                         `(:foreground ,(doom-color 'red))))))

(defun org-coords-open (path _)
  "Open org-mode link with coordinates"
  (browse-url (format "https://www.openstreetmap.org/#map=16/%s" path)))



(defun hax/clamp (value min max)
  (cond
   ((and min max (<= min value max)) value)
   ((and min (< value min)) min)
   ((and max (< max value)) max)
   (t value)))

(defun hax/org-end-of-the-day-stamp ()
  (format-time-string "<%Y-%m-%d %a 23:59:59>"))


(defun hax/counsel-org-tag-action (tag)
  (unless (equal tag "")
    (let* ((current-tags (counsel--org-get-tags))
           (had-tag (member tag current-tags)))
      (setq counsel-org-tags
            (if had-tag
                (delete tag (copy-sequence current-tags))
              (append current-tags (list tag))))
      (counsel-org--set-tags)
      (if had-tag
          (hax/insert-logbook-tag-entry tag 'removed)
        (hax/insert-logbook-tag-entry tag 'added)))))

(load! "lang-org.d/hax-org-configure.el")

(defconst hax/cache.d (expand-file-name "$HOME/.cache/haxscramper"))

(after! org
  (hax/org-mode-configure))

(after! org-special-block-extras
  (o-defblock
   parallel (cols 2) (bar nil)
   (format "[[<%s>]]" contents)))

(setq hax/immediate-note-content "")
(setq hax/immediate-note-tags '())

(defun hax/get-immediate-note-content ()
  hax/immediate-note-content)

(cl-defun hax/immediate-note-tags (&optional (predefined '()))
  (let ((tags (append predefined hax/immediate-note-tags)))
    (if (< 0 (length tags))
        (s-wrap (s-join ":" tags) ":" ":")
      "")))

(hax/immediate-note-tags)




(when nil
  (defface hax/org-speech
    `((t (:foreground ,(doom-lighten (doom-color 'base7) 0.6)
          :weight light
          :slant italic))) "Face for keystrokes"
    :group 'org-faces)

  (font-lock-replace-keywords
   'org-mode
   `((,(rx (group "\"" (+ (not "\"")) "\"")) (0 'hax/org-speech t)))))


(defvar hax/org-clock-stack '() "Stack of currently active tasks")
(defvar hax/org-clock-history '() "History of the clocked task activation")

(defun read-from-file (file)
  "Parse FILE as a serialized elisp value and return the result of
parsing."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (read (current-buffer))))

(defun write-to-file (file data)
  "Write DATA to FILE"
  (with-temp-file file
    (prin1 data (current-buffer))))


(defun advice-remove-all (sym)
  "AUTOMATICALLY remove all advice from a function symbol, without
having to go through repeated invocations of the same function
over and over as suggested in the
https://lists.gnu.org/archive/html/emacs-devel/2017-04/msg00767.html (the
use case is 'I want to automate the logic and this is a basic
function for cleaning up in case the 'smart' advice system shits
itself once again')"
  (interactive "aFunction symbol: ")
  (advice-mapc (lambda (advice _props) (advice-remove sym advice)) sym))

(defun hax/open-org ()
  (interactive)
  (find-file hax/main.org)
  (hax/org-mode-configure)
  (hax/org-mode-hook)
  (find-file hax/staging.org)
  (find-file hax/repeated.org)
  (when hax/+roam (org-roam-db))
  (find-file hax/notes.org))

(defun hax/org-get-logbook-ranges ()
  (org-element-map
      (hax/org-element-get-logbook)
      'clock
    (lambda (item) (org-element-property :value item))))

(defun hax/org-company-tags (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'hax/org-company-tags))
    (prefix (progn
              (when (and (looking-back "#\\w*")
                         (derived-mode-p 'org-mode))
                (company-grab-symbol))))
    (candidates (--filter (and (stringp it) (cl-search arg it))
                          (--map (car it) org-tag-alist)))
    (meta (format "This value is named %s" arg))))

(add-to-list 'company-backends 'hax/org-company-tags)

(defun tmp()
  (interactive)
  (setq
   company-backends
   '(company-capf
     (:separate hax/org-company-tags
                company-dabbrev company-yasnippet company-ispell)
     (:separate hax/org-company-tags
                company-ispell company-dabbrev company-yasnippet))))


(defun hax/after-inline-uppercase ()
  (interactive)
  (let* ((word (current-word)))
    (s-uppercase-p (substring word 0 1))))

(defun hax/flyspell-org-mode-verify ()
  "Customized wrapper around `org-mode-flyspell-verify' that also
skips capitalized and upperacsed words (names and abbreviations)"
  (if (eq (org-element-type
           (org-element-parent
            (org-element-at-point-no-context)))
          'quote-block)
      nil
    (when (org-mode-flyspell-verify)
      (not (hax/after-inline-uppercase)))))

(defun hax/org-element-inspect ()
  (interactive)
  (let* ((el (org-element-at-point)))
    (with-current-buffer (get-buffer-create "*element-inspect*")
      (delete-region (point-min) (point-max))
      (insert (format "%s" el)))))



(defun remove-string-properties (text)
  (with-temp-buffer
    (insert text)
    (buffer-substring-no-properties (point-min) (point-max))))

(load! "lang-org.d/hax-org-general-edit.el")
(load! "lang-org.d/hax-org-export.el")
(load! "lang-org.d/hax-org-keybinds.el")
