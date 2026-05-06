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
(load! "lang-org.d/hax-agenda-structured.el")
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

(load! "lang-org.d/hax-tg-message-edit.el")

(defun org-coords-open (path _)
  "Open org-mode link with coordinates"
  (browse-url (format "https://www.openstreetmap.org/#map=16/%s" path)))

(load! "lang-org.d/hax-org-configure.el")

(defun hax/open-org ()
  (interactive)
  (find-file hax/main.org)
  (hax/org-mode-configure)
  (hax/org-mode-hook)
  (find-file hax/staging.org)
  (find-file hax/repeated.org)
  (when hax/+roam (org-roam-db))
  (find-file hax/notes.org))

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

(defun remove-string-properties (text)
  (with-temp-buffer
    (insert text)
    (buffer-substring-no-properties (point-min) (point-max))))

(load! "lang-org.d/hax-org-general-edit.el")
(load! "lang-org.d/hax-org-export.el")
(load! "lang-org.d/hax-org-keybinds.el")
