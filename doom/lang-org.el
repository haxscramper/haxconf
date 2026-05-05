;;; -*- lexical-binding: t; -*-

(load! "lang-org.d/hax-agenda.el")
(load! "lang-org.d/hax-link-insertion.el")
(load! "lang-org.d/hax-link-insertion-code.el")
(load! "lang-org.d/hax-org-utils.el")
(load! "lang-org.d/hax-refile.el")
(load! "lang-org.d/hax-org-keybinds.el")
(load! "lang-org.d/subtree-navigation.el")

(defun hax/org-update-all-cookies ()
  (interactive)
  (let ((current-prefix-arg '(4)))
    (org-update-statistics-cookies "ALL")))

(defun hax/org-insert-todo-entry ()
  "Insert new TODO entry with creation date filled in. Simplified
down version of the `+org/insert-item-below' that does not do
anything context-aware. It simply inserts new 'TODO' entry after
the current one."
  (interactive)
  (org-end-of-subtree)
  (setq org-adapt-indentation t)
  (insert "\n\n" (make-string (or (org-current-level) 1) ?*) " TODO ")
  (setq org-expiry-inactive-timestamps t)
  (org-expiry-insert-created)
  (evil-insert-state)
  (hax/org-update-all-cookies)
  (hax/ensure-todo "TODO"))

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

(defun hax/org-download-setup ()
  (interactive)
  (require 'org-download)
  ;; NOTE function redefinition should happen after the hook.
  (defun org-download-insert-link (link filename)
    ;; HACK Setting `org-download-display-inline-images' to `nil' does not
    ;; work - I need to explicitly override function call with an empty
    ;; function. I also need to remove all advices that were put on this
    ;; symbol to prevent `+org' hook from triggering.
    (let* ((beg (point))
           (line-beg (line-beginning-position))
           str)
      (insert
       (format "[[file:%s][image]]"
               (org-link-escape
                (funcall org-download-abbreviate-filename-function filename))))
      (setq str (buffer-substring-no-properties line-beg (point)))
      str))

  (advice-remove-all 'org-download-insert-link)

  (let ((base
         (if (buffer-file-name) (buffer-file-name)
           (let ((buf (org-capture-get :buffer)))
             (if buf (buffer-file-name buf)) (f-join hax/indexed.d "images")))))
    (setq org-download-image-dir
          (f-join (f-dirname base) (concat (f-base base) ".images")))
    (setq org-download-timestamp (concat (f-filename base) "_%Y%m%d-%H%M%S_" ))
    (setq org-image-actual-width 300)
    ;; Update configuration to it's original values - some doom emacs
    ;; configuration changes these as well, and I don't need it.
    (setq org-download-link-format-function
          #'org-download-link-format-function-default)
    ;; Don't redisplay all images in file on each insertion - if I need to
    ;; redisplay them, I can do it perfectly well by myself.
    (setq org-download-display-inline-images nil)
    (setq org-download-abbreviate-filename-function #'file-relative-name)
    (setq org-download-method 'directory)
    (setq org-download-heading-lvl nil)))

(defun hax/org-paste-clipboard (&optional default-name to-monochrome-image)
  (interactive "P")
  (require 'org-download)
  (let ((file
         (if default-name
             (read-string
              (format "Filename [%s]: " org-download-screenshot-basename)
              nil nil org-download-screenshot-basename)

           default-name))
        (out (s-split
              "\n"
              (with-output-to-string
                (call-process
                 "xclip" nil standard-output nil
                 "-o" "-selection" "cli" "-t" "TARGETS")))))
    ;; If clipboard does not contain image use regular pasting logic,
    ;; otherwise insert image. Mapybe `p' should work differently in
    ;; org-mode instead, but I'm not sure about that.
    (if (--any? (s-starts-with? "image/" it) out)
        (progn
          ;; (when to-monochrome-image
          ;;   (shell-command-to-string
          ;;    "xclip -selection clipboard -t image/png -o > /tmp/org-down-colored.png")
          ;;   (shell-command-to-string
          ;;    "convert /tmp/org-down-colored.png -monochrome /tmp/monochrome.png")
          ;;   (shell-command-to-string
          ;;    "xclip -sel cli -t image/png -i /tmp/monochrome.png")
          ;;   )
          (org-download-clipboard file)
          )
      (evil-paste-after-without-register 1))))


(load! "lang-org.d/hax-tags.el")

(defun hax/org-capture-vc-root-dir ()
  (if (bound-and-true-p org-capture-mode)
      (let ((buffer (org-capture-get :original-buffer)))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (vc-root-dir))))
    (vc-root-dir)))

(defvar hax/select-from-list-or-add-history nil)

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

(defun hax/org-insert-uuid-anchor ()
  (interactive)
  (insert "<<")
  (kill-ring-save
   (point)
   (progn (uuidgen t) (point)))
  (insert ">>"))

(defun hax/insert-q ()
  (interactive)
  (insert "\\q{")
  (hax/org-insert-link 'person nil)
  (insert "}{")
  (save-excursion (insert "}")))

(defun at-empty-line () (and (not (bobp)) (looking-at-p "^\\s-*$")))

(defun backward-to-empty-line-after-non-empty ()
  "Move the cursor backwards to the empty line directly after the first non-empty line encountered."
  (interactive)
  (while (at-empty-line) (forward-line -1))
  (unless (bobp) (forward-line 1)))

(defun hax/maybe-numeric-prefix (else)
  "Return optional numeric prefix, if one was supplied for current
interactive function call"
  (if (and current-prefix-arg (not (consp current-prefix-arg)))
      (prefix-numeric-value current-prefix-arg)
    else))

(defun hax/?? (&optional last-n)
  (hax/log "[%s]" last-n))

(map! :n "M-s-d" (cmd! (hax/?? (hax/maybe-numeric-prefix))))

(defun org-get-known-file-buffers ()
  (save-excursion
    (let (entries)
      (dolist (b (buffer-list))
        (with-current-buffer b
          (when (derived-mode-p 'org-mode)
            (setq entries (nconc entries (list b))))))
      entries)))

(defun goto-marker (marker)
  (switch-to-buffer (marker-buffer marker))
  (goto-char (marker-position marker)))

(defun hax/org-collect-repeated-entries ()
  (save-window-excursion
    (save-excursion
      (--map
       (car it)
       (--filter
        (and (cdr it) (s-contains? "+" (cdr it)))
        (-map (lambda (tree)
                (goto-marker (cdr tree))
                (cons
                 tree
                 (and
                  (org-get-todo-state)
                  (-contains? '("TODO" "WIP" "POSTPONED" "NEXT")
                              (remove-string-properties (org-get-todo-state)))
                  (or (org-entry-get (point) "DEADLINE")
                      (org-entry-get (point) "SCHEDULED")))))
              (org-collect-known-entries)))))))

(defun hax/org-action-interactively (action &optional target)
  "Execution ACTION (function with no arguments) on the selected subtree position"
  (save-excursion
    (goto-marker (cdr (hax/org-select-subtree
                       (pcase target
                         ('repeated (hax/org-collect-repeated-entries))
                         ('active (hax/org-collect-active-entries t))
                         (_ (org-collect-known-entries))))))
    (funcall action)))

(defun hax/ensure-todo (state)
  (unless (s-equals? (substring-no-properties (org-get-todo-state)) state) (org-todo state)))

(defun hax/org-clock-in-interactively (&optional target)
  "Interactively select target to clock in using
`hax/org-collect-active-entries'"
  (interactive)
  (save-window-excursion
    (save-excursion
      (hax/org-action-interactively
       (lambda () (org-clock-in) (hax/ensure-todo "WIP"))
       target))))

(cl-defun hax/org-complete-interactively (&optional (state "COMPLETED") target)
  "Interactively select target to complete using
`hax/org-collect-active-entries'"
  (interactive)
  (save-window-excursion
    (save-excursion
      (hax/org-action-interactively
       (lambda () (hax/ensure-todo state))
       target))))

(cl-defun hax/org-insert-subtree-old-archive (&optional only-archives)
  (interactive)
  (hax/org-select-subtree-callback
   "Target subtree: "
   (lambda (x) (org-entry-put
                (point)
                "ARCHIVE_OLPATH_PARENT_ID"
                (hax/get-subtree-id-for-marker (cdr x))))
   'hax/org-insert-link-to-heading
   (org-collect-known-entries (org-get-known-file-buffers-archival only-archives))))

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

(defun diary-day (daylist)
  (memq (calendar-day-of-week date) daylist))

(defun diary-block-d (year month d-before d-after)
  "Diary block that accepts humane time ranges - year if first,
  then month, they day. Like every other sane person would do -
  like ISO standard does."
  (diary-block month d-before year month d-after year))

(defun diary-block-m (year m-before m-after d-before d-after)
  "Diary block in the same year, but with different month and day.
  Also uses *STANDARD* time part arrangement, instead of the
  default org-mode abomination."
  (diary-block m-before d-before year m-after d-after year))

(load! "lang-org.d/hax-org-data-query.el")
(load! "lang-org.d/hax-org-subtree-edit.el")


(defvar hax/org-src-use-full t
  "Org-src buffer edits should use full buffer, not small popup")

(defun hax/org-toggle-src-full ()
  (interactive)
  (setq hax/org-src-use-full (not hax/org-src-use-full)))

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

(defun ensure-text-around (expr)
  (if (= (- (line-end-position) 1) (point))
      (progn
        (goto-char (+ 1 (point)))
        (when (not (or (= (char-after) ?\s)
                       (= (char-before) ?\s)))
          (insert " "))
        (funcall expr))
    (progn
      (when (or (not (char-after))
                (not (= (char-after) ?\s)) ) (insert " "))
      (when (not (= (char-before) ?\s)) (insert " "))
      (funcall expr)
      (when (not (= (char-before ?\s))) (insert " "))
      (when (or (not (char-after))
                (not (= (char-after) ?\s))) (insert " ")))))

(defun hax/org-insert-text-tag ()
  "Insert org-mode hashtag under corrent cursor position, adding
necessary whitespace around it if he cursor wasn't positioned at
the empty area."
  (interactive)
  (ensure-text-around (lambda () (insert (concat "#" (hax/select-tag nil))))))

(defun hax/org-insert-timestamp ()
  "Insert org-mode hashtag under corrent cursor position, adding
necessary whitespace around it if he cursor wasn't positioned at
the empty area."
  (interactive)
  (ensure-text-around (lambda () (org-insert-time-stamp (current-time) t t))))

(defun hax/org-insert-timestamped-parens ()
  (interactive)
  (ensure-text-around
   (lambda ()
     (insert "((")
     (org-insert-time-stamp (current-time) t t)
     (insert "))"))))

(defun pop-selection ()
  (when (use-region-p)
    (let ((res (buffer-substring-no-properties (region-beginning) (region-end))))
      (delete-region (region-beginning) (region-end))
      res)))

;; (global-set-key (kbd "M-i") nil)



(defun hax/calibre-follow (path)
  (call-process "xdg-open" nil 0 nil (concat "calibre:" path)))

(defun hax/org-mode-flyspell ()
  "Ignore text between begin_quote and end_quote."
  (interactive)
  (setq ispell-skip-region-alist
        (cl-remove-if (lambda (region) (equal region '("begin_quote" . "end_quote")))
                      ispell-skip-region-alist))
  (add-to-list 'ispell-skip-region-alist '("begin_quote" . "end_quote")))

(defun hax/org-edit-id ()
  (interactive)
  (let* ((used (org-entry-get nil "ID"))
         (original (if used used "")))
    (org-entry-put nil "ID" (read-from-minibuffer ":ID:> " original))))

(defun hax/org-flyspell-hook ()
  (interactive)
  (hax/org-mode-flyspell)
  (setq flyspell-generic-check-word-predicate 'hax/flyspell-org-mode-verify))

(defun hax/org-mode-hook ()
  (interactive)
  ;; https://aliquote.org/post/enliven-your-emacs/ font-lock `prepend/append'
  ;; pair here is copied from this blog post, since I can't really figure out
  ;; what exactly is going on with ordering. But that implementation allows
  ;; me to override the default checkbox highlighting for checkboxes.

  (require 'evil-surround)
  (require 'counsel)
  (hax/log "Org-mode hook executed")
  (setq hl-todo-exclude-modes '(asdf-whatever-mode))
  (hl-todo-mode 1)
  ;; hl-todo-mode works randomly, most of the time it does not work after
  ;; the latest update, so I had to come up with my own mode here. 
  (hax/hl-todo-mode)

  (push '(?$ . ("\\(" . "\\)")) evil-surround-pairs-alist)
  (setq olivetti-body-width (+ 75 7))

  (org-link-set-parameters "calibre" :follow 'hax/calibre-follow)

  (setq flyspell-generic-check-word-predicate 'hax/flyspell-org-mode-verify)
  (abbrev-mode 1)
  (flyspell-mode 1)
  (org-indent-mode t)
  (hax/org-mode-flyspell)
  ;; Indentation guides slow down org-mode when there are multiple folds
  ;; (at least I was able to identifiy the implementation ot that point)
  ;; (highlight-indent-guides-mode -1)

  (hax/detail/configure-keybinds)
  (setq-local company-backends
              '(company-capf (:separate company-ispell company-dabbrev company-yasnippet))))

(map!
 ;; https://github.com/hlissner/doom-emacs/issues/3978#issuecomment-699004440
 ;; `:leader' key mapping is global, and I want to use `SPC-SPC' only for
 ;; org-mode. But local leader (`:localleader') is configured to be
 ;; `SPC-m', and I don't wnat to use this combinations (or change mnemnics
 ;; for every single other combination), so keybindings here are global,
 ;; with hack for specific goto configurations.
 ;;
 ;; Not specifying `:map' as `:leader' takes priority anyway.
 :leader
 :n "SPC" #'hax/replace-next-placeholder
 :nv "si" (cmd! (call-interactively
                 (cond
                  ((eq major-mode 'org-mode) 'counsel-org-goto)
                  (t 'counsel-imenu)))) )

(add-hook! 'org-mode-hook 'hax/org-mode-hook)


(defun hax/org-capture-pre-finalize ()
  (save-excursion
    ;; Automatically dedent subtree content to the line 0 --
    ;; `org-adapt-indentation' does not work for content separately so this
    ;; adjustment is required in order to get a sane automatic content
    ;; placement using `org-note-insert-at-time+date' that I use for
    ;; automatic note additions.
    (goto-char (point-min))
    (let* ((el (org-element-at-point))
           (end (org-element-property :end el)))
      (hax/log "reformattting %s %s" end (point-max))
      (goto-char end)
      (indent-code-rigidly end (point-max) (* -1 (current-line-indent))))))

(defun hax/org-capture-after-finalize ()
  )

(defmacro push-tmp-value! (variable value body)
  `(let ((tmp-value ,variable))
     (setq ,variable ,value)
     ,body
     (setq ,variable tmp-value)))

(defun hax/org-capture-hook ()
  (interactive)
  (map!
   :map org-capture-mode-map
   "C-c C-k" (cmd!
              (push-tmp-value!
               hax/delete-region-as-kill nil
               (org-capture-kill))))
  ;; I use adaptive indentation for drawers, but I don't want to forcefully
  ;; indent the text inside of the subtrees.
  (let ((text (buffer-substring (line-beginning-position) (point))))
    (when (string-match "^\s+$" text)
      (hax/log "%s" text)
      (delete-region (line-beginning-position) (point)))))

;; Scroll to the last position of the message buffer after something has
;; been printed.
(defadvice message (after message-tail activate)
  "goto point max after a message"
  (with-current-buffer "*Messages*"
    (goto-char (point-max))
    (walk-windows
     (lambda (window)
       (if (string-equal (buffer-name (window-buffer window)) "*Messages*")
           (set-window-point window (point-max))))
     nil
     t)))


(add-hook! 'org-capture-mode-hook 'hax/org-capture-hook)
(setq hax/fullscreen-capture nil)
(setq hax/fullscreen-client-name nil)
(defun hax/org-post-capture-hook ()
  (interactive)
  ;; If starting capture from dropdown mode, delete the client window frame
  ;; after. That won't work if it is the single client, but when launching
  ;; using daemon mode it does /exactly/ what is needed.
  (when hax/fullscreen-capture
    (delete-frame))
  ;; Fullscreen capture must be set again with new `--eval' argument
  ;; when starting next cature.
  (setq hax/fullscreen-capture nil)
  (setq hax/fullscreen-client-name nil))

(add-hook! 'org-capture-prepare-finalize-hook 'hax/org-capture-pre-finalize)
(add-hook! 'org-capture-after-finalize-hook 'hax/org-capture-after-finalize)

(setq org-latex-listings 'minted
      org-latex-packages-alist '(("" "minted"))
      org-latex-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

(setq warning-minimum-level :error)

(add-hook! 'org-agenda-mode-hook 'hax/agenda-mode-hook)

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

;; Store capture location *before* the capture happens, this way location
;; information is reported to the hook properly.
(defvar hax/org-capture-from-line nil)
(defun hax/org-pre-capture-hook (&optional fun args)
  (interactive)
  (setq hax/org-capture-from-line (line-number-at-pos)))

(defvar hax/delete-region-as-kill t)
(defun hax/delete-region (fun &rest args)
  (if hax/delete-region-as-kill
      (apply fun args)
    (apply 'delete-region args)))

(advice-add 'kill-region :around #'hax/delete-region)

(advice-add 'org-capture :before #'hax/org-pre-capture-hook)

(require 'uuidgen)

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

(defun org-odt-inline-src-block (_inline-src-block _contents _info)
  "Transcode an INLINE-SRC-BLOCK element from Org to ODT.
  CONTENTS holds the contents of the item.  INFO is a plist holding
  contextual information."
  (format "<text:span text:style-name=\"%s\">%s</text:span>"
          "OrgCode" (org-odt--encode-plain-text
                     (org-element-property :value _inline-src-block))))


(setq
 org-expiry-created-property-name "CREATED"
 org-expiry-inactive-timestamps   t
 )

(defun hax/insert-created-timestamp()
  "Insert a CREATED property using org-expiry.el for TODO entries"
  (interactive)
  (org-expiry-insert-created)
  (org-back-to-heading)
  (org-end-of-line)
  (insert " "))

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


(defun hax/org-insert-subtree-comment ()
  (interactive)
  (insert (format "%s COMMENT" (s-repeat (+ 1 (org-current-level)) "*")))
  (org-expiry-insert-created))

(defun hax/clamp (value min max)
  (cond
   ((and min max (<= min value max)) value)
   ((and min (< value min)) min)
   ((and max (< max value)) max)
   (t value)))

(defun hax/org-insert-subtree-offset (title level-offset)
  (interactive "sTitle: ")
  (insert
   (format
    "%s %s"
    (s-repeat (hax/clamp (+ level-offset (org-current-level)) 1 nil)
              "*")
    title))
  (org-id-get-create)
  (org-expiry-insert-created))

(defun hax/org-insert-subtree-same (title)
  (interactive "sTitle: ")
  (hax/org-insert-subtree-offset title 0))

(defun hax/org-insert-subtree-below (title)
  (interactive "sTitle: ")
  (hax/org-insert-subtree-offset title 1))

(defun hax/org-insert-subtree-above (title)
  (interactive "sTitle: ")
  (hax/org-insert-subtree-offset title -1))

(defun hax/org-end-of-the-day-stamp ()
  (format-time-string "<%Y-%m-%d %a 23:59:59>"))

(require 'f)

(when hax/+roam
  (defvar org-roam-db-update-queue
    (list) "List of the files for delayed org-roam update")

  (defun org-roam-db-schedule-update-file (&optional file-path)
    ;; do same logic as original to determine current file-path if not
    ;; passed as arg
    (setq file-path (or file-path (buffer-file-name (buffer-base-buffer))))
    (with-temp-buffer
      (find-file file-path)
      (if (-contains-p (org-get-tags) "DO_NOT_ORG_ROAM")
          (hax/log "org-roam: skipping update of %s" file-path)
        (progn (hax/log "org-roam: scheduling update of %s" file-path)
               (if (not (memq file-path org-roam-db-update-queue))
                   (push file-path org-roam-db-update-queue))))))

  ;; this function will be called when emacs is idle for a few seconds
  (defun org-roam-db-idle-update-files ()
    ;; go through queued filenames one-by-one and update db
    ;; if we're not idle anymore, stop. will get rest of queue next idle.
    (while (and org-roam-db-update-queue (current-idle-time))
      (let ((file (pop org-roam-db-update-queue)))
        (hax/log "org-roam: running update of %s" file)
        ;; apply takes function var and list
        (org-roam-db-update-file file))))

  ;; we'll only start updating db if we've been idle for this many seconds
  (run-with-idle-timer 5 t #'org-roam-db-idle-update-files)

  (defun hax/org-after-save ()
    (when (derived-mode-p 'org-mode)
      (org-roam-db-schedule-update-file (buffer-file-name))))

  (add-hook! 'after-save-hook #'hax/org-after-save))

(when hax/+roam
  (setq
   ;; Directory to store org-roam topic files. Because I can both write
   ;; standalone conceptual notes and reference them in daily captures I see
   ;; no reason to separate those two.
   org-roam-directory hax/indexed.d
   ;; I implemented custom update on save + idle refresh, so no need to use
   ;; built-in implementation
   org-roam-db-update-on-save nil
   ;; Main index file for org-roamm
   org-roam-index-file (f-join org-roam-directory "roam-index.org")
   ;; Allow exclusion of certain singular files that are tagged with
   ;; `DO_NOT_ORG_ROAM' filetag.
   org-roam-db-node-include-function
   (lambda () (if (member "DO_NOT_ORG_ROAM" (org-get-tags)) nil t))
   ;; Remove all formatting from titles - bold, italic, verbatim etc. are not
   ;; shown in the roam UI anyway, and I mostly view the graph/database using
   ;; it.
   org-roam-node-formatter (lambda (node) (hax/org-unformat-title
                                           (org-roam-node-title node))))
  ;; Because one of the main roam configuration variables it not good
  ;; enough of an authority to warrant implicit directory creation *if it
  ;; is missing*.
  (if (not (f-exists-p org-roam-directory))
      (f-mkdir-full-path org-roam-directory)))

(setq
 org-highlight-latex-and-related '(latex script entities)
 ;; Main notes directory
 org-directory "~/defaultdirs/notes/personal"
 ;; Do not fontify super/sub-scripts differently from the rest of the text.
 tex-fontify-script nil
 ;; File with locations of the org-id entries
 org-id-locations-file (f-join org-directory ".org-id-locations")
 ;; Directory for todo management and other indexed entries
 hax/indexed.d (f-join org-directory "indexed")
 ;; Main GTD organizer
 hax/main.org (f-join hax/indexed.d "main.org")
 ;; Hot cache of immediately targeted tasks
 hax/staging.org (f-join hax/indexed.d "staging.org")
 ;; Random junk notes that I generate, copy from other places etc.
 hax/notes.org (f-join hax/indexed.d "notes.org")
 hax/repeated.org (f-join hax/indexed.d "repeated.org")
 hax/fic.org (f-join hax/indexed.d "fic.org")
 ;; Project configuration
 hax/projects.org (f-join hax/indexed.d "projects.org")
 hax/projects_cold.org (f-join hax/indexed.d "projects_cold.org")
 org-structure-template-alist '(;; ("f" . "formula\n")
                                ;; ("a" . "export ascii\n")
                                ;; ("d" . "definition\n")
                                ;; ("c" . "center\n")
                                ;; ("C" . "comment\n")
                                ;; ("e" . "example\n")
                                ;; ("E" . "export\n")
                                ;; ("h" . "export html\n")
                                ;; ("l" . "export latex\n")
                                ;; ("q" . "quote\n")
                                ;; ("s" . "src\n")
                                ;; ("v" . "verse\n")
                                ))

(require 'ivy)
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

(defface hax/org-agenda-header
  '((t :inherit org-agenda-structure
     :extend t))
  "Face for custom agenda section headers.")

(set-face-attribute 'hax/org-agenda-header nil
                    :background (doom-darken (doom-color 'red) 0.5))

(defun hax/org-agenda-style-headers ()
  "Add empty line after headers and style them with red background."
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (when (get-text-property (point) 'org-agenda-structural-header)
          (let ((end (line-end-position)))
            (add-text-properties (line-beginning-position) end
                                 '(face hax/org-agenda-header))
            ;; Extend face to full width
            (goto-char end)
            (unless (and (not (eobp))
                         (save-excursion (forward-line 1)
                                         (looking-at "^\\s-*$")))
              (insert "\n"))))
        (forward-line 1)))))



(setq org-duration-format '((special . h:mm)))

(defun hax/org-agenda-hook ()
  (display-line-numbers-mode 1)
  (hax/org-agenda-style-headers)
  (hax/org-agenda-delete-empty-blocks)
  )

(add-hook 'org-agenda-finalize-hook #'hax/org-agenda-hook)





(setq org-startup-indented t)

(defun hax/org-agenda-skip ()
  (or (hax/org-has-tag "no_agenda")
      (hax/org-agenda-skip-recurring)))

(defun hax/org-agenda-skip-low-priority ()
  (save-excursion
    (org-back-to-heading t)
    (unless (or (re-search-forward ":bug:" (line-end-position) t)
                (re-search-forward "#[XAS]" (line-end-position) t))
      (or (outline-next-heading) (point-max)))))

(defun hax/org-mode-configure ()
  (interactive)
  ;; Default inline latex highlighting is a bold white text, which is too
  ;; similar to a regular text.
  ;; (set-face-attribute
  ;;  'org-latex-and-related nil
  ;;  :foreground "dim gray")

  (setq flyspell-generic-check-word-predicate 'hax/flyspell-org-mode-verify)

  (defface org-bold `((t :inherit bold :foreground ,(doom-lighten 'red 0.7)))
    "Face for bold text in Org mode.")

  (defface org-italic `((t :inherit italic :foreground ,(doom-lighten 'yellow 0.7)))
    "Face for bold text in Org mode.")

  (defface org-underline `((t :inherit underline :foreground ,(doom-lighten 'cyan 0.7)))
    "Face for bold text in Org mode.")

  ;; Update the org-emphasis-alist
  (setq org-emphasis-alist
        '(("*" org-bold)
          ("/" org-italic)
          ("_" org-underline)
          ("=" org-verbatim)
          ("~" org-code)
          ("+" (:strike-through t))))

  (global-company-mode -1)
  (org-link-set-parameters "coords" :follow #'org-coords-open)
  (require 'ts)
  (require 'org-expiry)
  (require 'org-clock)
  (define-abbrev-table 'org-mode-abbrev-table
    '(("anon" "anonymous")
      ("inf" "infinite")
      ("vm" "VM")
      ("Flugel" "Fl\\Uuml{}gel")
      ;; Yes, I can't type for shit
      ("ofthe" "of the")
      ("tothe" "to the")
      ("onthe" "on the")
      ("forthe" "for the")
      ("athe" "at the")
      ("inthe" "in the")
      ("ast" "AST")
      ("teh" "the")
      ("im" "I'm")
      ("ambig" "ambiguous")
      ("assesment" "assessment")
      ("i" "I")))
  (setq org-priority-highest ?A)
  (setq org-priority-lowest ?X)
  (setq org-priority-default ?B)



  (setq org-priority-faces '((?A . (:foreground "#F0DFAF" :weight bold :underline t :overline t))
                             (?B . (:foreground "#FD971F"))
                             (?C . (:foreground "#66D9EF"))
                             (?D . (:foreground "#A1EFE4"))
                             (?E . (:foreground "#A6E22E" :weight light))
                             (?F . (:foreground "#AE81FF" :weight light))
                             (?S . (:foreground "#FD5FF0" :box t))
                             (?X . (:foreground "red" :box (:line-width 2 :color "red")))))

  (setq
   org-cycle-separator-lines 12000
   org-capture-templates
   ;; agenda just includes everything that contains an "active time
   ;; stamp". An active time stamp is any time stamp in angular
   ;; brackets. That's why all templates use `%U' instead of `%T' for
   ;; timestamps - creation date should not be inserted in the agenda.
   '(;; Add new entry to the inbox. No sorting, no hierarchical placement,
     ;; just dump everything in it, refile later.
     ("d" "Daily" entry (file+olp+datetree hax/notes.org)
      "** %U W%<%U> :recollection##day:
  :PROPERTIES:
  :CREATED: %U
  :END:

%?"
      :empty-lines-before 1
      :empty-lines-after 1)
     ("@" "Daily" entry (file+olp+datetree hax/notes.org)
      "** %U %(hax/immediate-note-tags)
  :PROPERTIES:
  :CREATED: %U
  :END:

%(hax/get-immediate-note-content)
"
      :immediate-finish t
      :empty-lines-before 1
      :empty-lines-after 1)
     ;; Immediate plans for today. Similar to GTD inbox, but reserved for
     ;; very minor items used to organize the thoughts. Items from here can
     ;; duplicate others (heading might be a direct link), and they are not
     ;; refiled anywhere else.
     ("i" "Immediate" entry (file+olp+datetree hax/notes.org)
      "* TODO %?
  :PROPERTIES:
  :CREATED: %U
  :END:
"
      :prepend t
      :empty-lines-after 1
      :empty-lines-before 1)
     ("D" "Daily start" plain (file+olp+datetree hax/notes.org)
      "%i%?"
      :empty-lines-before 1
      :empty-lines-after 1)
     ("c" "Clock" entry (clock)
      ;; (function hax/goto-top-clock)
      "** TODO %?
  :PROPERTIES:
  :CREATED: %U
  :END:
"
      :empty-lines-before 1
      :empty-lines-after 1)
     ("S" "Staging subtree" entry
      (function (lambda () (hax/org-goto-select-subtree
                            (org-collect-known-entries (list hax/staging.org)))))
      "** TODO %?
  :PROPERTIES:
  :CREATED: %U
  :END:
"
      :empty-lines-before 1
      :empty-lines-after 1
      )
     ("s" "Staging toplevel" entry (file hax/staging.org)
      ;; (function hax/goto-top-clock)
      "* TODO %?
  :PROPERTIES:
  :CREATED: %U
  :END:
"
      :empty-lines-before 1
      :empty-lines-after 1
      )
     ;; Todo items that don't have a concrete deadline yet, but must be
     ;; positioned under some task
     ("u" "Subtask under" entry
      (function hax/org-goto-select-subtree)
      "** TODO %?
  :PROPERTIES:
  :CREATED: %U
  :END:
"
      :empty-lines-before 1
      :empty-lines-after 1
      )
     ("a" "Subtask under active" entry
      (function hax/org-goto-select-active-subtree)
      "** TODO %?
  :PROPERTIES:
  :CREATED: %U
  :END:
"
      :empty-lines-before 1
      :empty-lines-after 1)
     ))

  (when hax/+roam
    (setq
     ;; Don not open org-roam buffer automatically
     +org-roam-open-buffer-on-find-file nil
     ;; Don't implicitly enable the super-distracting,
     ;; almost-never-needed-unless-explicitly requested automatic UI follow
     ;; mode that zooms to closely no nodes that you instantly loose all the
     ;; context of what you are working with.
     org-roam-ui-follow nil))

  (setq
   ;; Agenda is a main todo file and inbox
   org-agenda-files (list hax/main.org
                          hax/staging.org
                          hax/notes.org
                          hax/repeated.org
                          hax/projects.org
                          hax/projects_cold.org)
   org-refile-targets `((nil :maxlevel . 4)
                        (,hax/fic.org :maxlevel . 9)
                        (,hax/main.org :maxlevel . 3)
                        (,hax/projects.org :maxlevel . 7)
                        (,hax/projects_cold.org :maxlevel . 7)
                        (,hax/notes.org :maxlevel . 3)
                        (,hax/staging.org :maxlevel . 1))
   ;; Yes, you can in fact consider the entry completed with some of the
   ;; `TODO' left over, this happens, in real life not all tasks must be
   ;; closed with 100% accuracy.
   org-enforce-todo-dependencies nil
   ;; Add `COMPLETED' timestamp property
   org-log-done 'time
   ;; Log when schedule changed
   org-log-reschedule 'time
   org-tags-column -1
   ;; Notes should go from top to bottom
   org-log-states-order-reversed nil
   ;; Log when deadline changed
   org-log-redeadline 'time
   org-clock-modeline-total 'today
   ;; Not using default configuration - instead more informative version is
   ;; implemented using custom hooks.
   org-log-refile nil
   ;; This looks nice but has a lot of random glitches on this
   ;; configuration
   org-startup-indented t
   ;; Start week on monday like any normal human being
   calendar-week-start-day 1
   ;; Insert newlines before heading, but plain lists should be packed
   ;; together.
   org-blank-before-new-entry '((heading . t) (plain-list-item . nil))
   org-hierarchical-todo-statistics t
   ;; Override of the default agenda date formatting with customized
   ;; function
   org-agenda-format-date 'hax/org-agenda-format-date
   org-goto-interface 'outline-path-completionp
   org-image-actual-width (list 300)
   ;; Store seconds in the timestamp format
   org-time-stamp-formats '("<%Y-%m-%d %a>" . "<%Y-%m-%d %a %H:%M:%S %Z>")
   ;; Fontification of the 'headings' also affects checkbox items, and I
   ;; tend to them on several lines where heading fontification only
   ;; highlights first one. So essentially this features is pretty nice,
   ;; but just doesn't work properly for my use case
   org-fontify-done-headline nil
   org-startup-with-inline-images nil
   ;; `:DRAWER:' should be indented to the heading
   org-adapt-indentation t
   ;; Notes
   org-log-into-drawer t
   ;; Don't remove zero-length clocks. Most tasks can't be accomplished
   ;; under a minute, but I like to avoid having extra smart filtering of
   ;; the actions I make. If I need to do the analysis of my actions I can
   ;; filter out outliers manually
   org-clock-out-remove-zero-time-clocks nil)

  (setq
   org-agenda-custom-commands
   `(("l" "Long"
      ((agenda
        ""
        ((org-agenda-span 30)
         (org-agenda-start-day "-0d")
         (org-deadline-warning-days 35)))))
     ("*" "All"
      ((todo
        "NEXT|WIP|PAUSED|BLOCKED"
        ((org-agenda-overriding-header "In progress (NEXT/WIP/PAUSED/BLOCKED)")))
       (todo
        "TODO"
        ((org-agenda-overriding-header "Staging todo")
         (org-agenda-skip-function #'hax/org-agenda-skip)
         (org-agenda-files '(,hax/staging.org))))
       (todo
        "TODO"
        ((org-agenda-overriding-header "Notes & High priority project todos")
         (org-agenda-skip-function
          (lambda ()
            (cond
             ((string= (buffer-file-name) (expand-file-name hax/notes.org))
              (hax/org-agenda-skip))
             ((string= (buffer-file-name) (expand-file-name hax/projects.org))
              (hax/org-agenda-skip-low-priority))
             (t (point-max)))))
         (org-agenda-files (list hax/notes.org hax/projects.org))))
       (agenda
        ""
        ((org-agenda-overriding-header "2-week preview")
         (org-agenda-span 14)
         (org-agenda-start-day "-0d")
         (org-agenda-skip-function #'hax/org-agenda-skip)
         (org-deadline-warning-days 0)))
       (todo
        "TODO"
        ((org-agenda-overriding-header "Repeated todos")
         (org-agenda-files '(,hax/repeated.org))))))))

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((C . t)
     (nim . t)
     (shell . t)
     (mermaid . t)
     (sqlite . t)
     (python . t)))
  ;; Because fucking mmdc is /so/ stupid it cannot find `(which chromium-browser)'
  (setenv "PUPPETEER_EXECUTABLE_PATH" "/bin/chromium-browser")
  ;; And yes, whatever regular npm install does, it manages to fuck it up.
  (setq ob-mermaid-cli-path (f-expand "~/software/node_modules/.bin/mmdc"))
  (setq org-latex-listings 'minted)
  (setq org-todo-keywords
        ;; I don't use most of these keywords as well, and sometimes they
        ;; overlap with GTD management. You can think of them as "yes" and
        ;; "yes, but in green"
        '(("TODO(t!/!)"         ;; Must be done now
           "TODO(t!/!)"         ;; Some other function breaks
           ;; org-todo-keywords, and I need to add
           ;; TODO twice here. I don't know what is
           ;; the cause, but this is a FIXME, although
           ;; with low priority.
           "WIP(w!)"            ;; Working on it
           "NEXT(n!)"            ;; Working on it
           "TRIAGED(T!)"        ;; Investigated the issue in some detail,
           ;; might work on it later
           "REVIEW(r!/!)"       ;; Check if this task must be done or not
           "|"
           ;; Work is temporarily paused, but I have a vague idea about
           ;; next restart time.
           "PAUSED(p@/!)"
           "BLOCKED(b@/@)"   ;; External event is preventing further work
           "TIMEOUT(T)"      ;; Cannot be done due to time limits
           "FAILED(f@/@)"    ;; Tried to finish the task but failed
           "CANCELED(C@/@)"
           "COMPLETED(c!/@)" ;; Task completed
           "PARTIALLY(P@/@)" ;; Can be considered completed
           )))
  (setq org-todo-keyword-faces
        `(("TODO" . "orange")
          ("PAUSED" . "coral")
          ("CANCELED" . "snow")
          ("WIP" . "tan")
          ("BLOCKED" . "purple")

          ("PARTIALLY" . "goldenrod")
          ("REVIEW" . "SteelBlue")
          ("FAILED" . ,(doom-color 'red))
          ("TIMEOUT" . ,(doom-color 'red))))
  (setq hax/tags-file (f-full "~/.config/tags"))
  (setq hax/private-tags-file (f-full "~/.hax-private-tags"))
  (setq hax/private-tags-prefix-list '("work##vizio" "work##epam"))
  (when (f-exists? (f-join hax/cache.d "org-clock-stack"))
    (setq hax/org-clock-stack
          (read-from-file (f-join hax/cache.d "org-clock-stack"))))

  (when (f-exists? (f-join hax/cache.d "org-clock-history"))
    (setq hax/org-clock-history
          (read-from-file (f-join hax/cache.d "org-clock-history"))))
  (when (not (f-exists? hax/tags-file)) (f-touch hax/tags-file))
  (setq org-tag-alist
        (let ((tag-list
               (append
                (s-split "\n" (if (f-exists? hax/tags-file)
                                  (f-read hax/tags-file) "") t)
                (s-split "\n" (if (f-exists? hax/private-tags-file)
                                  (f-read hax/private-tags-file) "") t))))
          (cl-remove-duplicates
           (--map
            `(,(substring it 1 (length it)) . ??)
            (--filter (< 0 (length it)) tag-list))
           :test #'equal))))

(defconst hax/cache.d (expand-file-name "$HOME/.cache/haxscramper"))

(after! org
  (require 'org-capture)
  (hax/org-mode-configure))

(setq
 org-odt-category-map-alist
 '(("__Table__" "Figure" "value" "Таблица"
    org-odt--enumerable-p)
   ("__Figure__" "Figure" "value" "Рисунок"
    org-odt--enumerable-image-p)
   ("__MathFormula__" "Text" "math-formula" "Equation"
    org-odt--enumerable-formula-p)
   ("__DvipngImage__" "Equation" "value" "Equation"
    org-odt--enumerable-latex-image-p)
   ("__Listing__" "Listing" "value" "Листинг"
    org-odt--enumerable-p)))

(setq
 org-odt-transform-processes
 '(("Optimize Column Width of all Tables"
    "soffice" "--norestore" "--invisible" "--headless"
    "macro:///OrgMode.Utilities.OptimizeColumnWidth(%I)")
   ("Update All"
    "soffice" "--norestore" "--invisible" "--headless"
    "macro:///OrgMode.Utilities.UpdateAll(%I)")
   ("Reload"
    "soffice" "--norestore" "--invisible" "--headless"
    "macro:///OrgMode.Utilities.Reload(%I)")))

(setq org-odt--entry-template "
      <text:table-of-content-entry-template text:outline-level=\"%d\" text:style-name=\"Contents_20_%d\">
       <text:index-entry-link-start text:style-name=\"Internet_20_link\"/>
       <text:index-entry-chapter/>
       <text:index-entry-text/>
       <text:index-entry-tab-stop style:type=\"right\" style:leader-char=\".\"/>
       <text:index-entry-page-number />
       <text:index-entry-link-end/>
      </text:table-of-content-entry-template>\n")

(defun org-odt--format-toc (title entries depth)
  "Return a table of contents.
TITLE is the title of the table, as a string, or nil.  ENTRIES is
the contents of the table, as a string.  DEPTH is an integer
specifying the depth of the table."
  (concat
   "
<text:table-of-content text:style-name=\"OrgIndexSection\" text:protected=\"true\" text:name=\"Table of Contents\">\n"
   (format "  <text:table-of-content-source text:outline-level=\"%d\">" depth)
   (and title
        (format "
    <text:index-title-template text:style-name=\"Contents_20_Heading\">%s</text:index-title-template>
"
                title))

   (let ((levels (number-sequence 1 10)))
     (mapconcat
      (lambda (level)
        (format org-odt--entry-template level level)) levels ""))
   "
  </text:table-of-content-source>
  <text:index-body>"
   (and title
        (format "
    <text:index-title text:style-name=\"Sect1\" text:name=\"Table of Contents1_Head\">
      <text:p text:style-name=\"Contents_20_Heading\">%s</text:p>
    </text:index-title>\n"
                title))
   entries
   "
  </text:index-body>
</text:table-of-content>"))

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

;;; Reimplementation of the built-in org-mode clock system with something
;;; that works properly instead of failing on every corner. Fixes following
;;; (and other) bugs and misfeatures: (*) Clocks are not truly persistent,
;;; I need to re-enable current clock to restore the time when emacs was
;;; turned off. (*) Undoing clock activation breaks emacs' brain and need
;;; to manually discard all the active clock time. (*) Clocking does not
;;; support nested tasks - I'm more than capable of subdividing current
;;; activity into shorter todo items as I go, clocking them in and finaly
;;; finishing everything. (*) cannot "pause and resume" clock - no
;;; immediate history of activity.

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


(defun org-element-subtree-by-id (id)
  "Get parsed org-element for entry with `id', or `nil' if no such
subtree can be found."
  (with-temp-buffer
    (org-id-open id nil)
    (org-element-at-point)))


(defun org-get-logbook-extent ()
  "Get start and end position of the `:LOGBOOK:' drawer for current
subtree."
  (save-excursion
    (unless (org-at-heading-p) (outline-previous-heading))
    (goto-char (- (org-log-beginning t) 2))
    ;; For unknown reasons `org-log-BEGINNING' puts cursor on the END of
    ;; the logbook. Super intuitive, I know.
    (search-backward ":LOGBOOK:")
    (let* ((elt (org-element-property-drawer-parser nil))
           (beg (org-element-property :contents-begin elt))
           (end (org-element-property :contents-end elt)))
      (cons beg end))))

(defun org-get-logbook-notes ()
  "Get text of the org-mode logbook drawer notes"
  (let ((range (org-get-logbook-extent)))
    (buffer-substring-no-properties (car range) (cdr range))))

(defun hax/tmp ()
  (interactive)
  (let ((con (org-get-logbook-extent)))
    (hax/org-last-active-clock (car con) (cdr con))))

(defun org-element-parse-string (str)
  "Parse input string as org-mode buffer and return result"
  (with-temp-buffer
    (insert str)
    (org-element-parse-buffer)))


(defun hax/org-finish-timestamp ()
  (when (and (re-search-forward (rx "]") nil t)
             (not (org-at-date-range-p t)))
    (insert "--")
    (org-insert-time-stamp (org-current-time) 'with-hm 'inactive)
    (org-evaluate-time-range)))

(defun hax/maybe-finish-active-clock ()
  "Finsh last active clock if current subtree has any. If any
changes where made, return `t', otherwise return nil."
  (save-excursion
    (let ((active (hax/org-get-active-clock-timestamp-position)))
      (when active
        (goto-char active)
        (hax/dbg/looking-at)
        (hax/org-finish-timestamp)
        t))))

(defun hax/org-clock-out-this-task ()
  "Clock out task under cursor if logbook contains any active
items. If logbook is does not exist, might create new one."
  (interactive)
  ;; Find timestamp position in the current active clock of the entry
  (hax/maybe-finish-active-clock))

(defun hax/current-timestamp ()
  (with-temp-buffer
    (org-insert-time-stamp (org-current-time) 'with-hm 'inactive)
    (buffer-substring (point-min) (point-max))))

(defun hax/org-clock-in-this-task (&optional select start-time)
  "Add current task the clock stack"
  (interactive)
  (save-excursion
    (push (org-id-get-create) hax/org-clock-history)
    (push (org-id-get-create) hax/org-clock-stack)
    (org-back-to-heading)
    (forward-line)
    (hax/maybe-finish-active-clock)
    (org-add-log-entry (format "CLOCK: %s" (hax/current-timestamp)))))




(defun hax/org-clock-out-task (id)
  "Clock out the task with ID from the stack and all
subsequent (nested) ones"
  (when (not id) (error "Cannot clock out `nil' task."))
  (save-excursion
    (hax/log "Finished task %s" id)
    (org-id-open id nil)
    (hax/dbg/looking-at)
    (hax/org-clock-out-this-task)))

(defun hax/org-clock-out-top-task
    (&optional SWITCH-TO-STATE FAIL-QUIETLY AT-TIME)
  "Clock out the topmost task from the stack."
  (interactive)
  (hax/org-clock-out-task (pop hax/org-clock-stack)))

(defun hax/around-org-clock-in (fun &rest args)
  (interactive)
  (funcall-interactively 'hax/org-clock-in-this-task args))

(defun hax/around-org-clock-out (fun &rest args)
  (interactive)
  (funcall-interactively 'hax/org-clock-out-top-task args))

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

(when nil
  (advice-add 'org-clock-in :around #'hax/around-org-clock-in)
  (advice-add 'org-clock-out :around #'hax/around-org-clock-out))

(defun hax/goto-top-clock ()
  (interactive)
  (let ((top (car (last hax/org-clock-stack))))
    (if top
        (org-id-open top nil)
      (error "No active clocked item - cannot add subtask"))))

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

(defun hax/org-get-entry-activations ()
  "Return (TODO, now prints) full list of the activation ranges for
all known agenda entries."
  (let* ((now (ts-now))
         (year (ts-year now))
         (month (ts-month now))
         (day (ts-day now)))
    (--each (org-map-entries
             (lambda ()
               (let* ((ranges (hax/org-get-logbook-ranges))
                      (filtered
                       (when ranges
                         (--filter
                          (and
                           (eq (org-element-property :year-end it) year)
                           (eq (org-element-property :month-end it) month)
                           (eq (org-element-property :day-end it) day))
                          ranges))))
                 (when (and ranges (< 0 (length filtered)))
                   (cons (org-get-outline-path t) ranges))))
             nil
             `(,hax/main.org))
      (when it
        (hax/log "%s" (org-format-outline-path (-slice (car it) -2) 256))
        (dolist (time (cdr it))
          (hax/log "  %s" (org-element-property :raw-value time)))))))

(defun hax/closest-unicode-fraction (value)
  (let* ((values '((1 . "⅟")
                   (0 . "⁰")
                   (0.25 . "¼")
                   (0.5 . "½")
                   (0.75 . "¾")
                   (0.33 . "⅓")
                   (0.66 . "⅔")
                   (0.2 . "⅕")
                   (0.4 . "⅖")
                   (0.6 . "⅗")
                   (0.8 . "⅘")
                   (0.16 . "⅙")
                   (0.83 . "⅚"))))
    (cdr (first (sort (--map (cons (abs (- (abs value) (car it))) (cdr it)) values)
                      (lambda (lhs rhs) (< (car lhs) (car rhs))))))))

;; (hax/closest-unicode-fraction 0.5)

(defun hax/relative-hour-fraction (tdiff)
  (/ (if (< 0 tdiff)
         (mod (/ tdiff 60) 60)
       (- 60 (mod (/ tdiff 60) 60))) 60))

(defun hax/org-agenda-format-date (date)
  "Format a DATE string for display in the daily/weekly agenda.
This function makes sure that dates are aligned for easy reading.
Mostly reimplements `org-agenda-format-date-aligned', but also
displays relative (from the current time) hour and minute range."
  (require 'cal-iso)
  (let* ((day (cadr date))
         (month (car date))
         (year (nth 2 date))
         ;; Immediately convert to the `ts` time format, because regular
         ;; emacs time formatting is an absolutely nauseating trash.
         (ts (ts-apply :year year
                       :month month
                       :day day
                       :hour 0
                       :minute 0 (ts-now)))
         (tdiff (ts-difference ts (ts-now))))
    (format
     "%03d%s %s"
     (/ tdiff 3600)
     (hax/closest-unicode-fraction (hax/relative-hour-fraction tdiff))
     (ts-format "%m-%d %a" ts))))

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

(defun org-count-subheadings ()
  "Count the number of direct and recursive subheadings below the current heading. Return cons with `(CHILDREN . DESCENDANTS)'"
  (interactive)
  (let ((descendants 0)
        (children 0)
        (heading-level (1+ (org-outline-level)))
        (end (save-excursion
               (ignore-errors
                 (outline-end-of-subtree)
                 (point)))))
    (when end
      (save-excursion
        (while (and (outline-next-heading)
                    (< (point) end))
          (progn
            (setf descendants (1+ descendants))
            (when (= heading-level (org-outline-level))
              (setf children (1+ children)))))))
    (cons children descendants)))

(defun hax/org-has-subtree () (< 0 (car (org-count-subheadings))))

(defun hax/compare-subtrees (t1 t2)
  (let* ((todo1 (car t1))
         (todo2 (car t2))
         (head1 (cdr t1))
         (head2 (cdr t2))
         (names (hax/org-todo-only-names))
         (res (if (and todo1 todo2) (> (-elem-index todo1 names)
                                       (-elem-index todo2 names))
                (if todo1 t
                  (if todo2 t
                    (string< head1 head2))))))
    ;; (hax/log ">> %s ? %s = %s" t1 t2 res)
    res))

(defun hax/getkey-title ()
  (cons (org-get-todo-state) (org-get-heading)))

(defun hax/sort-subtree-contextually ()
  (interactive)
  (if (hax/org-has-subtree)
      (progn
        (org-sort-entries nil ?F #'hax/getkey-title #'hax/compare-subtrees)
        (save-excursion
          (org-back-to-heading)
          (org-cycle-internal-local)
          (org-cycle-internal-local)))
    (save-excursion
      (outline-up-heading 1)
      (org-sort-entries nil ?f #'hax/getkey-title #'hax/compare-subtrees)
      (org-back-to-heading)
      (org-cycle-internal-local)
      (org-cycle-internal-local))))


(defun hax/org-todo-only-names ()
  (--map (substring it 0 (s-index-of "(" it)) (car org-todo-keywords)))


(defun hax/fill-paragraph (&optional arg)
  (interactive "P")
  (let ((fill-column (if (and arg (numberp (prefix-numeric-value arg)))
                         (prefix-numeric-value arg)
                       999999)))
    (funcall-interactively 'fill-paragraph 'full)))

(defun org-refile-targets-all-files ()
  "Use all currently opened Org buffer files as org-refile targets."
  (mapcar 'buffer-file-name
          (seq-filter ; filter Org buffers
           (lambda (buffer)
             (if-let (file (buffer-file-name buffer))
                 (string-equal (file-name-extension file) "org")))
           (buffer-list))))


(defun org-export-filter-timestamp-remove-brackets (timestamp backend info)
  "removes relevant brackets from a timestamp"
  (cond
   ((org-export-derived-backend-p backend 'latex)
    (replace-regexp-in-string
     (rx "[" (group (* (not "]"))) "]") "\\1"
     (s-replace "textit" "texttt" timestamp)))
   ((org-export-derived-backend-p backend 'html)
    (replace-regexp-in-string "&[lg]t;\\|[][]" "" timestamp))))

(eval-after-load 'ox
  '(add-to-list
    'org-export-filter-timestamp-functions
    'org-export-filter-timestamp-remove-brackets))


(defun org-latex-quote-block (quote-block contents info)
  "Transcode a QUOTE-BLOCK element from Org to LaTeX.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (let ((environment
         (or (org-export-read-attribute :attr_latex quote-block :environment)
             (plist-get info :latex-default-quote-environment)))
        (options
         (or (org-export-read-attribute :attr_latex quote-block :options)
             "")))
    (org-latex--wrap-label
     quote-block (format "\\begin{%s}%s\n%s\\end{%s}"
                         environment
                         options
                         contents
                         environment)
     info)))

(defun hax/org-element-inspect ()
  (interactive)
  (let* ((el (org-element-at-point)))
    (with-current-buffer (get-buffer-create "*element-inspect*")
      (delete-region (point-min) (point-max))
      (insert (format "%s" el)))))


(defun get-capture-target-marker (location)
  "Return a marker to the target location of an org-capture template."
  (save-window-excursion
    (save-excursion
      ;; (org-capture-set-target-location location)
      (pop-to-buffer-same-window (org-capture-get :buffer))
      (goto-char (org-capture-get :pos))
      (point-marker))))

;; (require 'org-capture)
(defun remove-string-properties (text)
  (with-temp-buffer
    (insert text)
    (buffer-substring-no-properties (point-min) (point-max))))

(defun get-headline-from-marker (marker)
  "Return the headline of the org subtree pointed to by MARKER."
  (with-current-buffer (marker-buffer marker)
    (goto-char marker)
    (remove-string-properties (org-get-heading t t t t))))



(load! "lang-org.d/hax-org-general-edit.el")


(defun hax/org-subtree-has-children (&optional invisible)
  ;; Return non-nil if entry at point has child headings.
  ;; Only children are considered, not other descendants.
  ;; Code from `org-cycle-internal-local'.
  (save-excursion
    (let ((level (funcall outline-level)))
      (outline-next-heading)
      (and (org-at-heading-p t)
           (> (funcall outline-level) level)))))

(defun hax/org-sort-entries-recursive-multi (&optional keys)
  "Call `hax/org-sort-entries-recursive'.
If KEYS, call it for each of them; otherwise call interactively
until \\[keyboard-quit] is pressed."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (when (not (org-at-heading-p)) (outline-next-heading))
    (while (org-at-heading-p)
      (hax/dbg/looking-around)
      (when (hax/org-subtree-has-children) (org-sort-entries nil ?o))
      (outline-next-heading))))

(defun hax/org-add-id-to-all-subtrees ()
  "Ensure all subtrees have IDs"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (when (not (org-at-heading-p)) (outline-next-heading))
    (while (org-at-heading-p)
      (org-id-get-create)
      (outline-next-heading))))

;; (custom-set-faces!
;;   '(minimap-font-face :family "BlockFont" :height 30 :group 'minimap))

(defun hax/rice-org ()
  (interactive)
  (custom-theme-set-faces
   'user
   '(variable-pitch ((t (:family "IBM Plex Serif" :height 170 :weight thin))))
   '(fixed-pitch ((t ( :family "Fira Code Nerd Font" :height 130)))))

  (let* ((variable-tuple
          (cond ((x-list-fonts "IBM Plex Serif")  '(:font "IBM Plex Serif"))
                ((x-list-fonts "Source Sans Pro") '(:font "Source Sans Pro"))
                ((x-list-fonts "Lucida Grande")   '(:font "Lucida Grande"))
                ((x-list-fonts "Verdana")         '(:font "Verdana"))
                ((x-family-fonts "Sans Serif")    '(:family "Sans Serif"))
                (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
         (base-font-color     (face-foreground 'default nil 'default))
         (headline           `(:inherit default :weight bold :foreground ,base-font-color)))

    (hax/log "Using %s and %s" headline variable-tuple)
    (custom-theme-set-faces
     'user
     `(org-level-8 ((t (,@headline ,@variable-tuple))))
     `(org-level-7 ((t (,@headline ,@variable-tuple))))
     `(org-level-6 ((t (,@headline ,@variable-tuple))))
     `(org-level-5 ((t (,@headline ,@variable-tuple))))
     `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.1))))
     `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.25))))
     `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.5))))
     `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.75))))
     `(org-document-title ((t (,@headline ,@variable-tuple :height 2.0 :underline nil))))))

  (setq org-startup-indented t
        org-bullets-bullet-list '(" ") ;; no bullets, needs org-bullets package
        org-ellipsis "  " ;; folding symbol
        org-pretty-entities t
        org-hide-emphasis-markers t
        ;; show actually italicized text instead of /italicized text/
        org-agenda-block-separator ""
        org-fontify-whole-heading-line t
        org-fontify-done-headline t
        org-fontify-quote-and-verse-blocks t)
  )

(defun org-export-gfm-to-clipboard ()
  "Export selected text or current subtree in org-mode buffer as GitHub-flavored markdown and copy it to clipboard."
  (interactive)
  (require 'ox-gfm)
  (let ((region-p (region-active-p))
        export-str)
    (if region-p
        (setq export-str (buffer-substring (region-beginning) (region-end)))
      (save-excursion
        (org-back-to-heading t)
        (setq export-str (buffer-substring (point) (org-end-of-subtree t)))))
    (with-temp-buffer
      (insert export-str)
      (let* ((org-mode-hook nil)  ; avoid running org-mode hooks
             (backend (org-export-get-backend 'gfm))
             (org-export-with-toc nil))
        (org-mode)
        (org-export-to-buffer backend "*Org GFM Export*")
        (kill-ring-save (point-min) (point-max))
        (kill-buffer "*Org GFM Export*")))
    (if region-p
        (deactivate-mark))
    (hax/log "Exported to GFM and copied to clipboard.")))

(defun org-get-x-clipboard (value) "")


(when t
  (defun hax/tag-to-state (tags)
    "Convert tags to a state string."
    (mapconcat
     'identity
     (delq nil
           (mapcar
            (lambda (tag)
              (pcase tag
                ("status##pending_clarification" "QUESTION")
                ("status##not_reproducible" "*NO REPRO*")
                ("status##waiting_review" "*ON REVIEW*")
                ("status##need_help" "*NEED HELP*")
                ("status##blocking_dependency" "*DEPENDENCY*")
                ;; Use status block reason
                ("BLOCKED" nil)
                ((or "COMPLETED" "WIP") (concat "*" tag "*"))
                ;; Add more tag-to-state mappings here
                ))
            tags))
     ", "))

  (defun hax/process-subtree ()
    "Process the current subtree and generate a checklist item."
    (let ((title (org-get-heading t t))
          (tags (org-get-tags))
          (todo-keyword (org-get-todo-state)))
      (let* ((state (hax/tag-to-state (append tags (list todo-keyword))))
             (id (org-id-get-create))
             (formatted-title (if (string-match "=\\(\\w+-\\w+\\)=" title)
                                  (match-string 1 title)
                                ""))
             (remainder-title (replace-regexp-in-string "=.*?=" "" title))
             (prefix (concat "- "
                             (if (> (length state) 0) (concat state " "))
                             "[[" id "][" formatted-title "]] "))
             (prefix-len (- 88 (- (length prefix) (+ 2 (length id)))))
             (truncated-title (if (> (length remainder-title) prefix-len)
                                  (substring remainder-title 0 prefix-len)
                                remainder-title)))
        (concat prefix "_" (s-trim truncated-title) "_"))))

  (defun hax/generate-todo-checklist ()
    "Generate a todo checklist from all matching subtrees."
    (interactive)
    (let ((checklist '()))
      (org-map-entries
       (lambda ()
         (when (string-match "=.*?-.*?=" (org-get-heading t t))
           (push (hax/process-subtree) checklist))))
      (substring-no-properties (mapconcat 'identity (nreverse checklist) "\n"))))

  (defun hax/generate-todo-checklist-file (filename)
    "Generate a todo checklist from all matching subtrees in the file specified by FILENAME."
    (with-current-buffer (find-file-noselect filename)
      (hax/generate-todo-checklist)))

  (defun hax/insert-todo-checklist-staging ()
    (interactive)
    (insert (hax/generate-todo-checklist-file hax/staging.org))))

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



