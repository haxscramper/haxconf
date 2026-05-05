;;; -*- lexical-binding: t; -*-

(defun hax/detail/configure-keybinds ()
  ;; https://github.com/hlissner/doom-emacs/blob/develop/docs/faq.org#my-new-keybinds-dont-work
  ;; because I override the default keybindings I had to use this
  ;; abomination of a `map!' call to do what I need.
  (map!
   :map org-mode-map
   [C-S-return] nil
   [C-return] nil
   [M-return] nil
   )

  (map!
   :map evil-org-mode-map
   :ni [C-S-return] nil
   :ni [C-return] nil
   [M-return] nil
   )

  (map!
   :map org-mode-map
   [M-return] #'hax/org-add-trailing-note
   [C-S-return] #'hax/org-insert-todo-entry
   [C-return] #'+org/insert-item-below)

  (map!
   :map evil-org-mode-map
   :ni "M-q" #'hax/fill-paragraph
   )

  (hax/org-download-setup)
  (map!
   :map org-mode-map
   :localleader
   :nv "dt" #'hax/org-subtree-timestamp
   :desc "sort, todo state order"
   :nv "soo" #'hax/sort-subtree-contextually)

  ;; (map!
  ;;  :map evil-org-mode-map
  ;;  :nvi "M-i M-l M-t" nil
  ;;  )

  (map!
   :map evil-org-mode-map
   ;; Consistent multicursor bindings are more important for me, so
   ;; `org-shiftdown' and `org-shiftup' can go elsewhere.
   :ni "C-S-j" nil
   :ni "C-S-k" nil
   :n [S-up] #'hax/org-shiftup
   :n [S-down] #'hax/org-shiftdown

   :ni "C-;" #'flyspell-correct-wrapper
   :nvi [M-return] #'hax/org-add-trailing-note
   :ni [C-S-return] #'hax/org-insert-todo-entry
   :ni [C-return] #'+org/insert-item-below

   :nv ",#" #'hax/org-update-all-cookies
   :nv ",eh" 'org-html-export-to-html
   :desc "Async export to pdf"
   :nv ",ep" (cmd! (org-latex-export-to-pdf t))
   :nv ",ea" 'org-ascii-export-to-ascii
   :nv ",eP" 'org-latex-export-to-pdf
   :nv ",el" 'org-latex-export-to-latex
   :nv ",eo" 'org-odt-export-to-odt
   :nv ",eh" 'org-html-export-to-html
   :nv ",in" #'org-add-note
   :desc "Set tree ID"
   :n ",ti" #'hax/org-edit-id
   :n ",tP" #'org-set-property-and-value
   :n ",tp" #'org-set-property
   :desc "Toggle checkbox"
   :n ",tc" (cmd! (when (org-in-item-p)
                    (save-excursion
                      (goto-char (org-in-item-p))
                      (org-toggle-checkbox))))
   :n ",ta" #'hax/org-assign-tag
   :desc "Insert tag in text"
   :n ",tt" #'hax/org-insert-text-tag
   :desc "Insert timestamp in text"
   :n ",ts" #'hax/org-insert-timestamp
   :n ",tS" #'hax/org-insert-timestamped-parens
   :ni "M-i M-i" #'hax/org-paste-clipboard
   :ni "M-i M-b" (cmd! (hax/org-paste-clipboard nil t))
   :desc "math"
   :ni "M-i M-m" (lambda (text)
                   (interactive "sMath: ")
                   (insert (format "\\(%s\\)" text)))
   :desc "math"
   :v "M-i M-m" (cmd! (insert (format "\\(%s\\)" (pop-selection))))
   :desc "insert ~code~"
   :ni "M-i M-`" (cmd! (insert (format "~%s~" (read-string "Code: "))))
   :desc "insert src_code{}"
   :ni "M-i M-c M-c" (cmd! (hax/insert-created-timestamp))
   :desc "insert src_code{}"
   :ni "M-i M-c M-s" (cmd!
                      (insert
                       (format
                        "src_%s{%s}"
                        (ivy-read "Language: "
                                  '("asm" "cpp" "sh")
                                  :history 'hax/insert-stc-code-history)
                        (read-string "Code: "))))
   :desc "separator"
   :ni "M-i M-s" (cmd! (insert
                        (format "%s\n" (s-repeat fill-column "-"))))
   :desc "- [timestamp]"
   :ni [M-S-return] (cmd! (org-insert-item)
                          (insert (hax/org-current-timestamp))
                          (insert " "))

   :desc "timestamped element timestamp"
   :ni "M-i M-e M-t" (cmd! (insert (hax/org-current-timestamp)))
   :ni "M-i M-S-i" (cmd! (insert "#+capion: ")
                         (save-excursion (hax/org-paste-clipboard)))

   :desc "link to word"
   :v "M-i M-l M-w" (cmd! (let ((text (get-selected-region-text)))
                            (delete-region (get-selected-region-start)
                                           (get-selected-region-end))
                            (insert (format "[[%s][%s]]" text text))))

   :desc "link to subtree, refs"
   :nvi "M-i M-l M-t M-r" #'hax/add-subtree-refs

   :desc "link to subtree, full name"
   :v "M-i M-l M-t M-f" (cmd! (let ((text (get-selected-region-text)))
                                (delete-region (get-selected-region-start)
                                               (get-selected-region-end))
                                (hax/org-insert-clipboard-link text)))
   :desc "link to clipboard"
   :v "M-i M-l M-l" (cmd! (let ((text (get-selected-region-text)))
                            (delete-region (get-selected-region-start)
                                           (get-selected-region-end))
                            (hax/org-insert-clipboard-link text)))

   :desc "Insert subtree"
   :vi "M-i M-t" (cmd!
                  (insert "\n* TODO ")
                  (save-excursion
                    (insert (format "
  :PROPERTIES:
  :CREATED:  [%s]
  :END:
" (format-time-string "%Y-%m-%d %a %H:%M:%S %Z" (current-time))))))

   :desc "link subtree, full name"
   :ni "M-i M-l M-t M-f" (cmd! (hax/org-insert-link-to-subtree))
   :desc "link subtree, short name"
   :ni "M-i M-l M-t M-s" (cmd! (hax/org-insert-link-to-subtree :last-n 1))
   :desc "link subtree, short name"
   :v "M-i M-l M-t M-s" (cmd! (let ((text (get-selected-region-text)))
                                (delete-region (get-selected-region-start)
                                               (get-selected-region-end))
                                (hax/org-insert-link-to-subtree
                                 :description text
                                 :last-n 1)))
   :desc "link subtree, manual description"
   :ni "M-i M-l M-t M-d" (lambda (description)
                           (interactive "sLink description: ")
                           (hax/org-insert-link-to-subtree :description description))

   :desc "subtree, only short name"
   :ni "M-i M-l M-t M-n" (cmd! (hax/org-select-subtree-callback
                                "Insert: "
                                (lambda
                                  (x) (insert (hax/org-outline-path-at-marker
                                               (cdr x) 1)))
                                'hax/org-insert-link-to-heading))

   :desc "link active subtree, full name"
   :ni "M-i M-l M-a" (cmd! (hax/org-insert-link-to-subtree
                            :entries (hax/org-collect-active-entries)))

   :desc "Insert link"
   :ni "S-C-i" (cmd! (hydra-insert-link/body))
   :ni "M-i M-l M-l" #'hax/org-insert-clipboard-link
   :ni "M-i M-l M-d" (lambda (description)
                       (interactive "sLink description: ")
                       (hax/org-insert-clipboard-link description))

   :ni "M-i M-l M-f" (cmd! (hax/org-insert-footnote-link (hax/org-gen-footnote-name)))
   :desc "Inline footnote"
   :ni "M-i M-f M-i" (cmd! (insert "[fn::")
                           (save-excursion (insert "]")))
   :desc "footnote & prompt"
   :ni "M-i M-f M-p" (cmd! (save-excursion
                             (hax/org-insert-footnote (hax/org-gen-footnote-name))
                             (insert (read-string "Footnote: "))
                             (org-fill-paragraph)))
   :desc "footnote & goto"
   :ni "M-i M-f M-f" (cmd! (hax/org-insert-footnote (hax/org-gen-footnote-name)))
   :desc "insert {{{macro}}}"
   :ni "M-i M-{" (cmd! (insert "{{{") (save-excursion (insert "}}}")))
   :desc "insert #+begin_src"
   :n ",ic" (cmd!
             (evil-insert-state)
             (yas-expand-snippet "#+begin_src $1\n$0\n#+end_src"))
   :nv ",hi" #'org-indent-mode
   :nv ",ci" #'org-clock-in
   :nv ",co" #'org-clock-out
   :desc "start WIP clocking"
   :nv ",cw" (cmd! (hax/ensure-todo "WIP") (org-clock-in))
   )

  (map!
   :leader
   :n "oai" #'hax/org-clock-in-interactively
   :desc "Clock in repeated tasks"
   :n "oari" (cmd! (hax/org-clock-in-interactively 'repeated))
   :desc "Complete repeated task"
   :n "oarc" (cmd! (hax/org-complete-interactively "COMPLETED" 'repeated))

   :n "oao" #'org-clock-out
   :desc "Complete current clock"
   :n "oac" (cmd! (save-excursion
                    (org-clock-goto)
                    (org-clock-out)
                    (hax/ensure-todo "COMPLETED")))
   :n "oag" #'hax/org-goto-select-active-subtree
   :n "oaC" #'hax/org-complete-interactively)

  (map!
   :n ",nt" (cmd! (org-capture nil "t"))
   :n ",nd" (cmd! (org-capture nil "d"))
   ;; Quick access to common operations. No specific meaning behind the key
   ;; specification.
   :desc "Global agenda"
   :n [M-f7] (cmd! (org-agenda nil "*"))
   :desc "New note"
   :n [M-f8] (cmd! (org-capture nil "d"))
   :desc "Item under clocked"
   :n [M-f9] (cmd! (org-capture nil "c"))
   :desc "New immediate todo"
   :n [M-f10] (cmd! (org-capture nil "i"))
   :desc "New staging item"
   :n [M-f11] (cmd! (org-capture nil "S"))
   :desc "New capture"
   :n [M-insert] #'org-capture)
  )


(defun hax/agenda-mode-hook ()
  (interactive)
  (setq org-agenda-max-title-length (- (window-width) 10))
  ;; I want to be able to use fX __**EVERYWHERE**__.
  (local-set-key (kbd "<f1>") 'winum-select-window-1)
  (local-set-key (kbd "<f2>") 'winum-select-window-2)
  (local-set-key (kbd "<f3>") 'winum-select-window-3)
  (local-set-key (kbd "<f4>") 'winum-select-window-4)
  (local-set-key (kbd "<f5>") 'winum-select-window-5)
  (local-set-key (kbd "<f6>") 'winum-select-window-6)
  (local-set-key (kbd "<M-f7>") 'org-agenda-quit))


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
