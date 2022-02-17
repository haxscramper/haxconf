;;; -*- lexical-binding: t; -*-


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
  (insert "\n\n" (make-string (or (org-current-level) 1) ?*) " TODO ")
  (org-expiry-insert-created)
  (evil-insert-state)
  (hax/org-update-all-cookies))

(defun hax/org-mode-hook ()
  (interactive)
  (abbrev-mode 1)
  (org-indent-mode -1)
  ;; https://github.com/hlissner/doom-emacs/blob/develop/docs/faq.org#my-new-keybinds-dont-work
  ;; because I override the default keybindings I had to use this
  ;; abomination of a `map!' call to do what I need.
  (map! :map org-mode-map [C-return] nil)
  (map! :map evil-org-mode-map :ni [C-return] nil)
  (map! :map org-mode-map [C-return] #'hax/org-insert-todo-entry)
  (map! :map evil-org-mode-map :ni [C-return] #'hax/org-insert-todo-entry)
  (map!
   :map org-mode-map
   :nv ",#" #'hax/org-update-all-cookies
   :nv ",hi" #'org-indent-mode))

(after! org
  (require 'org-expiry)
  (define-abbrev-table 'org-mode-abbrev-table
    '(("rst" "RST")
      ("i" "I")))
  (add-hook! org-mode-hook 'hax/org-mode-hook)
  (setq
   org-directory "~/defaultdirs/notes/personal"
   org-agenda-files (f-glob "todo/*.org" org-directory)
   org-log-reschedule 'note
   org-log-states-order-reversed nil
   org-log-redeadline 'note
   org-startup-indented nil
   calendar-week-start-day 1
   org-hierarchical-todo-statistics t
   org-image-actual-width (list 300)
   org-startup-with-inline-images nil
   org-adapt-indentation nil
   org-log-into-drawer t)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((C . t)
     (nim . t)
     (shell . t)
     (python . t)))
  (setq org-todo-keywords
        '((sequence
           "TODO(t!/!)"
           "LATER(l!)"
           "NEXT(n!)"
           "POSTPONED(P!/!)"
           "WIP(w!)"
           "STALLED(s!)"
           "REVIEW(r!/!)"
           "TIMEOUT(T)"
           "FAILED(f@/!)"
           "CANCELED(C@/!)"
           "|"
           "DONE(d!/@)"
           "COMPLETED(c!/@)"
           "NUKED(N@/!)"
           "PARTIALLY(p@/!)"
           "FUCKING___DONE(F)")))
  (setq org-todo-keyword-faces
        '(("TODO" . "orange")
          ("LATER" . "DeepPink")
          ("NEXT" . "DarkRed")
          ("POSTPONED" . "coral")
          ("DONE" . "green")
          ("NUKED" . "purple")
          ("CANCELED" . "snow")
          ("PARTIALLY" . "goldenrod")
          ("WIP" . "tan")
          ("STALLED" . "gold")
          ("REVIEW" . "SteelBlue")
          ("FAILED" . "red")
          ("FUCKING___DONE" . "gold1")
          ("TIMEOUT" . "red")))
  (setq org-global-properties
        '(("Effort_ALL" .
           "0:10 0:30 1:00 2:00 4:00 1d 2d 3d 1w 2w 1m 2m 3m 5m 1y 100y")))
  )
