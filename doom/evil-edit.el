;; Sane (IMO of course) configurations for the evil editing operations
;;
;; - d **DELETES** the line instead of clobbering your kill ring

(evil-define-operator evil-delete-char-without-register (beg end type reg)
  "delete character without"
  :motion evil-forward-char
  (interactive "<R><y>")
  (delete-region beg end))

(evil-define-operator evil-delete-backward-char-without-register (beg end type _)
  "delete backward character without yanking"
  :motion evil-backward-char
  (interactive "<R><y>")
  (delete-region beg end))

(evil-define-operator evil-delete-without-register (beg end type _ _2)
  (interactive "<R><y>")
  (delete-region beg end))

(evil-define-operator evil-delete-without-register-if-whitespace (beg end type reg yank-handler)
  (interactive "<R><y>")
  (delete-region beg end))

(evil-define-operator evil-delete-line-without-register (beg end type _ yank-handler)
    (interactive "<R><y>")
    (delete-region beg end))

;; TODO Alow push deleted text to delete stack
(evil-define-operator evil-change-without-register (beg end type _ yank-handler)
  (interactive "<R><y>")
  (evil-change beg end type ?_ yank-handler))

(evil-define-operator evil-change-line-without-register (beg end type _ yank-handler)
  "Change to end of line without yanking."
  :motion evil-end-of-line
  (interactive "<R><y>")
  (evil-change beg end type ?_ yank-handler #'evil-delete-line))

(evil-define-command evil-paste-after-without-register
  (count &optional register yank-handler)
  "evil paste before without yanking"
  :suppress-operator t
  (interactive "P<x>")
  (if (evil-visual-state-p)
      (evil-visual-paste-without-register count register)
      (evil-paste-after count register yank-handler)))

(evil-define-command evil-paste-before-without-register (count &optional register yank-handler)
  "evil paste before without yanking"
  :suppress-operator t
  (interactive "P<x>")
  (if (evil-visual-state-p)
      (evil-visual-paste-without-register count register)
      (evil-paste-before count register yank-handler)))

(evil-define-command evil-visual-paste-without-register (count &optional register)
  "Paste over Visual selection."
  :suppress-operator t
  (interactive "P<x>")
  ;; evil-visual-paste is typically called from evil-paste-before or
  ;; evil-paste-after, but we have to mark that the paste was from
  ;; visual state
  (setq this-command 'evil-visual-paste)
  (let* ((text (if register
                   (evil-get-register register)
                 (current-kill 0)))
         (yank-handler (car-safe (get-text-property
                                  0 'yank-handler text)))
         new-kill
         paste-eob)
    (evil-with-undo
      (let* ((kill-ring (list (current-kill 0)))
             (kill-ring-yank-pointer kill-ring))
        (when (evil-visual-state-p)
          (evil-visual-rotate 'upper-left)
          ;; if we replace the last buffer line that does not end in a
          ;; newline, we use `evil-paste-after' because `evil-delete'
          ;; will move point to the line above
          (when (and (= evil-visual-end (point-max))
                     (/= (char-before (point-max)) ?\n))
            (setq paste-eob t))
          (evil-delete-without-register evil-visual-beginning evil-visual-end
                       (evil-visual-type))
          (when (and (eq yank-handler #'evil-yank-line-handler)
                     (not (eq (evil-visual-type) 'line))
                     (not (= evil-visual-end (point-max))))
            (insert "\n"))
          (evil-normal-state)
          (setq new-kill (current-kill 0))
          (current-kill 1))
        (if paste-eob
            (evil-paste-after count register)
          (evil-paste-before count register)))
      (kill-new new-kill)
      ;; mark the last paste as visual-paste
      (setq evil-last-paste
            (list (nth 0 evil-last-paste)
                  (nth 1 evil-last-paste)
                  (nth 2 evil-last-paste)
                  (nth 3 evil-last-paste)
                  (nth 4 evil-last-paste)
                  t)))))

(defun backward-delete-word-no-push (arg)
  "Delete characters backward until encountering the beginning of a word.
With argument, do this that many times.
This command does not push text to `kill-ring'."
  (interactive "p")
  (delete-region (point)
                 (progn (forward-word -1)
                        (point))))


(defun forward-delete-word-no-push (arg)
  "Delete characters backward until encountering the beginning of a word.
With argument, do this that many times.
This command does not push text to `kill-ring'."
  (interactive "p")
  (delete-region (point)
                 (progn (forward-word)
                        (point))))

;;;#== Range deletion
(defun delete-range-to-symbol-including-no-push (arg)
  "Delete all symbols between current point position and first
occurrence of symbol"
  (interactive "P")
  (if  (equal arg nil)
      (setq arg 1))
  (let ((range-start (point))
        (range-end   (evil-find-char)))
    (unless (equal range-end range-start)
      (progn
        (internal/push-text-to-delete-ring
         (buffer-substring range-start range-end))))))


(map!
 :n  "c"  #'evil-change-without-register
 :n  "C"  #'evil-change-line-without-register
 :n  "p"  #'evil-paste-after-without-register
 :n  "P"  #'evil-paste-before-without-register
 :n  "x"  #'evil-delete-char-without-register
 :n  "X"  #'evil-delete-backward-char-without-register
 :n  "d"  #'evil-delete-without-register
 ;; Cut cut things
 :v "x" 'kill-region
 ;; Delete deletes things
 :v "d" 'delete-region
 ;; Backspace should **DELETE** text - why in world someone could even
 ;; thing this is not how it should functino.
 :nvi "<C-backspace>" 'backward-delete-word-no-push
 )



;; Additional functionality for the multicursor editing

(defun hax/mc-any-cursors-at-pos? (pos)
  "Check if ther is any cursors located at position `pos'"
  (some
   (lambda (cursor) (equal pos (evil-mc-get-cursor-start cursor)))
   evil-mc-cursor-list))

(defun hax/mc-ensure-cursor-here ()
  "If there is a cursor at `pos' do nothing, otherwise create new
  evil-mc-cursor"
  (unless (hax/mc-any-cursors-at-pos? (point)) (evil-mc-make-cursor-here)))

(defun hax/mc-ensure-single-cursor-here ()
  "Ensure that there is either mc-cursor or real one at given
  position, and not two at the same time. If there is two
  cursors, remove evil-mc one"
  (if (hax/mc-any-cursors-at-pos? (point)) (evil-mc-undo-cursor-at-pos (point))))


;; TODO Add version that not only ignores lines that don't have enough
;; characters but also lines that have whitespace character

(defun hax/mc-make-cursors-precise-move-line (num &optional dir)
  "Create evil-mc cursor at current position and move precisely
above. If line does not contain enough characters search for
matching line until found. If called with `num' equal to nil
search for line until found and place cursor there. If num is not
nil perform exactly `num' attempts to create cursor. dir can
either be 1, -1 or nil. Nil defaults to 1 (below)"
  (interactive "P")
  (if (equal dir nil) (setq dir 1))
  (if (equal num nil)
      (progn
        (let ((above-pos (exhaustive-search-char-vertically dir 100)))
          (if (equal above-pos nil) (message "Cannot create cursor above")
            (progn
              (hax/mc-ensure-cursor-here)
              (goto-char above-pos)
              (hax/mc-ensure-single-cursor-here)))))
    (progn
      (dotimes (i (+ num 1))
        (let ((above-pos (point-has-character-next-vertically (* i dir))))
          (unless (equal above-pos nil)
            (progn
              (save-excursion (goto-char above-pos) (hax/mc-ensure-cursor-here))
              (hax/mc-ensure-single-cursor-here))))))))

(defun hax/mc-make-cursor-above-move-next-line (count)
  "Make cursor precisely above current position of the cursor."
  (interactive "P")
  (hax/mc-make-cursors-precise-move-line count -1))

(defun hax/mc-make-cursor-below-move-prev-line (count)
  (interactive "P")
  (hax/mc-make-cursors-precise-move-line count 1))

(after! evil-mc
  (dolist
    ;; Configure multicursor editor to use proper keybindings. Taken from
    ;; https://github.com/gilbertw1/bmacs/blob/master/bmacs.org#evil-mc
    (commands '(
      (evil-change-without-register
       . ((:default . evil-mc-execute-default-evil-change)))
      (evil-change-line-without-register
       . ((:default . evil-mc-execute-default-evil-change-line)))
      (evil-delete-without-register
       . ((:default . evil-mc-execute-default-evil-delete)))
      (evil-delete-without-register-if-whitespace
       . ((:default . evil-mc-execute-default-evil-delete)))
      (evil-delete-char-without-register
       . ((:default . evil-mc-execute-default-evil-delete)))
      (evil-delete-backward-char-without-register
       . ((:default . evil-mc-execute-default-evil-delete)))
      (evil-delete-line-without-register
       . ((:default . evil-mc-execute-default-evil-delete)))
      (evil-paste-after-without-register
       . ((:default . evil-mc-execute-default-evil-paste)))
      (evil-paste-before-without-register
       . ((:default . evil-mc-execute-default-evil-paste)))))
    (push commands evil-mc-custom-known-commands)))

(map!
 :n "C-S-k" 'hax/mc-make-cursor-above-move-next-line
 :n "C-S-j" 'hax/mc-make-cursor-below-move-prev-line)
