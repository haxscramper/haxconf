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

(map!
 :n  "c"  #'evil-change-without-register
 :n  "C"  #'evil-change-line-without-register
 :n  "p"  #'evil-paste-after-without-register
 :n  "P"  #'evil-paste-before-without-register
 :n  "x"  #'evil-delete-char-without-register
 :n  "X"  #'evil-delete-backward-char-without-register
 :n  "d"  #'evil-delete-without-register)
