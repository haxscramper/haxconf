;;; -*- lexical-binding: t; -*-

;; Indentation-based operations - moving code blocks around, inserting
;; newlines, getting active indentation.

(defun code-indent-shift-left (start end &optional count)
  "Shift lines contained in region START END by COUNT columns to the left.
COUNT defaults to `1'.  If region isn't
active, the current line is shifted.  The shifted region includes
the lines in which START and END lie.  An error is signaled if
any lines in the region are indented less than COUNT columns."
  (interactive (get-selected-region))
  (if count
      (setq count (prefix-numeric-value count))
    (setq count 1))
  (when (> count 0)
    (let ((deactivate-mark nil))
      (save-excursion
        (goto-char start)
        (while (< (point) end)
          (if (and (< (current-indentation) count)
                   (not (looking-at "[ \t]*$")))
              (user-error "Can't shift all lines enough"))
          (forward-line))
        (indent-rigidly start end (- count))))))

(defun code-indent-shift-right (start end &optional count)
  "Shift lines contained in region START END by COUNT columns to the right.
COUNT defaults to `1'.  If region isn't
active, the current line is shifted.  The shifted region includes
the lines in which START and END lie."
  (interactive (get-selected-region))
  (let ((deactivate-mark nil))
    (setq count (if count (prefix-numeric-value count) 1))
    (indent-rigidly start end count)))
