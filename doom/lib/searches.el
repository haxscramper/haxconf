;;; -*- lexical-binding: t; -*-

(defun line-length (n)
  "Length of the Nth line."
  (save-excursion
    (goto-char (point-min))
    (if (zerop (forward-line (- n 1)))
        (current-line-length))))

(defun current-line-length ()
  "Length of current line"
  (- (line-end-position) (line-beginning-position)))


(defun exhaustive-search-char-vertically (direction &optional search-limit)
  "Exchaustively search for character above current position of
  the cursor. Return position of the character of found,
  otherwiser return nil. Latter case should only be expected if
  search hits beginning/end of the buffer. direction can either
  be 1, -1 or nil. Nil defaults to 1 (search below). Will be
  performed `search-limit` attempts before stopping. If value is
  negative or nil do not stop"
  (if (equal direction nil) (setq direction 1))
  (save-excursion
    (let ((increment (* direction 1))
          (relative direction))
      (loop
       (let ((char-pos (point-has-character-next-vertically relative)))
         (cond
          ( ; Found matching character
           (not (equal char-pos nil))
           (return char-pos))
          ( ; Hit search limit
           (and (>= (/ relative increment) search-limit)
                (not (equal search-limit nil)))
           (return nil))
          ( ; Increment and continue search
           t
           (setq relative (+ relative increment)))))))))

(defun point-has-character-next-vertically (num)
  "If there is a character `num' lines below (above if num is
  negative) from the position of the point return
  position of the character. Otherwise return nil"
  (save-excursion
    (let ((line-now (what-line))
          (column-now (current-column))
          (point-now (point)))
      (forward-line num)
      (if (>= (current-line-length)
              column-now)
          (progn
            (beginning-of-line)
            (+ (point) column-now))
        nil))))
