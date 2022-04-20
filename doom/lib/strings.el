;;; -*- lexical-binding: t; -*-

(defun binary-number-to-string (i)
  "convert an integer into it's binary representation in string format"
  (let ((res ""))
    (while (not (= i 0))
      (setq res (concat (if (= 1 (logand i 1)) "1" "0") res))
      (setq i (lsh i -1)))
    (when (string= res "") (setq res "0"))
    res))

(defun matches-in-buffer (regexp match-cb &optional buffer)
  "return a list of matches of REGEXP in BUFFER or the current buffer if not given."
  (let ((matches))
    (save-match-data
      (save-excursion
        (with-current-buffer (or buffer (current-buffer))
          (save-restriction
            (widen)
            (goto-char 1)
            (while (search-forward-regexp regexp nil t 1)
              (push (funcall match-cb) matches)))))
      matches)))

(defun s-delete-lines (text first &optional last)
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (forward-line first)
    (let ((start (point)))
      (goto-char (point-max))
      (when last
        (forward-line (- last)))
      (buffer-substring start (point)))))

;; (s-delete-lines "haxser\n123\n123" 1)
