;;; -*- lexical-binding: t; -*-


(defun hax/clamp (value min max)
  (cond
   ((and min max (<= min value max)) value)
   ((and min (< value min)) min)
   ((and max (< max value)) max)
   (t value)))

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

(defun write-to-file-unquoted (file data)
  "Write DATA to FILE"
  (with-temp-file file
    (princ data (current-buffer))))
