;;; -*- lexical-binding: t; -*-

;; Functions for worktin with active selections - getting their boundaries,
;; splicing them into other `(interactive)' functions with start end
;; arguments


(defun get-selected-region-start ()
  "Get start of the selected region if any, otherwise return line start position"
  (if mark-active (region-beginning) (line-beginning-position)))

(defun get-selected-region-end ()
  "Get end of the selected region if any, otherwise return line end position"
  (if mark-active (region-end) (line-end-position)))

(defun get-selected-region ()
  "Return (start, end) list depending on the code selection
state. This function can be used in `(interactive)' checks for
functions that accept arguments with code region"
  (list (get-selected-region-start) (get-selected-region-end) current-prefix-arg))
