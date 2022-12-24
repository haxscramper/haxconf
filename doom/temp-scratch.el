;;; -*- lexical-binding: t; -*-

(defun json-parse-file (file)
  (json-parse-string (f-read file)))

