;;; -*- lexical-binding: t; -*-

(defun json-parse-file (file)
  (json-parse-string (f-read file)))




(s-replace-regexp
 (rx (group (1+ digit)) " "
     (group rx-month-name-or-digit) " "
     (group (1+ digit)))
 "\\3-\\2-\\1"
 "27 March 2020"
 )

(require 'dbus)

(defun my-dbus-method-handler (logical physical quote)
  (insert (format "- %s (%s) /\"%s\"/" logical physical quote))
  '(:boolean t))

(dbus-register-method
 :session "org.freedesktop.TextEditor" "/org/freedesktop/TextEditor"
 "org.freedesktop.TextEditor" "OpenFile"
 #'my-dbus-method-handler)
