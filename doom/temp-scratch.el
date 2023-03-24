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

(defun hax/dbus-register-methods (method-list)
  (dolist (method method-list)
    (dbus-register-method
     :session "hax.haxconf.Emacs" "/hax/haxconf/Emacs"
     "hax.haxconf.Emacs"
     (car method)
     (cdr method))))



(hax/dbus-register-methods
 (list
  (cons "InsertPdfQuoteBlock2"
        (lambda (logical physical quote)
          (insert (format "#+caption: %s (%s)\n#+begin_quote\n%s\n#+end_quote\n"
                          logical physical quote))))
  (cons "InsertPdfQuoteItem2"
        (lambda (logical physical quote)
          (insert (format "- %s (%s) /\"%s\"/"
                          logical physical quote))))
  (cons "PrintTest" (lambda (input) (message "%s" input)))))


(set-face-attribute 'winum-face nil :weight 'bold :foreground "green")
