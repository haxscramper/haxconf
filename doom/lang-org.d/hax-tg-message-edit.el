;;; -*- lexical-binding: t; -*-

(when t
  (defun fixup-buffer-dates ()
    (interactive)
    (save-excursion
      (let* ((point-now (point))
             (start (buffer-substring (point-min) (point-max)))
             (result (fixup-text-dates start)))
        (delete-region (point-min) (point-max))
        (insert result)
        (goto-char point-now))))

  (rx-define rx-month-name
    (| "January" "February" "March" "April" "May"
       "June" "July" "August" "September" "October" "November" "December"
       "Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul"
       "Aug" "Sep" "Oct" "Nov" "Dec"))

  (rx-define rx-month-digit
    (| "01" "02" "03" "04" "05" "06" "07" "08" "09"
       "1" "2" "3" "4" "5" "6" "7" "8" "9" "10" "11" "12"))

  (rx-define rx-month-name-or-digit (| rx-month-name rx-month-digit))
  (rx-define rx-day-digit (1+ digit))
  (rx-define rx-year (1+ digit))
  (rx-define rx-space (| " " " "))

  (defun fixup-text-dates (str)
    "Automatically convert string timestamps from some unreadable and
  unparseable formats such as `month-day-year H:M PM/AM' into a
  sane 8601 version."
    ;; The implementation runs in two parts. First I try to get rid of the
    ;; AM/PM nonsense by replacing all the content into the "almost correct"
    ;; version and then I map things to the proper ISO8601 format.

    (setq
     replacement-pairs
     (list
      (cons (rx "["
                (group rx-day-digit) rx-space
                (group rx-month-name) rx-space
                (group rx-year)
                "]")
            "[\\3-\\2-\\1]")
      (cons (rx "["
                (group (1+ digit)) ":"
                (group (1+ digit)) rx-space
                (group (| "AM" "PM"))
                "]") "[\\1\\3:\\2]")
      (cons (rx "["
                ;; Month Day, Year Hour:Minute AM|PM
                (group rx-month-name-or-digit) rx-space
                (group rx-day-digit) ", "
                (group (1+ digit))
                "]") "[20\\3-\\1-\\2]")
      (cons (rx "["
                ;; Month Day, Year Hour:Minute AM|PM
                (group rx-month-name-or-digit) rx-space
                (group (1+ digit)) ", "
                (group (1+ digit)) rx-space
                (group (1+ digit)) ":"
                (group (1+ digit)) rx-space
                (group (| "AM" "PM"))
                "]") "[20\\3-\\1-\\2 \\4\\6:\\5]")
      (cons (rx "["
                ;; Month/Day/Year Hour:Minute AM|PM
                (group rx-month-name-or-digit) "/"
                (group (1+ digit)) "/"
                (group (1+ digit))
                (? " "
                   (group (1+ digit)) ":"
                   (group (1+ digit)) rx-space
                   (group (| "AM" "PM")))
                "]") "[20\\3-\\1-\\2 \\4\\6:\\5]")
      (cons (rx "2020" (group digit digit)) "20\\1")
      (cons (rx "20" (group digit digit digit digit)) "\\1")
      (cons (rx "-" (group digit) (group (| rx-space "]"))) "-0\\1\\2")
      (cons (rx " :]") "]")
      (cons (rx "20" (group digit digit) "-" (group digit) "-" (group digit digit)) "20\\1-0\\2-\\3")))

    (dolist (pair replacement-pairs)
      (setq str (s-replace-regexp (car pair) (cdr pair) str)))

    (setq
     str
     (s-replace-all
      '(
        ("Jan" . "01")
        ("Feb" . "02")
        ("Mar" . "03")
        ("Apr" . "04")
        ("May" . "05")
        ("Jun" . "06")
        ("Jul" . "07")
        ("Aug" . "08")
        ("Sep" . "09")
        ("Oct" . "10")
        ("Nov" . "11")
        ("Dec" . "12")
        ("January" . "01")
        ("February" . "02")
        ("March" . "03")
        ("April" . "04")
        ("May" . "05")
        ("June" . "06")
        ("July" . "07")
        ("August" . "08")
        ("September" . "09")
        ("October" . "10")
        ("November" . "11")
        ("December" . "12"))
      str))

    (s-replace-all
     '(("10PM" . "22") ("11PM" . "23") ("12PM" . "12")
       ("10AM" . "01") ("11AM" . "11") ("12AM" . "00")
       ("1PM" . "13") ("2PM" . "14") ("3PM" . "15")
       ("4PM" . "16") ("5PM" . "17") ("6PM" . "18")
       ("7PM" . "19") ("8PM" . "20") ("9PM" . "21")
       ("1AM" . "01") ("2AM" . "02") ("3AM" . "03")
       ("4AM" . "04") ("5AM" . "05") ("6AM" . "06")
       ("7AM" . "07") ("8AM" . "08") ("9AM" . "09")) str))
  ;; (fixup-text-dates "[Feb 15, 2011] [2 March 2020]")
  )

(progn
  (cl-defun org-note-insert-at-time+date
      (text second minute hour day month year &optional (tags '()))

    (let ((org-overriding-default-time
           (encode-time second minute hour day month year)))
      (setq hax/immediate-note-tags tags)
      (setq hax/immediate-note-content text)
      (org-capture nil "@")
      (setq hax/immediate-note-content "")
      (setq hax/immediate-note-tags '())))

  (defun hax/tg-extract-date (msg)
    (with-temp-buffer
      (insert (fixup-text-dates msg))
      (goto-char 0)
      (re-search-forward (rx "["
                             (group (1+ digit)) "-" ;; 1 year
                             (group (1+ digit)) "-" ;; 2 month
                             (group (1+ digit)) " " ;; 3 day
                             (group (1+ digit)) ":" ;; 4 hour
                             (group (1+ digit)) ;; 5 minute
                             "]"))
      (list
       0 ;; sec
       (string-to-number (match-string 5)) ;; min
       (string-to-number (match-string 4)) ;; hour
       (string-to-number (match-string 3)) ;; day
       (string-to-number (match-string 2)) ;; mon
       (string-to-number (match-string 1)) ;; year
       )))

  (defun hax/tg-extract-tags (msg)
    (with-temp-buffer
      (insert msg)
      (goto-char 0)
      (-distinct (matches-in-buffer
                  (rx "#" (group (1+ (| word "_" "#"))))
                  (lambda () (match-string 1))))))
  (defun hax/tg-insert-note (msg)
    (interactive)
    (pcase (hax/tg-extract-date msg)
      (`(,sec ,min ,hour ,day ,month ,year)
       (hax/log
        "Inserted note with sec:%s min:%s hour:%s day:%s month:%s year:%s"
        sec min hour day month year)
       ;; (calendar-gregorian-from-absolute
       ;;  (time-to-days (encode-time sec min hour day month year))
       ;;  )
       (org-note-insert-at-time+date
        (s-trim msg) sec min hour day month year
        (append '("from_tg") (hax/tg-extract-tags msg)))
       )))

  (defun hax/tg-insert-selected-note (beginning end)
    (interactive "r")
    (hax/tg-insert-note (s-replace "𝖍𝖆𝖝𝖘𝖈𝖗𝖆𝖒𝖕𝖊𝖗"
                                   "haxscramper"
                                   (buffer-substring beginning end)))
    (kill-region beginning end)
    (hax/log (propertize "inserted note" 'face
                         `(:foreground ,(doom-color 'red))))))
