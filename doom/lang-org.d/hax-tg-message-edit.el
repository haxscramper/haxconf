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
