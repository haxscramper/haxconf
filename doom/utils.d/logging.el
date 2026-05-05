;;; -*- lexical-binding: t; -*-

(require 'macroexp)

(defvar hax/log-file "/tmp/hax-emacs.log")

(defvar hax/log-file--initialized nil)

(defun hax/log-file--initialize ()
  (unless hax/log-file--initialized
    (with-temp-file hax/log-file)
    (setq hax/log-file--initialized t)))

(defun hax/log-file--cli-handler (option)
  (setq hax/log-file (pop command-line-args-left)))

(add-to-list 'command-switch-alist '("--hax/log-file" . hax/log-file--cli-handler))

(defun hax/log--format-location (file line function)
  (format "%s:%s%s"
          (or file "<unknown>")
          (or line "?")
          (if function
              (format " %s" function)
            "")))

(defun hax/log--write-string (text file append)
  (let ((coding-system-for-write 'utf-8))
    (write-region text nil file append 'silent)))

(defun hax/log--emit (text print-stdout print-stderr append-to redirect-and-overwrite)
  (hax/log-file--initialize)
  (cond
   (redirect-and-overwrite
    (hax/log--write-string text redirect-and-overwrite nil))
   (append-to
    (hax/log--write-string text append-to t))
   (t
    (hax/log--write-string text hax/log-file t)))
  (when print-stdout
    (let ((coding-system-for-write 'utf-8))
      (write-region text nil "/dev/stdout" t 'silent)))
  (when print-stderr
    (let ((coding-system-for-write 'utf-8))
      (write-region text nil "/dev/stderr" t 'silent))))

(defun hax/log--parse-options (args)
  (let ((message nil)
        (print-stdout nil)
        (print-stderr nil)
        (append-to nil)
        (redirect-and-overwrite nil))
    (while args
      (let ((arg (pop args)))
        (if (keywordp arg)
            (pcase arg
              (:print-stdout
               (setq print-stdout t))
              (:print-stderr
               (setq print-stderr t))
              (:append-to
               (setq append-to (pop args)))
              (:redirect-and-overwrite
               (setq redirect-and-overwrite (pop args)))
              (_
               (error "Unknown hax/log option: %S" arg)))
          (if message
              (error "Multiple message arguments passed to hax/log")
            (setq message arg)))))
    (unless message
      (error "hax/log requires a message argument"))
    (list :message message
          :print-stdout print-stdout
          :print-stderr print-stderr
          :append-to append-to
          :redirect-and-overwrite redirect-and-overwrite)))

(defun hax/log--call (file line function &rest args)
  (let* ((parsed (hax/log--parse-options args))
         (message (plist-get parsed :message))
         (print-stdout (plist-get parsed :print-stdout))
         (print-stderr (plist-get parsed :print-stderr))
         (append-to (plist-get parsed :append-to))
         (redirect-and-overwrite (plist-get parsed :redirect-and-overwrite))
         (location (hax/log--format-location file line function))
         (text (format "[%s] %s\n" location message)))
    (hax/log--emit text
                   print-stdout
                   print-stderr
                   append-to
                   redirect-and-overwrite)))

(defmacro hax/log (&rest args)
  (let ((file (or (macroexp-file-name)
                  load-file-name
                  buffer-file-name))
        (line (line-number-at-pos))
        (function
         (save-excursion
           (ignore-errors
             (beginning-of-defun)
             (let ((form (read (current-buffer))))
               (when (memq (car-safe form) '(defun cl-defun defmacro cl-defmacro))
                 (nth 1 form)))))))
    `(hax/log--call ,file ,line ',function ,@args)))
