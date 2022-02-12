;;; package -- ??
;;; Code:
;;; Commentary:

(toggle-debug-on-error)

(setq backtrace-print-function
 (lambda (a b) (let ((print-level 1200)
              (print-length 1200)))
   (princ a b)))


(require 'haxloader)
;; (setq haxloader-dirs '(buffer-file-name))
(hax:load-file "daily_note.nim")

(hax:load-file "module.nim")
(message "hello")
(message "[%s]" (test))
(message "emcall: [%s]" (hax:bind 1))

(hax:load-file "mutable_module.nim")

(setq notes-dir "/aaaa")
(hax:open-daily-note 'weekly)

(provide 'main)
;;; main.el ends here

