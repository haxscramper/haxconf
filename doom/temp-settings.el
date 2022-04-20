;;; -*- lexical-binding: t; -*-

(cl-defun replace-selection
    (txt &optional
         (begin (get-selected-region-start))
         (end (get-selected-region-end)))
  (delete-region begin end)
  (insert txt))

(defun hax/nim-wrap-warnings ()
  (interactive)
  (replace-selection (format "(assert fase; {.warning: \"[FIXME] '%s'\".})"
                             (get-selected-region-text))))




(defun hax/insert-radio-link ()
  (interactive)
  (let ((targets
         (matches-in-buffer
          (rx "<<" (group (* (not ">"))) ">>" (group (* word)))
          (lambda () (cons (match-string 1) (match-string 2))))))
    (ivy-read
     "Select target"
     targets
     :action (lambda (x) (message "Selected tag {%s}" x)))))

(cl-defun hax/hex-to-binary (hex &optional
                                 (word-format "%s")
                                 (word-bits 8))
  (s-join
   " "
   (--map
    (format
     word-format
     (s-join " " (--map
                  (s-pad-left word-bits "0" it)
                  (seq-partition (binary-number-to-string (string-to-number it 16)) word-bits))))
    (s-split  " " hex))))

(defun hax/copy-hex-as-binary ()
  (interactive)
  (let* ((sel (hax/hex-to-binary (get-selected-region-text) "[%s]")))
    (with-temp-buffer
      (insert sel)
      (copy-region-as-kill (point-min) (point-max))
      (message "%s" (buffer-substring (point-min) (point-max)))))
  (deactivate-mark))

(defun org-babel-execute:hex (body params)
  (hax/hex-to-binary body "[%s]"))

(custom-set-variables
 '(mini-frame-show-parameters
   '((top . 200)
     (width . 0.8)
     (left . 0.5))))
