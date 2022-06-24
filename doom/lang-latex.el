;;; -*- lexical-binding: t; -*-

(cl-defun tex-get-buffer-pdf (&optional buffer)
  "Return pdf file associated with buffer (current buffer is used
  if no argument is supplied)"
  (concat
   (f-base (if buffer (buffer-file-name buffer) (buffer-file-name)))
   ".pdf"))

(defun zathura-open-latex ()
  "Open pdf file associated with current latex buffer"
  (interactive)
  (call-process-shell-command
   (concat "zathura " (tex-get-buffer-pdf) " &")
   nil
   0))

(defun lpr-print-latex ()
  "Use lpr to print file"
  (interactive)
  (let ((command (concat "lpr -o media=a4 " (tex-get-buffer-pdf) " &")))
    (when (yes-or-no-p (format "run printing using '%s' ?" command))
      (call-process-shell-command command nil 0))))

(defun hax/latex-mode-hook ()
  (interactive)
  ;;;#== Document viewers
  (setq TeX-view-program-list '(("zathura" "zathura %o")))
  (setq TeX-view-program-selection '((output-pdf "zathura")))
  ;; (yasnippet-snippets-initialize)
  ;; (highlight-parentheses-mode 1)
  (LaTeX-add-environments
   '("Remark" LaTeX-env-with-preset "" ""))
  (map!
   :n ",o" 'zathura-open-latex))

(add-hook 'latex-mode-hook 'hax/latex-mode-hook)
(add-hook 'LaTeX-mode-hook 'hax/latex-mode-hook)



(setq TeX-master nil)
