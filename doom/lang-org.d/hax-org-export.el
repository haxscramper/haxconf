;;; -*- lexical-binding: t; -*-

(defun org-export-gfm-to-clipboard ()
  "Export selected text or current subtree in org-mode buffer as GitHub-flavored markdown and copy it to clipboard."
  (interactive)
  (require 'ox-gfm)
  (let ((region-p (region-active-p))
        export-str)
    (if region-p
        (setq export-str (buffer-substring (region-beginning) (region-end)))
      (save-excursion
        (org-back-to-heading t)
        (setq export-str (buffer-substring (point) (org-end-of-subtree t)))))
    (with-temp-buffer
      (insert export-str)
      (let* ((org-mode-hook nil)  ; avoid running org-mode hooks
             (backend (org-export-get-backend 'gfm))
             (org-export-with-toc nil))
        (org-mode)
        (org-export-to-buffer backend "*Org GFM Export*")
        (kill-ring-save (point-min) (point-max))
        (kill-buffer "*Org GFM Export*")))
    (if region-p
        (deactivate-mark))
    (hax/log "Exported to GFM and copied to clipboard.")))

(setq org-latex-listings 'minted
      org-latex-packages-alist '(("" "minted"))
      org-latex-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))


