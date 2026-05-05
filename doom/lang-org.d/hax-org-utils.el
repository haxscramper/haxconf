;;; -*- lexical-binding: t; -*-

(defun hax/org-get-outline-path-full ()
  "Return the full path of the current subtree."
  (save-window-excursion
    (save-excursion
      (let ((path (org-get-heading t t t t)))
        (if (org-up-heading-safe)
            (concat (hax/org-get-outline-path-full) "/" path)
          path)) ) ))

(defun hax/get-subtree-id-for-marker (marker)
  (save-window-excursion
    (save-excursion
      (with-current-buffer (marker-buffer marker)
        ;; Save excursion to avoid moving cursor in
        ;; other buffers (or in the same buffer if
        ;; linking within one file)
        (save-excursion
          (goto-char (marker-position marker))
          (org-id-get-create))))))

(defun hax/org-unformat-title (title)
  (s-replace-all '(("=" . "") ("*" . "") ("~" . "")) title))

(defun hax/set-archive-to-full-path ()
  "Set the :ARCHIVE: property of the current subtree to its full path."
  (interactive)
  (let ((full-path (hax/org-get-outline-path-full)))
    (org-set-property "ARCHIVE" (format "%%s_archive::* %s" full-path))))

(defun hax/org-outline-path-at-marker (marker &optional last-n with-tags cleanup-name-formatting)
  "Get formatted outline entry at position specified by MARKER"
  (with-current-buffer (marker-buffer marker)
    (save-excursion
      (goto-char (marker-position marker))
      ;; If full path is requested, return it
      ;; formatted directly, otherwise fall back to
      ;; default formatting logic.
      (let* ((tags (org-get-tags-string))
             (loc (-slice (org-get-outline-path t)
                          (if last-n (- 0 last-n) 0)))
             (path (org-format-outline-path loc 256)))
        (concat (if cleanup-name-formatting (hax/org-unformat-title path) path)
                (if with-tags tags ""))))))
