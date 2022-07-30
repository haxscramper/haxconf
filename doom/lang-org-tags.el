;;; -*- lexical-binding: t; -*-

(defvar hax/org-tags-docs (make-hash-table) "Documentation for some of the used org-mode tags")



(when t
  (setq
   tmp
   '(("code" . "Markup and programming languages")
     ("idea" . "Might be turned into todo, but probably won't be")
     ("programming" .
      ("Programming-related activities" .
       (("gui" . "Writing graphical applications"))))
     ("software" . "About different programs")))
  (setq hax/org-tags-docs (make-hash-table :test 'equal))
  (defun hax/tmp/rec-build-org-tag-docs (item &optional prefix)
    (dolist (i item)
      (let ((value (cdr i))
            (key (if prefix (concat prefix (car i)) (car i))))
        (if (listp value)
            (progn (puthash key (car value) hax/org-tags-docs)
                   (hax/tmp/rec-build-org-tag-docs
                    (cdr value)
                    (format "%s##" (car i))))
          (puthash key value hax/org-tags-docs)))))

  (hax/tmp/rec-build-org-tag-docs tmp)
  (gethash "programming##gui" hax/org-tags-docs))


