;;; -*- lexical-binding: t; -*-

(require 'ob)
(require 'org-element)
(require 'subr-x)

(defvar org-babel-default-header-args:inkscape
  '((:results . "file link replace")
    (:exports . "results")))

(defconst hax/inkscape--empty-document
  "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>
<svg
  xmlns=\"http://www.w3.org/2000/svg\"
  xmlns:inkscape=\"http://www.inkscape.org/namespaces/inkscape\"
  width=\"1000px\"
  height=\"1000px\"
  viewBox=\"0 0 1000 1000\">
</svg>")

(defconst hax/inkscape--command "lang-inkscape.py")

(defun hax/inkscape--run (&rest arguments)
  (hax/log
   (format "Running Inkscape helper: %s"
           (mapconcat
            #'shell-quote-argument
            (cons hax/inkscape--command arguments)
            " "))
   :print-stdout)

  (with-temp-buffer
    (let ((status
           (apply #'call-process
                  hax/inkscape--command
                  nil
                  (current-buffer)
                  nil
                  arguments))
          output)
      (setq output (string-trim (buffer-string)))

      (unless (string-empty-p output)
        (hax/log output :print-stdout))

      (unless (and (integerp status)
                   (zerop status))
        (error "Inkscape helper failed with status %S: %s"
               status
               output)))))

(defun hax/inkscape--read-file (file)
  (with-temp-buffer
    (insert-file-contents file)
    (string-trim-right (buffer-string) "[\r\n]+")))

(defun org-babel-execute:inkscape (body _params)
  (let* ((document-directory
          (if-let ((document-file (buffer-file-name)))
              (file-name-directory document-file)
            default-directory))
         (svg
          (if (string-empty-p (string-trim body))
              hax/inkscape--empty-document
            body))
         (hash (secure-hash 'sha256 svg))
         (output-name (format "inkscape-SVG-%s.svg" hash))
         (output-file
          (expand-file-name output-name document-directory)))
    (write-region svg nil output-file nil 'silent)
    output-name))


(defun hax/inkscape-edit-block ()
  (interactive)
  (let* ((element (org-element-context))
         (body (org-element-property :value element))
         (existing
          (not (string-empty-p (string-trim body))))
         (initial-body
          (if existing
              body
            hax/inkscape--empty-document))
         (source-file
          (make-temp-file "org-inkscape-" nil ".svg"))
         edited-body)
    (unwind-protect
        (progn
          (write-region initial-body nil source-file nil 'silent)

          (if existing
              (hax/inkscape--run
               "edit"
               "--existing"
               source-file)
            (hax/inkscape--run
             "edit"
             source-file))

          (setq edited-body
                (hax/inkscape--read-file source-file))

          (when (string-empty-p edited-body)
            (error "Inkscape produced an empty SVG"))

          (org-babel-update-block-body edited-body))
      (when (file-exists-p source-file)
        (delete-file source-file)))))

(defun hax/org-edit-special-inkscape
    (original-function &rest arguments)
  (let ((element (org-element-context)))
    (if (and (eq (org-element-type element) 'src-block)
             (string=
              (org-element-property :language element)
              "inkscape"))
        (hax/inkscape-edit-block)
      (apply original-function arguments))))

(advice-add
 'org-edit-special
 :around
 #'hax/org-edit-special-inkscape)

(provide 'lang-inkscape)

