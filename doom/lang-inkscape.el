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

(defconst hax/inkscape--large-document-word-limit 1000)

(defun hax/inkscape--large-file-path (body)
  (condition-case nil
      (pcase-let* ((`(,value . ,position)
                    (read-from-string body))
                   (remainder
                    (string-trim
                     (substring body position))))
        (when (and (string-empty-p remainder)
                   (listp value)
                   (= (length value) 2)
                   (eq (car value) :large-file-path)
                   (stringp (cadr value)))
          (cadr value)))
    (error nil)))

(defun hax/inkscape--word-count (text)
  (with-temp-buffer
    (insert text)
    (count-words (point-min) (point-max))))

(defun hax/inkscape--large-output-path ()
  (let ((document-file (buffer-file-name)))
    (unless document-file
      (user-error
       "Save the Org document before creating a large Inkscape diagram"))

    (let* ((document-directory
            (file-name-directory document-file))
           (base-name
            (file-name-sans-extension
             (file-name-nondirectory document-file)))
           (image-directory-name
            (concat base-name ".images"))
           (image-directory
            (expand-file-name
             image-directory-name
             document-directory))
           (timestamp
            (format-time-string "%Y%m%dT%H%M%S-%N"))
           (file-name
            (format "diagram-%s.svg" timestamp)))
      (make-directory image-directory t)
      (cons
       (expand-file-name file-name image-directory)
       (concat image-directory-name "/" file-name)))))


(defun org-babel-execute:inkscape (body _params)
  (let* ((document-directory
          (if-let ((document-file (buffer-file-name)))
              (file-name-directory document-file)
            default-directory))
         (large-file-path
          (hax/inkscape--large-file-path body)))
    (if large-file-path
        (let ((absolute-path
               (expand-file-name
                large-file-path
                document-directory)))
          (unless (file-regular-p absolute-path)
            (error "Inkscape SVG does not exist: %s"
                   absolute-path))
          large-file-path)
      (let* ((svg
              (if (string-empty-p (string-trim body))
                  hax/inkscape--empty-document
                body))
             (hash
              (secure-hash 'sha256 svg))
             (output-name
              (format "inkscape-SVG-%s.svg" hash))
             (output-file
              (expand-file-name
               output-name
               document-directory)))
        (write-region svg nil output-file nil 'silent)
        output-name))))



(defun hax/inkscape-edit-block ()
  (interactive)
  (let* ((element
          (org-element-context))
         (block-begin
          (org-element-property :begin element))
         (body
          (or (org-element-property :value element) ""))
         (document-directory
          (if-let ((document-file (buffer-file-name)))
              (file-name-directory document-file)
            default-directory))
         (large-file-path
          (hax/inkscape--large-file-path body))
         (large-file
          (when large-file-path
            (expand-file-name
             large-file-path
             document-directory)))
         (existing
          (or large-file-path
              (not (string-empty-p (string-trim body)))))
         (initial-body
          (if existing
              body
            hax/inkscape--empty-document))
         (temporary-source
          (unless large-file
            (make-temp-file
             "org-inkscape-edit-"
             nil
             ".svg")))
         (source-file
          (or large-file temporary-source))
         edited-body
         updated-body)
    (when (and large-file
               (not (file-regular-p large-file)))
      (error "Inkscape SVG does not exist: %s"
             large-file))

    (unwind-protect
        (progn
          (unless large-file
            (write-region
             initial-body
             nil
             source-file
             nil
             'silent))

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

          (setq updated-body
                (cond
                 (large-file-path
                  (format
                   "(:large-file-path %S)"
                   large-file-path))

                 ((>
                   (hax/inkscape--word-count edited-body)
                   hax/inkscape--large-document-word-limit)
                  (pcase-let*
                      ((`(,output-file . ,relative-path)
                        (hax/inkscape--large-output-path)))
                    (write-region
                     edited-body
                     nil
                     output-file
                     nil
                     'silent)
                    (format
                     "(:large-file-path %S)"
                     relative-path)))

                 (t
                  edited-body)))

          (org-babel-update-block-body updated-body)
          (goto-char block-begin)
          (org-babel-execute-src-block))
      (when (and temporary-source
                 (file-exists-p temporary-source))
        (delete-file temporary-source)))))


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

