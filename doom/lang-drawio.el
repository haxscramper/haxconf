;;; -*- lexical-binding: t; -*-
(require 'ob)
(require 'subr-x)

(defvar org-babel-default-header-args:drawio
  '((:results . "file link replace")
    (:exports . "results")))

(defconst hax/drawio--empty-document
  "<mxfile host=\"app.diagrams.net\">
  <diagram id=\"page-1\" name=\"Page-1\">
    <mxGraphModel>
      <root>
        <mxCell id=\"0\"/>
        <mxCell id=\"1\" parent=\"0\"/>
      </root>
    </mxGraphModel>
  </diagram>
</mxfile>")

(defun hax/drawio--run (&rest arguments)
  (hax/log
   (format "Running drawio: %s"
           (mapconcat #'shell-quote-argument
                      (cons "drawio" arguments)
                      " "))
   :print-stdout)

  (with-temp-buffer
    (let ((status (apply #'call-process
                         "drawio"
                         nil
                         (current-buffer)
                         nil
                         arguments))
          output)
      (setq output (string-trim (buffer-string)))

      (unless (string-empty-p output)
        (hax/log output :print-stdout))

      (unless (eq status 0)
        (error "drawio failed with status %S: %s" status output)))))

(defun hax/drawio--read-file (file)
  (with-temp-buffer
    (insert-file-contents file)
    (string-trim-right (buffer-string) "[\r\n]+")))

(defun org-babel-execute:drawio (body _params)
  (let* ((document-directory
          (if-let ((document-file (buffer-file-name)))
              (file-name-directory document-file)
            default-directory))
         (diagram
          (if (string-empty-p (string-trim body))
              hax/drawio--empty-document
            body))
         (hash (secure-hash 'sha256 diagram))
         (output-name (format "drawio-PNG-%s.png" hash))
         (output-file (expand-file-name output-name document-directory))
         (source-file (make-temp-file "org-drawio-" nil ".drawio")))
    (unwind-protect
        (progn
          (write-region diagram nil source-file nil 'silent)

          (hax/drawio--run
           "--export"
           "--format" "png"
           "--embed-diagram"
           "--output" output-file
           source-file)

          output-name)
      (when (file-exists-p source-file)
        (delete-file source-file)))))

(defun hax/drawio-edit-block ()
  (interactive)
  (let* ((element (org-element-context))
         (body (org-element-property :value element))
         (source-file (make-temp-file "org-drawio-" nil ".drawio"))
         (initial-body
          (if (string-empty-p (string-trim body))
              hax/drawio--empty-document
            body))
         edited-body)
    (unwind-protect
        (progn
          (write-region initial-body nil source-file nil 'silent)

          ;; Blocks until Drawio exits.
          (hax/drawio--run source-file)

          (setq edited-body (hax/drawio--read-file source-file))
          (when (string-empty-p edited-body)
            (error "Drawio produced an empty diagram"))

          (org-babel-update-block-body edited-body))
      (when (file-exists-p source-file)
        (delete-file source-file)))))

(defun hax/org-edit-special-drawio (original-function &rest arguments)
  (let ((element (org-element-context)))
    (if (and (eq (org-element-type element) 'src-block)
             (string= (org-element-property :language element) "drawio"))
        (hax/drawio-edit-block)
      (apply original-function arguments))))

(advice-add 'org-edit-special :around #'hax/org-edit-special-drawio)

(provide 'lang-drawio)
