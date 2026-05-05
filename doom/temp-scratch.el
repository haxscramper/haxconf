;;; -*- lexical-binding: t; -*-

(when nil
  (defun json-parse-file (file)
    (json-parse-string (f-read file)))

  (s-replace-regexp
   (rx (group (1+ digit)) " "
       (group rx-month-name-or-digit) " "
       (group (1+ digit)))
   "\\3-\\2-\\1"
   "27 March 2020"
   )

  (require 'dbus)

  (setq shell-file-name (executable-find "sh"))

  (defun my-dbus-method-handler (logical physical quote)
    (insert (format "- %s (%s) /\"%s\"/" logical physical quote))
    '(:boolean t))

  (defun hax/dbus-register-methods (method-list)
    (dolist (method method-list)
      (dbus-register-method
       :session "hax.haxconf.Emacs" "/hax/haxconf/Emacs"
       "hax.haxconf.Emacs"
       (car method)
       (cdr method))))

  (hax/dbus-register-methods
   (list
    (cons "InsertPdfQuoteBlock2"
          (lambda (logical physical quote)
            (insert (format "#+caption: %s (%s)\n#+begin_quote\n%s\n#+end_quote\n"
                            logical physical quote))))
    (cons "InsertPdfQuoteItem2"
          (lambda (logical physical quote)
            (insert (format "- %s (%s) /\"%s\"/"
                            logical physical quote))))
    (cons "PrintTest" (lambda (input) (message "%s" input)))))


  (set-face-attribute 'winum-face nil :weight 'bold :foreground "green"))


(require 'cl-lib)
(require 'wid-edit)

(cl-defstruct hax/tree-node
  label
  checked
  children)

(defvar hax/tree-wizard--root nil)
(defvar hax/tree-wizard--result nil)
(defconst hax/tree-wizard--buffer "*hax-tree-wizard*")

(defun hax/tree-wizard--sample-tree ()
  (make-hax/tree-node
   :label "Workspace"
   :checked nil
   :children
   (list
    (make-hax/tree-node
     :label "Project A"
     :checked t
     :children
     (list
      (make-hax/tree-node
       :label "Build"
       :checked nil
       :children
       (list
        (make-hax/tree-node
         :label "Linux"
         :checked t
         :children
         (list
          (make-hax/tree-node
           :label "Debug"
           :checked nil
           :children
           (list
            (make-hax/tree-node
             :label "Warnings as errors"
             :checked t
             :children nil)
            (make-hax/tree-node
             :label "Sanitizers"
             :checked nil
             :children nil)))
          (make-hax/tree-node
           :label "Release"
           :checked t
           :children
           (list
            (make-hax/tree-node
             :label "LTO"
             :checked t
             :children nil)))))
        (make-hax/tree-node
         :label "Windows"
         :checked nil
         :children
         (list
          (make-hax/tree-node
           :label "MSVC"
           :checked t
           :children nil)))))
      (make-hax/tree-node
       :label "Tests"
       :checked nil
       :children
       (list
        (make-hax/tree-node :label "Unit" :checked t :children nil)
        (make-hax/tree-node :label "Integration" :checked nil :children nil)))))
    (make-hax/tree-node
     :label "Project B"
     :checked nil
     :children
     (list
      (make-hax/tree-node
       :label "Deploy"
       :checked t
       :children
       (list
        (make-hax/tree-node
         :label "Staging"
         :checked t
         :children
         (list
          (make-hax/tree-node
           :label "Smoke tests"
           :checked t
           :children
           (list
            (make-hax/tree-node
             :label "HTTP healthcheck"
             :checked t
             :children nil)))))))
      (make-hax/tree-node
       :label "Docs"
       :checked nil
       :children nil))))))

(defun hax/tree-wizard--copy-node (node)
  (make-hax/tree-node
   :label (hax/tree-node-label node)
   :checked (hax/tree-node-checked node)
   :children (mapcar #'hax/tree-wizard--copy-node
                     (hax/tree-node-children node))))

(defun hax/tree-wizard--node-at-path (root path)
  (let ((node root))
    (dolist (idx path)
      (setq node (nth idx (hax/tree-node-children node))))
    node))

(defun hax/tree-wizard--delete-at-path (path)
  (let* ((parent-path (butlast path))
         (idx (car (last path)))
         (parent (if parent-path
                     (hax/tree-wizard--node-at-path hax/tree-wizard--root parent-path)
                   hax/tree-wizard--root)))
    (setf (hax/tree-node-children parent)
          (cl-loop for child in (hax/tree-node-children parent)
                   for i from 0
                   unless (= i idx)
                   collect child))))

(defun hax/tree-wizard--add-child (node)
  (push (make-hax/tree-node
         :label "New item"
         :checked nil
         :children nil)
        (hax/tree-node-children node))
  (setf (hax/tree-node-children node)
        (nreverse (hax/tree-node-children node)))
  (hax/tree-wizard--render))

(defun hax/tree-wizard--insert-node (node path depth)
  (let ((indent (make-string (* depth 2) ?\s)))
    (widget-insert indent)

    (widget-create
     'checkbox
     :value (hax/tree-node-checked node)
     :notify
     (lambda (widget &rest _)
       (setf (hax/tree-node-checked node) (widget-value widget))))

    (widget-insert " ")

    (widget-create
     'editable-field
     :size 30
     :format "%v"
     :value (hax/tree-node-label node)
     :notify
     (lambda (widget &rest _)
       (setf (hax/tree-node-label node) (widget-value widget))))

    (widget-insert " ")

    (widget-create
     'push-button
     :tag "+child"
     :notify
     (lambda (&rest _)
       (hax/tree-wizard--add-child node)))

    (widget-insert " ")

    (unless (null path)
      (widget-create
       'push-button
       :tag "delete"
       :notify
       (lambda (&rest _)
         (hax/tree-wizard--delete-at-path path)
         (hax/tree-wizard--render)))
      (widget-insert " "))

    (widget-insert "\n")

    (cl-loop for child in (hax/tree-node-children node)
             for idx from 0
             do (hax/tree-wizard--insert-node
                 child
                 (append path (list idx))
                 (1+ depth)))))

(defun hax/tree-wizard--finish ()
  (interactive)
  (setq hax/tree-wizard--result
        (hax/tree-wizard--copy-node hax/tree-wizard--root))
  (quit-window t (get-buffer-window hax/tree-wizard--buffer)))

(defun hax/tree-wizard--cancel ()
  (interactive)
  (setq hax/tree-wizard--result nil)
  (quit-window t (get-buffer-window hax/tree-wizard--buffer)))

(defun hax/tree-wizard--render ()
  (with-current-buffer (get-buffer-create hax/tree-wizard--buffer)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (remove-overlays)
      (kill-all-local-variables)
      (special-mode)
      (setq-local widget-button-face 'custom-button)
      (widget-insert "Nested tree wizard\n\n")
      (widget-insert "Mouse click works on checkboxes and buttons.\n\n")
      (hax/tree-wizard--insert-node hax/tree-wizard--root nil 0)
      (widget-insert "\n")
      (widget-create
       'push-button
       :tag "Finish"
       :notify (lambda (&rest _) (hax/tree-wizard--finish)))
      (widget-insert " ")
      (widget-create
       'push-button
       :tag "Cancel"
       :notify (lambda (&rest _) (hax/tree-wizard--cancel)))
      (widget-insert "\n")
      (use-local-map widget-keymap)
      (widget-setup)))
  (pop-to-buffer hax/tree-wizard--buffer))

(defun hax/tree-wizard-run ()
  (interactive)
  (setq hax/tree-wizard--root (hax/tree-wizard--sample-tree))
  (setq hax/tree-wizard--result nil)
  (hax/tree-wizard--render)
  hax/tree-wizard--result)


