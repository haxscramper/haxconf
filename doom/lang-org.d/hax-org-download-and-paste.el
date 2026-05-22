;;; -*- lexical-binding: t; -*-

(defun hax/org-download-setup ()
  (interactive)
  ;; NOTE function redefinition should happen after the hook.
  (defun org-download-insert-link (link filename)
    ;; HACK Setting `org-download-display-inline-images' to `nil' does not
    ;; work - I need to explicitly override function call with an empty
    ;; function. I also need to remove all advices that were put on this
    ;; symbol to prevent `+org' hook from triggering.
    (let* ((beg (point))
           (line-beg (line-beginning-position))
           str)
      (insert
       (format "[[file:%s][image]]"
               (org-link-escape
                (funcall org-download-abbreviate-filename-function filename))))
      (setq str (buffer-substring-no-properties line-beg (point)))
      str))

  (advice-remove-all 'org-download-insert-link)

  (let ((base
         (if (buffer-file-name) (buffer-file-name)
           (let ((buf (org-capture-get :buffer)))
             (if buf (buffer-file-name buf)) (f-join hax/indexed.d "images")))))
    (setq org-download-image-dir
          (f-join (f-dirname base) (concat (f-base base) ".images")))
    (setq org-download-timestamp (concat (f-filename base) "_%Y%m%d-%H%M%S_" ))
    (setq org-image-actual-width 300)
    ;; Update configuration to it's original values - some doom emacs
    ;; configuration changes these as well, and I don't need it.
    (setq org-download-link-format-function
          #'org-download-link-format-function-default)
    ;; Don't redisplay all images in file on each insertion - if I need to
    ;; redisplay them, I can do it perfectly well by myself.
    (setq org-download-display-inline-images nil)
    (setq org-download-abbreviate-filename-function #'file-relative-name)
    (setq org-download-method 'directory)
    (setq org-download-heading-lvl nil)))

(defun hax/org-paste-clipboard (&optional default-name to-monochrome-image)
  (interactive "P")
  (require 'org-download)
  (let ((file
         (if default-name
             (read-string
              (format "Filename [%s]: " org-download-screenshot-basename)
              nil nil org-download-screenshot-basename)

           default-name))
        (out (s-split
              "\n"
              (with-output-to-string
                (call-process
                 "xclip" nil standard-output nil
                 "-o" "-selection" "cli" "-t" "TARGETS")))))
    ;; If clipboard does not contain image use regular pasting logic,
    ;; otherwise insert image. Mapybe `p' should work differently in
    ;; org-mode instead, but I'm not sure about that.
    (if (--any? (s-starts-with? "image/" it) out)
        (progn
          ;; (when to-monochrome-image
          ;;   (shell-command-to-string
          ;;    "xclip -selection clipboard -t image/png -o > /tmp/org-down-colored.png")
          ;;   (shell-command-to-string
          ;;    "convert /tmp/org-down-colored.png -monochrome /tmp/monochrome.png")
          ;;   (shell-command-to-string
          ;;    "xclip -sel cli -t image/png -i /tmp/monochrome.png")
          ;;   )
          (org-download-clipboard file)
          )
      (evil-paste-after-without-register 1))))
