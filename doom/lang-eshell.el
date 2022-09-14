;;; -*- lexical-binding: t; -*-

(defun dir-replace-home (dir)
  "Replace home in DIR with tilde (~) character."
  (interactive)
  (let* ((home (expand-file-name (getenv "HOME")))
         (home-len (length home)))
    (if (and
         (>= (length dir) home-len)
         (equal home (substring dir 0 home-len)))
        (concat "~" (substring dir home-len))
      dir)))

(defun dir-shorten-dirs (dir)
  "Shorten all directory names in DIR except the last two."
  (let ((p-lst (split-string dir "/")))
    (if (> (length p-lst) 2)
        (mapconcat (lambda (elm) elm) (last p-lst 2) "/")
      dir)))

(defun split-directory-prompt (directory)
  (if (string-match-p ".*/.*" directory)
      (list (file-name-directory directory) (file-name-base directory))
    (list "" directory)))

(cl-defun s-align-right (num s &optional (fill " "))
  "Align string right"
  (concat (s-repeat (- num (length s)) fill) s))

(defun eshell/format-exec-time (u)
  (let* ((ms (round (* 1000 (- u (floor u)))))
         (desc (ts-human-duration u))
         (s (plist-get desc :seconds))
         (m (plist-get desc :minutes))
         (h (plist-get desc :hours)))
    (concat
     (if (< 0 h) (format "%sh" h) "")
     (if (< 0 m) (format "%sm" m) "")
     (if (< 0 s) (format "%ss" s) "")
     (format "%sms" ms))))

(defun hax/eshell-right-prompt ()
  (concat
   (propertize "# -> " 'face '(:foreground "green"))
   (let ((code (eshell-get-variable "?")))
     (if (and code (< 0 code))
         (propertize (format "exit %s, " code) 'face '(:foreground "red"))
       ""))
   (if hax/eshell-timer
       (format
        "took %s"
        (let* ((start hax/eshell-timer)
               (diff (ts-difference (ts-now) start)))
          (eshell/format-exec-time diff)))
     "")))

;; https://github.com/howardabrams/dot-files/blob/master/emacs-eshell.org
(defun hax/eshell-prompt ()
  "A prompt for eshell that works locally (in that is assumes
that it could run certain commands) in order to make a prettier,
more-helpful local prompt."
  (interactive)
  (let* ((dir        (eshell/pwd))
         (directory (split-directory-prompt
                     (dir-shorten-dirs
                      (dir-replace-home dir))))
         (parent (car directory))
         (name   (cadr directory))
         (branch (magit-get-current-branch))

         (dark-env (eq 'dark (frame-parameter nil 'background-mode)))
         (for-bars                 `(:weight bold))
         (for-parent  (if dark-env `(:foreground "dark orange") `(:foreground "blue")))
         (for-dir     (if dark-env `(:foreground "orange" :weight bold)
                        `(:foreground "blue" :weight bold)))
         (for-git                  `(:foreground "green"))
         (right (hax/eshell-right-prompt)))

    (concat
     (s-align-right (- (window-total-width) 4) right)
     (propertize "\n")
     (propertize "# "    'face for-bars)
     (propertize parent   'face for-parent)
     (propertize name     'face for-dir)
     (when branch (format ":%s" (propertize branch 'face for-git)))
     (propertize " λ" 'face '(:foreground "light blue"))
     (propertize "\n"     'face for-bars)
     (propertize (if (= (user-uid) 0) " #" "# > ") 'face `(:weight ultra-bold)))))


(defun eshell/send-input-remember ()
  (interactive)
  (message "recalling point")
  (setq-local hax/last-stored-command-point (eshell/position-in-command))
  (eshell-queue-input))

(defun hax/eshell-hook ()
  (interactive)
  (abbrev-mode 1)
  (define-abbrev-table 'eshell-mode-abbrev-table
    '(("gis" "git status")
      ("rm" "rm -rf")
      ))

  (map!
   :leader
   :map eshell-mode-map
   ;; Eshell buffer is always considered to be 'modified' and the changes
   ;; are never required for storage, so kill it without any prompt DDoS
   "bd" #'kill-this-buffer
   )

  (map!
   :map eshell-mode-map
   "C-r" #'eshell/ivy-history
   "<C-M-ret>" #'eshell/send-input-remember
   )

  (map!
   :map eshell-mode-map
   :leader
   "y" #'eshell/copy-last-command-execution)


  (setq eshell-term-name "xterm")
  ;; Only search for the second line in the eshell prompt - it is very
  ;; simple.
  (setq eshell-prompt-regexp "# > "
        eshell-prompt-function 'hax/eshell-prompt))

(defvar-local hax/eshell-timer nil
  "Timer between two commands in the current buffer")


(defvar-local hax/last-stored-command-point nil)

(defun eshell/position-in-command ()
  "Get current cursor position in the command text"
  (interactive)
  (- (point) (save-excursion (eshell-bol))))

(defun hax/eshell-pre-command-hook ()
  (interactive)
  (setq-local hax/eshell-timer nil)
  (setq-local hax/eshell-timer (ts-now)))

(defun hax/eshell-post-command-hook ()
  (interactive)
  (rename-buffer (format "eshell at %s" (eshell/pwd)) t))

(defun eshell/ivy-history ()
  (interactive)
  (let ((history
         (delete-dups
          (mapcar (lambda (str)
                    (string-trim (substring-no-properties str)))
                  (ring-elements eshell-history-ring))))
        (input (let* ((beg (save-excursion (eshell-bol)))
                      (end (save-excursion (end-of-line) (point))))
                 (buffer-substring-no-properties beg end))))
    (ivy-read "[history] "
              history
              :action (lambda (x)
                        (end-of-line)
                        (eshell-kill-input)
                        (insert x))
              :initial-input input)))

(defun eshell/copy-last-command-execution ()
  (interactive)
  (let* ((end (save-excursion
                (eshell-goto-input-start)
                (forward-line -1)
                (goto-char (line-beginning-position))))
         (start (save-excursion
                  (eshell-goto-input-start)
                  (forward-line)
                  (re-search-forward eshell-prompt-regexp nil t)
                  (forward-line -2)
                  (line-end-position))))
    (evil-yank start end)))

(after! eshell
  (map! "<M-RET>" #'eshell-new)
  (add-hook! 'eshell-mode-hook 'hax/eshell-hook)
  (add-hook! 'eshell-post-command-hook 'hax/eshell-post-command-hook)
  (add-hook! 'eshell-pre-command-hook 'hax/eshell-pre-command-hook))

(defun eshell-new()
  "Open a new instance of eshell."
  (interactive)
  (eshell 'N))

;; Customize all shell color codes, including ones used by the eshell iself.
(custom-set-faces
 `(ansi-color-black          ((t (:foreground, (doom-color 'bg)))))
 `(ansi-color-red            ((t (:foreground, (doom-color 'red)))))
 `(ansi-color-green          ((t (:foreground, (doom-color 'green)))))
 `(ansi-color-yellow         ((t (:foreground, (doom-color 'yellow)))))
 `(ansi-color-blue           ((t (:foreground, (doom-color 'blue)))))
 `(ansi-color-magenta        ((t (:foreground, (doom-color 'magenta)))))
 `(ansi-color-cyan           ((t (:foreground, (doom-color 'cyan)))))
 `(ansi-color-gray           ((t (:foreground, (doom-color 'fg)))))
 `(ansi-color-bright-black   ((t (:foreground, (doom-color 'bg)))))
 `(ansi-color-bright-red     ((t (:foreground, (doom-color 'red)))))
 `(ansi-color-bright-green   ((t (:foreground, (doom-color 'green)))))
 `(ansi-color-bright-yellow  ((t (:foreground, (doom-color 'yellow)))))
 `(ansi-color-bright-blue    ((t (:foreground, (doom-color 'blue)))))
 `(ansi-color-bright-magenta ((t (:foreground, (doom-color 'magenta)))))
 `(ansi-color-bright-cyan    ((t (:foreground, (doom-color 'cyan))))))
