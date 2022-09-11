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
         )

    (concat
     (propertize "# "    'face for-bars)
     (propertize parent   'face for-parent)
     (propertize name     'face for-dir)
     (when branch
       (concat (propertize "  "    'face for-git)
               (propertize branch   'face for-git)))
     (propertize "\n"     'face for-bars)
     (propertize (if (= (user-uid) 0) " #" "# >") 'face `(:weight ultra-bold))
     (propertize " "    'face `(:weight bold)))))

(defun hax/eshell-hook ()
  (interactive)
  (setq eshell-term-name "xterm")
  (setq eshell-prompt-regexp "^[^#$\n]*[#$] "
        eshell-prompt-function 'hax/eshell-prompt))

(defvar-local hax/eshell-timer nil
  "Timer between two commands in the current buffer")

(defun hax/eshell-pre-command-hook ()
  (interactive)
  (setq-local hax/eshell-timer (ts-now)))

(defun hax/eshell-post-command-hook ()
  (interactive)
  (when (and (boundp 'start) start)
    (let* ((start hax/eshell-timer)
           (diff (ts-difference (ts-now) start)))
      (message (ts-human-format-duration diff))))
  (rename-buffer (format "eshell at %s" (eshell/pwd)))
  )

(after! eshell
  (map! "<M-RET>" #'eshell-new)
  (map!
   :leader
   ;; Eshell buffer is always considered to be 'modified' and the changes
   ;; are never required for storage, so kill it without any prompt DDoS
   "bd" #'kill-this-buffer
   )
  (add-hook! 'eshell-mode-hook 'hax/eshell-hook)
  (add-hook! 'eshell-post-command-hook 'hax/eshell-post-command-hook)
  (add-hook! 'eshell-pre-command-hook 'hax/eshell-pre-command-hook)
  )

(defun eshell-new()
  "Open a new instance of eshell."
  (interactive)
  (eshell 'N))
