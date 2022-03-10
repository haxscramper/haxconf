;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "haxscramper"
      user-mail-address "haxscramper@gmail.com")



(setq doom-font (font-spec :family "JetBrains Mono" :size 18)
      doom-unicode-font doom-font
      doom-serif-font doom-font
      doom-variable-pitch-font doom-font)

(defun hax/buffer-face-mode-variable ()
  "Set current buffer fase to a different configuration"
  (interactive)
  (setq buffer-face-mode-face '(:family "Iosevka Mono"))
  (buffer-face-mode))


(after! unicode-fonts
  ;; Otherwise unicode fonts are not properly set in the configuration, and
  ;; text does not loop properly (uses default fallback consolas)
  (setq doom-unicode-font doom-font)

  )

;; `:ui unicode' does it's own fontset configuration, so I have to hack
;; this in like suggested on issue
;; https://github.com/hlissner/doom-emacs/issues/3298
(defadvice! add-my-font-config (&rest _) :after #'unicode-fonts--setup-1
  ;; Everson mono can be installed from aur as `ttf-everson-mono' (font
  ;; page is https://www.evertype.com/emono/)
  (set-fontset-font "fontset-default" 'mathematical "Everson Mono"))

(setq +file-templates-dir (expand-file-name "templates" (dir!)))
(set-file-template! "/readme\\.org$" :trigger "__readme.org" :mode 'org-mode)
(set-file-template! "\\.el$" :trigger "__" :mode 'emacs-lisp-mode)
(set-file-template! "\\.sh$" :trigger "__" :mode 'sh-mode)


(defun tree-repr (item)
  "Return tree representation of the item's structure"
  (let ((tyof (type-of item))
        (name (symbol-name tyof)))
    (case tyof
      ("vector" "vector")
      (t name))))

(defun hax/c++-rotate-infix ()
  "Rotate arguments of the current infix expression"
  ;; TODO expression can also be 'rotated', but that would require lookup
  ;; map.
  (interactive)
  (let* ((infix (tree-sitter-node-at-point 'binary_expression)))
    (if (not infix) (message "No infix node found at current position")
      (let* ((lhs (tsc-get-child-by-field infix :left))
             (rhs (tsc-get-child-by-field infix :right))
             (r-rhs (tsc-node-range rhs))
             (r-lhs (tsc-node-range lhs))
             ;; Infix operators are not named child nodes for most language
             ;; parsers.
             (op  (tsc-get-nth-child infix 1))
             ;; Remember infix element position to jump to it immediately.
             (pos (+ 1
                     (aref r-lhs 0)
                     (- (aref r-rhs 1) (aref r-rhs 0))))
             ;; Adding extra spacing here because tree-sitter nodes don't
             ;; include whitespace character in their boundaries.
             (text (format "%s %s %s"
                           (tsc-node-text rhs)
                           (tsc-node-text op)
                           (tsc-node-text lhs))))
        ;; Get full range of infix elements
        (save-excursion
          (delete-region (aref r-lhs 0) (aref r-rhs 1))
          (insert text))
        (goto-char pos)))))

(load! "evil-main.el")
(load! "evil-edit.el")

;; Load helper functions for working with selections, indentation and other
;; elements. They are used later on in different keybindings for various modes.
(load! "lib/selections.el")
(load! "lib/indentation.el")
(load! "lib/fontlock.el")
(load! "lib/searches.el")

;; Language-specific configuration files.
(load! "lang-nim.el")
(load! "lang-elvish.el")
(load! "lang-lisp.el")
(load! "lang-base.el")
(load! "lang-c.el")
(load! "lang-org.el")
(load! "lang-latex.el")

(load! "config-editing.el")

(setq confirm-kill-emacs nil)

(after! evil-mc
  (global-evil-mc-mode t))

;; Use `C-c C-return' to annotate single lines in the commit message. Maybe
;; this can somehow be integrated with github review (read the anotations
;; from the commit message and add them as review on github instead of
;; going there and repeating the process manually)
(defvar hax/last-commit-section nil)

(defun hax/magit-insert-section-location ()
  (interactive)
  (let* ((msg-buffer (get-buffer "COMMIT_EDITMSG"))
         (hunk (magit-diff-visit--hunk)))
    (when (and msg-buffer hunk)
      (let* ((goto-worktree nil)
             (window (get-buffer-window msg-buffer))
             (file (magit-file-relative-name (magit-file-at-point t t)))
             (goto-from (magit-diff-visit--goto-from-p hunk goto-worktree))
             (line (magit-diff-hunk-line hunk goto-from)))
        (setq hax/last-commit-section (list (current-buffer) (point)))
        (select-window window)
        (goto-char (line-end-position))
        (insert "\n")
        (insert (format "- %s:%s :: " file line))
        (evil-insert-state 1)
        (save-excursion (insert "\n"))))))

(defun hax/return-to-section-location ()
  (interactive)
  (when hax/last-commit-section
    (let* ((buffer (nth 0 hax/last-commit-section))
           (pos (nth 1 hax/last-commit-section)))
      (select-window (get-buffer-window buffer))
      (goto-char pos))))

(map!
 :map magit-diff-mode-map
 :n "C-c <C-return>" #'hax/magit-insert-section-location)

(map!
 :map with-editor-mode-map
 :ni "C-c <C-return>" #'hax/return-to-section-location)

(after! magit
  (setq git-commit-major-mode 'org-mode)
  (add-to-list 'magit-no-confirm 'stage-all-changes)
  (defun hax/magit-force-push-current ()
    "random documentation string if krux ever wants to get any
more nitpickery about stuff I write in my configuration files."
    (interactive)
    (magit-git-command
     (format "git push --force origin %s"
             (magit-get-current-branch))))

  (map! :leader
        :desc "new commit"
        :nv "gcc" (cmd! (magit-stage-modified)
                        (magit-commit-create))
        :desc "new commit and push"
        :nv "gcC" (cmd! (magit-stage-modified)
                        (magit-commit-create)
                        (hax/magit-force-push-current))
        ;; This shortcut actually turned out to be a pretty interesting
        ;; addition - it is now /very/ easy to run incremental commentary
        ;; on the changes you do in the code, instead of trying to come up
        ;; with one large commit message at the end.
        :desc "amend commit"
        :nv "gca" (cmd! (magit-stage-modified) (magit-commit-amend))
        ;; For small feature branches it is easier to repeatedly extend the
        ;; same commit to keep the changes small, instead of having to do
        ;; the squash at the very end.
        :desc "extend commit"
        :nv "gce" (cmd! (magit-stage-modified) (magit-commit-extend))
        :desc "force push current to origin"
        :nv "gcP" #'hax/magit-force-push-current))

(after! xref
  (setq-default xref-backend-functions '(etags--xref-backend t)))

(after! keycast
  (define-minor-mode keycast-mode
    "Show current command and its key binding in the mode line."
    :global t
    (if keycast-mode
        (add-hook 'pre-command-hook 'keycast--update t)
      (remove-hook 'pre-command-hook 'keycast-mode-line-update)))

  (setq keycast-substitute-alist
        '((evil-next-line nil nil)
          (evil-previous-line nil nil)
          (evil-forward-char nil nil)
          (evil-backward-char nil nil)
          (ivy-done nil nil)
          (self-insert-command nil nil)))

  (add-to-list 'global-mode-string '("" keycast-mode-line)))

(load! "lang-spelling.el")
(set-popup-rule! "*Telega Root*" :ignore t)

;; (setq
;;  telega-chat-fill-column 60
;;  telega-chat-use-markdown-version 1
;;  ;; telega-chat-input-prompt "****\n"
;;  )





(setq revert-without-query '(".*"))
(global-auto-revert-mode 1)

(defun hax/save-buffers-kill-emacs (&optional arg)
  "Don't prompt for useless shit, learn to clean up after yourself"
  (interactive "P")
  (save-all-buffers)
  (kill-emacs))

(fset 'save-buffers-kill-emacs 'hax/save-buffers-kill-emacs)


(defun hax/telega-mode-hook ()
  (interactive)
  (setq telega-chat-use-markdown-formatting t)
  (spacemacs/toggle-relative-line-numbers-on)
  (map!
   :map telega-msg-button-map
   :nvi "k" nil
   :nvi "l" nil)
  (map!
   :map telega-chat-mode-map
   :nvi "RET" #'electric-newline-and-maybe-indent
   :i "<C-return>" #'telega-chatbuf-input-send
   :i "<f7>" (lambda (text)
               (interactive "scode: ")
               (insert (concat "`" (s-trim text) "` ")))))


(after! hl-todo
  (setq hl-todo-keyword-faces
        '(("TODO" warning bold)
          ("FIXME" error bold)
          ("HACK" font-lock-constant-face bold)
          ("REVIEW" font-lock-keyword-face bold)
          ("NOTE" success bold)
          ("DEPRECATED" font-lock-doc-face bold)
          ("BUG" error bold)
          ("MAYBE" warning bold)
          ("XXX" font-lock-constant-face bold))))

   ;; '(("TODO" . "#dc752f")
   ;;   ("NEXT" . "#dc752f")
   ;;   ("THEM" . "#2d9574")
   ;;   ("PROG" . "#4f97d7")
   ;;   ("OKAY" . "#4f97d7")
   ;;   ("REVIEW" . "#4f97d7")
   ;;   ("IDEA" . "#4f97d7")
   ;;   ("REFACTOR" . "#4f97d7")
   ;;   ("DONT" . "#f2241f")
   ;;   ("DOC" . "#f2241f")
   ;;   ;; ("FAIL" quote hax/face::nuclear)
   ;;   ("ERROR" . "#f2241f")
   ;;   ("TEST" quote hax/face::boxed::orange-bold-boxed)
   ;;   ("WARNING" quote hax/face::boxed::red-bold-boxed)
   ;;   ("IMPLEMENT" . "#f2241f")
   ;;   ("DONE" . "#86dc2f")
   ;;   ("NOTE" quote hax/face::boxed::green-bold-boxed)
   ;;   ("QUESTION" quote hax/face::boxed::dim-yellow-bold-boxed)
   ;;   ("MAYBE" quote hax/face::boxed::dim-yellow-bold-boxed)
   ;;   ("STYLE" quote hax/face::boxed::dim-yellow-bold-boxed)
   ;;   ("KLUDGE" quote hax/face::boxed::dim-yellow-bold-boxed)
   ;;   ("HACK" quote hax/face::boxed::dim-yellow-bold-boxed)
   ;;   ("TEMP" . "#9932cc")
   ;;   ("FIXME" quote hax/face::boxed::orange-bold-boxed)
   ;;   ("XXX" . "#a52a2a")
   ;;   ("XXXX" quote hax/face::boxed::red-bold-boxed)))

(winum-mode t)
