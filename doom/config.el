;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "haxscramper"
      user-mail-address "haxscramper@gmail.com")

(setq doom-font (font-spec
                 :family "FiraCode"
                 :size 18)

      doom-variable-pitch-font (font-spec
                                :family "sans"
                                :size 13))

(setq +file-templates-dir (expand-file-name "templates" (dir!)))
(set-file-templates!
 '(("/readme\\.org$" :trigger "__readme.org" :mode 'org-mode)
   ("*\\.sh$" :trigger "__" :mode 'sh-mode)))


(defun tree-repr (item)
  "Return tree representation of the item's structure"
  (let ((tyof (type-of item))
        (name (symbol-name tyof)))
    (case tyof
      ("vector" "vector")
      (t name))))

(defun hax/c++-rotate-infix ()
  "Rotate arguments of the current infix expression"
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

(load! "config-editing.el")

(setq confirm-kill-emacs nil)


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
        :desc "new commit and push"
        :nv "gcc" (cmd! (magit-stage-modified)
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

(setq save-abbrevs 'silent)

(setq flycheck-disabled-checkers '(emacs-lisp-checkdoc))

(after! spell-fu
  (setq spell-fu-idle-delay 0.5 ; default is 0.25
        ispell-program-name "hunspell"
        ;; aspell -> "--sug-mode=ultra"
        ;;ispell-extra-args '("-d en_US")
        ispell-dictionary "en_US" ; needed for MacOS in particular
        ispell-personal-dictionary "~/.aspell.en.pws" ; standard location
        spell-fu-dictionary "~/.config/dict") ; standard location

  ;; use American English as ispell default dicTionary
  (ispell-change-dictionary "american" t)
  (setq spell-fu-faces-exclude
        '(org-block-begin-line
          org-block-end-line
          org-code
          org-date
          org-drawer org-document-info-keyword
          org-ellipsis
          org-link
          org-meta-line
          org-properties
          org-properties-value
          org-special-keyword
          org-src
          org-tag
          org-verbatim
          rst-external
          rst-literal
          rst-reference
          sphinx-code-block-face
          font-lock-string-face))
  (setq-default ispell-program-name "hunspell"))

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
