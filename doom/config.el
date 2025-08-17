;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "haxscramper"
      user-mail-address "haxscramper@gmail.com")

(setq shell-file-name (executable-find "sh"))

(setq doom-font (font-spec :family "Iosevka" :size 18)
      doom-unicode-font (font-spec :family "Noto Sans Mono" :size 18)
      doom-serif-font doom-font
      doom-variable-pitch-font doom-font
      doom-theme 'doom-city-lights
      doom-emoji-fallback-font-families '("Apple Color Emoji"
                                          "Segoe UI Emoji"
                                          "Noto Color Emoji"
                                          "Noto Emoji")

      doom-symbol-fallback-font-families '("Segoe UI Symbol"
                                           "Apple Symbols"
                                           "Noto"))

(defun hax/counsel-fonts ()
  "Show a list of all supported font families for a particular frame.

You can insert or kill the name of the selected font."
  (interactive)
  (let ((current-font
         (symbol-name (font-get (face-attribute 'default :font) :family))))
    (ivy-read "Font: " (delete-dups (font-family-list))
              :preselect current-font
              :require-match t
              :history 'counsel-fonts-history
              :caller 'hax/counsel-fonts)))

(defun hax/buffer-face-mode-variable ()
  "Set current buffer fase to a different configuration"
  (interactive)
  (setq buffer-face-mode-face `(:family ,(hax/counsel-fonts)))
  (buffer-face-mode))


(after! unicode-fonts
  ;; Otherwise unicode fonts are not properly set in the configuration, and
  ;; text does not loop properly (uses default fallback consolas)
  (setq doom-unicode-font doom-font))


;; `:ui unicode' does it's own fontset configuration, so I have to hack
;; this in like suggested on issue
;; https://github.com/hlissner/doom-emacs/issues/3298
(defadvice! add-my-font-config (&rest _) :after #'unicode-fonts--setup-1
  ;; Everson mono can be installed from aur as `ttf-everson-mono' (font
  ;; page is https://www.evertype.com/emono/)
  (set-fontset-font "fontset-default" 'mathematical "Everson Mono"))


(setq +file-templates-dir (expand-file-name "templates" (dir!)))

(defun hax/in-snippet-file (path)
  (s-contains? "doom/snippets" path))

(defun hax/init-file-templates()
  (interactive)
  (setq +file-templates-alist '())
  (set-file-template! "/readme\\.org$" :trigger "__readme.org" :mode 'org-mode)
  (set-file-template! "\\.el$" :trigger "__" :mode 'emacs-lisp-mode)
  (set-file-template! "\\.sh$" :trigger "__" :mode 'sh-mode)
  (set-file-template! "\\.pro$" :trigger "__" :mode 'qt-pro-mode)
  (set-file-template! "\\.puml$" :trigger "__" :mode 'plantuml-mode)
  (set-file-template! "\\.cpp$" :trigger "__" :mode 'cpp-mode)
  (set-file-template! "\\.py$" :trigger "__" :mode 'python-mode)
  (set-file-template! "" :when 'hax/in-snippet-file :trigger "__default" :mode 'snippet-mode)
  (set-file-template! "Taskfile\\.yaml$"
    :trigger "__Taskfile.yaml" :mode 'yaml-mode)
  (set-file-template! "CMakeLists\\.txt" :trigger "__" :mode 'cmake-mode)
  (when (boundp 'yas-minor-mode) (when yas-minor-mode (yas-reload-all))))

(hax/init-file-templates)

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
(load! "lib/strings.el")
(load! "lib/indentation.el")
(load! "lib/fontlock.el")
(load! "lib/searches.el")

;; Language-specific configuration files.
(load! "lang-nim.el")
(load! "lang-elvish.el")
(load! "lang-lisp.el")
(load! "lang-eshell.el")
(load! "lang-base.el")
(load! "lang-c.el")
(load! "lang-org.el")
(load! "lang-llm.el")
(load! "lang-plantuml.el")
(load! "lang-latex.el")

(load! "config-editing.el")
(load! "temp-settings.el")

(setq confirm-kill-emacs nil)

(after! evil-mc
  (global-evil-mc-mode t))

(global-company-mode -1)

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

(defun hax/git-commit-hook ()
  (interactive)
  (set-fill-column 75))

(defun hax/buffer-associated-image ()
  "Get image associated with the buffer file, if such image exists"
  (let* ((file (buffer-file-name)))
    (cond
     ((member (f-ext file) '("png" "gif" "svg" "jpg" "jpeg"))
      (buffer-file-name))
     ((f-exists? (f-swap-ext file "png")) (f-swap-ext file "png"))
     ((f-exists? (f-swap-ext file "jpg")) (f-swap-ext file "jpg"))
     ((f-exists? (f-swap-ext file "svg")) (f-swap-ext file "svg"))
     ((f-exists? (f-swap-ext file "jpeg")) (f-swap-ext file "jpeg"))
     ((f-exists? (f-swap-ext file "gif")) (f-swap-ext file "gif"))
     (t (error (format "Could not find image associted with %s" file))))))


(defun hax/open-in-sxiv ()
  "Open image associated with current buffer in the sxiv"
  (interactive)
  (start-process
   "process" "*Sxiv buffer*" "sxiv" (hax/buffer-associated-image)))


(defun hax/open-in-zathura ()
  "Open image associated with current buffer in the zathura pdf reader"
  (interactive)
  (start-process
   "process" "*Zathura buffer*" "zathura"
   (f-swap-ext (buffer-file-name) "pdf")))

(defun hax/list-window-splits ()
  (interactive)
  (let ((result ""))
    (dolist (frame (frame-list))
      (select-frame frame)
      (let ((frame-id (frame-parameter frame 'outer-window-id)))
        (walk-windows
         (lambda (win)
           (let ((edges (window-edges win)))
             (setq result
                   (concat
                    result
                    (format "%s,%d,%d,%d,%d\n"
                            frame-id
                            (nth 0 edges)
                            (nth 1 edges)
                            (nth 2 edges)
                            (nth 3 edges))))))
         nil frame)))
    result))

(defun hax/select-window-split-by-position (frame-id left top right bottom)
  (interactive)
  (let ((target-frame nil)
        (target-window nil))
    (dolist (frame (frame-list))
      (when (string= (frame-parameter frame 'outer-window-id) frame-id)
        (setq target-frame frame)))
    (if (not target-frame)
        (error "Frame with ID %s not found" frame-id)
      (walk-windows
       (lambda (win)
         (let ((edges (window-edges win)))
           (when (and (= (nth 0 edges) left)
                      (= (nth 1 edges) top)
                      (= (nth 2 edges) right)
                      (= (nth 3 edges) bottom))
             (setq target-window win))))
       nil target-frame)
      (if (not target-window)
          (error "Window with edges (%d,%d,%d,%d) not found in frame %s"
                 left top right bottom frame-id)
        (progn
          (message "select OK %s" target-window)
          (select-frame-set-input-focus target-frame)
          (set-frame-selected-window target-frame target-window)
          ;; (select-window target-window)
          )))))

(after! magit
  (setq git-commit-major-mode 'org-mode)
  (add-to-list 'magit-no-confirm 'stage-all-changes)
  (remove-hook 'server-switch-hook 'magit-commit-diff)
  (add-to-list 'git-commit-setup-hook 'hax/git-commit-hook)
  (defun hax/magit-force-push-current ()
    "random documentation string if krux ever wants to get any
more nitpickery about stuff I write in my configuration files."
    (interactive)
    (magit-git-command
     (format "git push origin %s"
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
        :nv "gca" (cmd! (magit-git-command) (magit-commit-amend))
        :desc "amend commit without staging"
        :nv "gcA" (cmd! (magit-commit-amend))
        ;; For small feature branches it is easier to repeatedly extend the
        ;; same commit to keep the changes small, instead of having to do
        ;; the squash at the very end.
        :desc "extend commit"
        :nv "gce" (cmd! (magit-stage-modified) (magit-commit-extend))
        :desc "extend commit and force push"
        :nv "gcE" (cmd!
                   (magit-stage-modified)
                   (magit-git-command
                    (format "git commit --amend --no-edit && git push origin %s"
                            (magit-get-current-branch)))
                   ;; (push 'amend-published magit-no-confirm)
                   ;; (magit-commit-extend)
                   ;; (pop magit-no-confirm)
                   ;; (hax/magit-force-push-current)
                   )
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
          (next-line nil nil)
          (previous-line nil nil)
          (evil-next-visual-line nil nil)
          (evil-previous-visual-line nil nil)
          (evil-forward-char nil nil)
          (evil-backward-char nil nil)
          (ivy-done nil nil)
          (mouse-set-point nil nil)
          (org-self-insert-command nil nil)
          (self-insert-command nil nil)))

  (add-to-list 'global-mode-string '("" keycast-mode-line)))

(load! "lang-spelling.el")
(set-popup-rule! "*Telega Root*" :ignore t)
(set-popup-rule! "eshell" :ignore t :modeline t)
(set-popup-rule! "COMMIT_EDITMSG" :height 0.6)

(defun hax/telega-mode-hook ()
  (interactive)
  (setq
   telega-chat-fill-column 60
   telega-chat-use-markdown-version 1
   telega-chat-input-prompt "****\n")
  (doom/toggle-line-numbers)
  (map!
   :map telega-msg-button-map
   :nvi "k" nil
   :nvi "l" nil)
  (map!
   :map telega-chat-mode-map
   ;; FIXME this breaks 'ret' on the image preview selection
   :nvi [return] #'electric-newline-and-maybe-indent
   :nvi [C-return] #'telega-chatbuf-input-send
   :nvi [f7] (lambda (text)
               (interactive "scode: ")
               (insert (concat "`" (s-trim text) "` ")))))

(after! telega
  (add-hook 'telega-chat-mode-hook 'hax/telega-mode-hook))

(set-formatter! 'html-tidy
  '("xmllint" "--pretty" "2" "-")
  :modes '(nxml-mode))

(set-formatter! 'html-tidy '("hax-htmlfmt.sh") :modes '(html-mode web-mode))

(set-formatter! 'asmfmt
  '("hax-asmfmt.sh")
  :modes '(asm-mode))

(set-formatter! 'shfmt
  '("shfmt"
    "-ci" ;; Switch cases will be indented.
    "-i" "4" ;; Indent: 0 for tabs (default), >0 for number of spaces.
    "-sr" ;; Redirect operators will be followed by a space.
    ("-ln" "%s" (pcase sh-shell (`bash "bash") (`mksh "mksh") (_ "posix")))))

(set-formatter! 'pyfmt
  '("yapf")
  :modes '(python-mode)
  )

(set-formatter! 'cmakefmt
  '("cmake-format"
    "--line-width" "75"
    "--command-case" "lower"
    "--keyword-case" "upper"
    "-")
  :modes '(cmake-mode))


(set-formatter! 'luafmt
  '("luafmt"
    "--stdin"
    "--line-width" "75"
    )
  :modes '(lua-mode))

(set-formatter! 'sqlfmt
  '("sqlformat" "-"
    "--keywords" "upper"
    "--identifiers" "lower"
    "--reindent"
    )
  )

(setq revert-without-query '(".*"))
(global-auto-revert-mode 1)

(defun hax/save-buffers-kill-emacs (&optional arg)
  "Don't prompt for useless shit, learn to clean up after yourself"
  (interactive "P")
  (save-all-buffers)
  (kill-emacs))

(fset 'save-buffers-kill-emacs 'hax/save-buffers-kill-emacs)


(defmacro defface-derive (name base doc &rest content)
  `(defface ,name '((t :inherit ,base ,@content)) ,doc))

(defmacro defface-t (name doc &rest content)
  (list 'defface name (list '\` (list (append (list 't) content))) doc))

(defface-t hl-todo-STYLE "STYLE"
           :foreground ,(doom-color 'yellow)
           :underline t)


(defface-derive hl-todo-IMPORTANT warning "IMPORTANT"
                :weight bold :underline t)
(defface-derive hl-todo-TODO warning "TODO"
                :weight bold :underline t)
(defface-derive hl-todo-FIXME error "FIXME"
                :weight bold :underline t)
(defface-derive hl-todo-HACK font-lock-constant-face "HACK"
                :weight bold :underline t)
(defface-derive hl-todo-REVIEW font-lock-keyword-face "REVIEW"
                :weight bold :underline t)
(defface-derive hl-todo-IDEA success "IDEA"
                :weight bold :underline t)
(defface-derive hl-todo-NOTE success "NOTE"
                :weight bold :underline t)
(defface-derive hl-todo-DONE success "DONE"
                :weight bold :underline t)
(defface-derive hl-todo-DEPRECATED font-lock-doc-face "DEPRECATED"
                :weight bold :underline t)
(defface-derive hl-todo-REFACTOR font-lock-comment-face "REFACTOR"
                :underline t)
(defface-derive hl-todo-BUG error "BUG"
                :weight bold :underline t)
(defface-derive hl-todo-MAYBE warning "MAYBE"
                :weight bold :underline t)
(defface-derive hl-todo-XXX font-lock-constant-face "XXX"
                :weight bold :underline t)

(defface-derive hl-todo-IMPLEMENT warning "IMPLEMENT"
                :underline t :slant italic)

(defface-derive hl-todo-DOC hl-todo-TODO "DOC")
(defface-derive hl-todo-ERROR error "ERROR")
(defface-derive hl-todo-WARNING warning "WARNING" :underline t :slant italic :overline t)
(defface-derive hl-todo-QUESTION warning "WARNING" :underline t :slant italic)
(defface-derive hl-todo-TEMP hl-todo-IDEA "TEMP")
(defface-derive hl-todo-NEXT hl-todo-TODO "NEXT" :foreground "red")
(defface-derive hl-todo-TEST hl-todo-TODO "TEST")


(after! hl-todo
  (setq hl-todo-keyword-faces
        '(("TODO" . hl-todo-TODO)
          ("DONE" . hl-todo-DONE)
          ("DOC" . hl-todo-DOC)
          ("FIXME" . hl-todo-FIXME)
          ("HACK" . hl-todo-HACK)
          ("IDEA" . hl-todo-IDEA)
          ("NOTE" . hl-todo-NOTE)
          ("DEPRECATED" . hl-todo-DEPRECATED)
          ("REFACTOR" . hl-todo-REFACTOR)
          ("STYLE" . hl-todo-STYLE)
          ("BUG" . hl-todo-BUG)
          ("XXX" . hl-todo-XXX)
          ("XXXX" . hl-todo-XXX)
          ("xxx" . hl-todo-XXX)
          ("IMPLEMENT" . hl-todo-IMPLEMENT)
          ("IMPORTANT" . hl-todo-IMPORTANT)
          ("QUESTION" . hl-todo-QUESTION)
          ("TEST" . hl-todo-TEST)
          ("WARNING" . hl-todo-WARNING)
          ("ERROR" . hl-todo-ERROR)
          ("TEMP" . hl-todo-TEMP))))

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


(defun hax/ace-delete-window (&optional arg)
  "Ace delete window. If the universal prefix argument is used
then kill the buffer too."
  (interactive "P")
  (require 'ace-window)
  (aw-select
   " Ace - Delete Window"
   (lambda (window)
     (when (equal '(4) arg)
       (with-selected-window window
         (spacemacs/kill-this-buffer arg)))
     (aw-delete-window window))))

(setq aw-dispatch-always t)

(map!
 :leader
 :nv "w d"  #'hax/ace-delete-window
 )

(use-package! eaf
  :load-path "~/.emacs.d/site-lisp/emacs-application-framework/"
  :init
  :custom
  (eaf-browser-continue-where-left-off t)
  (eaf-browser-enable-adblocker t)
  ;; (browse-url-browser-function 'eaf-open-browser) ;; Make EAF Browser my default browser
  :config
  (defalias 'browse-web #'eaf-open-browser)

  (require 'eaf-file-manager)
  (require 'eaf-markdown-previewer)
  (require 'eaf-pdf-viewer)
  (require 'eaf-browser)
  ;; (when (display-graphic-p)
  ;;   (require 'eaf-all-the-icons))

  (require 'eaf-evil)
  (define-key key-translation-map (kbd "SPC")
              (lambda (prompt)
                (if (derived-mode-p 'eaf-mode)
                    (pcase eaf--buffer-app-name
                      ("browser" (if  (string= (eaf-call-sync "call_function" eaf--buffer-id "is_focus") "True")
                                     (kbd "SPC")
                                   (kbd eaf-evil-leader-key)))
                      ("pdf-viewer" (kbd eaf-evil-leader-key))
                      ("image-viewer" (kbd eaf-evil-leader-key))
                      (_  (kbd "SPC")))
                  (kbd "SPC")))))
