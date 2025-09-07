;;; -*- lexical-binding: t; -*-

(setq scroll-margin 2)
(setq whitespace-line-column 75)
(setq-default fill-column 75)
(global-visual-line-mode t)

(after! olivetti
  (setq olivetti-body-width (+ 75 7)))

;; Don't account for folded section lines
(setq display-line-numbers-type 'visual)
;; Editing operations such as deleting work only on parts of the word instead of
;; cutting off whole identifier at once.
;; https://www.gnu.org/software/emacs/manual/html_node/ccmode/Subword-Movement.html
(global-subword-mode t)

;; https://tecosaur.github.io/emacs-config/config.html#company
(after! company
  (setq company-idle-delay 0.2
        company-minimum-prefix-length 2)
  (setq company-show-numbers t)
  (add-hook 'evil-normal-state-entry-hook #'company-abort))


(setq-default history-length 1000)
(setq-default prescient-history-length 1000)

;; Disable prompts from the file-local variables by default.
(setq enable-local-variables :all)

(map!
 ;; M-> or M-< to indent code blocks in all modes
 :nv "M->" (cmd! (code-indent-shift-right
                  (get-selected-region-start)
                  (get-selected-region-end) 2))
 :nv "M-<" (cmd! (code-indent-shift-left
                  (get-selected-region-start)
                  (get-selected-region-end) 2))
 :nv [s-pause] (cmd! (shell-command "entr-space.sh")))

;; Text case conversion

(use-package! string-inflection
  :commands (string-inflection-all-cycle
             string-inflection-toggle
             string-inflection-camelcase
             string-inflection-lower-camelcase
             string-inflection-kebab-case
             string-inflection-underscore
             string-inflection-capital-underscore
             string-inflection-upcase)
  :init
  (map! :leader :prefix ("c~" . "naming convention")
        :desc "cycle" "~" #'string-inflection-all-cycle
        :desc "toggle" "t" #'string-inflection-toggle
        :desc "CamelCase" "c" #'string-inflection-camelcase
        :desc "downCase" "d" #'string-inflection-lower-camelcase
        :desc "kebab-case" "k" #'string-inflection-kebab-case
        :desc "under_score" "_" #'string-inflection-underscore
        :desc "Upper_Score" "u" #'string-inflection-capital-underscore
        :desc "UP_CASE" "U" #'string-inflection-upcase)
  (after! evil
    (evil-define-operator evil-operator-string-inflection (beg end _type)
      "Define a new evil operator that cycles symbol casing."
      :move-point nil
      (interactive "<R>")
      (string-inflection-all-cycle)
      (setq evil-repeat-info '([?g ?~])))
    (define-key evil-normal-state-map (kbd "g~") 'evil-operator-string-inflection)))

;; Highlight symbol under cursor using saner keybinding
(map! :nv ",hs" 'highlight-symbol)

(defun current-line-indent ()
  (interactive)
  (save-excursion (back-to-indentation) (current-column)))

(defun previous-line-indent ()
  (interactive)
  (save-excursion (forward-line -1) (current-column)))

(cl-defun newline-and-indent-same-level (&optional
                                         (indent-addition nil)
                                         (goto-eol nil))
  "Insert a newline, then indent to the same column as the current line."
  (interactive)
  (when goto-eol (goto-char (line-end-position)))
  (let ((col (save-excursion
               (back-to-indentation)
               (current-column))))
    (newline)
    (indent-to-column (if indent-addition
                          (+ indent-addition col)
                        col))))

(map! :i "<C-return>" 'newline-and-indent-same-level)

;; Modified answer from http://emacs.stackexchange.com/questions/47/align-vertical-columns-of-numbers-on-the-decimal-point
(defun align-repeat (start end regexp &optional justify-right after)
  "Repeat alignment with respect to the given regular expression.
If JUSTIFY-RIGHT is non nil justify to the right instead of the
left. If AFTER is non-nil, add whitespace to the left instead of
the right."
  (interactive "r\nsAlign regexp: ")
  (let* ((ws-regexp (if (string-empty-p regexp)
                        "\\(\\s-+\\)"
                      "\\(\\s-*\\)"))
         (complete-regexp (if after
                              (concat regexp ws-regexp)
                            (concat ws-regexp regexp)))
         (group (if justify-right -1 1)))

    (unless (use-region-p)
      (save-excursion
        (while (and
                (string-match-p complete-regexp (thing-at-point 'line))
                (= 0 (forward-line -1)))
          (setq start (point-at-bol))))
      (save-excursion
        (while (and
                (string-match-p complete-regexp (thing-at-point 'line))
                (= 0 (forward-line 1)))
          (setq end (point-at-eol)))))

    (align-regexp start end complete-regexp group 1 t)))

(defun align-repeat-decimal (start end)
  "Align a table of numbers on decimal points and dollar signs (both optional)"
  (interactive "r")
  (require 'align)
  (align-region start end nil
                '((nil (regexp . "\\([\t ]*\\)\\$?\\([\t ]+[0-9]+\\)\\.?")
                   (repeat . t)
                   (group 1 2)
                   (spacing 1 1)
                   (justify nil t)))
                nil))

(defmacro create-align-repeat-x (name regexp &optional justify-right default-after)
  "Helper macro to declare alignment functions for different regex patterns"
  (let* ((new-func (intern (concat "align-repeat-" name)))
         (new-func-defn
          `(defun ,new-func (start end switch)
             (interactive "r\nP")
             (let ((after (not (eq (if switch t nil) (if ,default-after t nil)))))
               (align-repeat start end ,regexp ,justify-right after)))))
    (put new-func 'function-documentation "Created by `create-align-repeat-x'.")
    new-func-defn))

(create-align-repeat-x "comma" "," nil t)
(create-align-repeat-x "semicolon" ";" nil t)
(create-align-repeat-x "colon" ":" nil t)
(create-align-repeat-x "equal" "=")
(create-align-repeat-x "math-oper" "[+\\-*/]")
(create-align-repeat-x "percent" "%")
(create-align-repeat-x "ampersand" "&")
(create-align-repeat-x "bar" "|")
(create-align-repeat-x "left-paren" "(")
(create-align-repeat-x "right-paren" ")" t)
(create-align-repeat-x "left-curly-brace" "{")
(create-align-repeat-x "right-curly-brace" "}" t)
(create-align-repeat-x "left-square-brace" "\\[")
(create-align-repeat-x "right-square-brace" "\\]" t)
(create-align-repeat-x "backslash" "\\\\")
(create-align-repeat-x "quote" "\"")
(create-align-repeat-x "hash" "#+")


(map!
 :v ",a," #'align-repeat-comma
 :v ",a:" #'align-repeat-colon
 :v ",a;" #'align-repeat-semicolon
 :v ",a]" #'align-repeat-right-square-brace
 :v ",a}" #'align-repeat-right-curly-brace
 :v ",a)" #'align-repeat-right-paren
 :v ",a[" #'align-repeat-left-square-brace
 :v ",a{" #'align-repeat-left-curly-brace
 :v ",a(" #'align-repeat-left-paren
 :v ",a=" #'align-repeat-equal
 :v ",a#" #'align-repeat-hash
 :v ",a'" #'align-repeat-quote
 )

(defun set-local-abbrevs (abbrevs)
  "Add ABBREVS to `local-abbrev-table' and make it buffer local.
     ABBREVS should be a list of abbrevs as passed to `define-abbrev-table'.
     The `local-abbrev-table' will be replaced by a copy with the new
     abbrevs added, so that it is not the same as the abbrev table used
     in other buffers with the same `major-mode'."
  (let* ((bufname (buffer-name))
         (prefix (substring (md5 bufname) 0 (length bufname)))
         (tblsym (intern (concat prefix "-abbrev-table"))))
    (set tblsym (copy-abbrev-table local-abbrev-table))
    (dolist (abbrev abbrevs)
      (define-abbrev (eval tblsym)
        (car abbrev)
        (cadr abbrev)
        (caddr abbrev)))
    (setq-local local-abbrev-table (eval tblsym))))



;; Claude, please write a code that un-fucks default emacs behavior that
;; makes absolutely no sense in any context whatsoever, but that somehow
;; made its way into other configurations. I don't understand, is it too
;; much to ask for the editor to not shit into the clipboard each time I'm
;; doing backspace or something?

(defun hax/delete-backward-char-no-kill (&optional n)
  "Delete N characters backward without adding to kill ring.
Works in all contexts including minibuffer and prompts."
  (interactive "p")
  (let ((n (or n 1)))
    (delete-char (- n))))

(defun hax/delete-forward-char-no-kill (&optional n)
  "Delete N characters forward without adding to kill ring."
  (interactive "p")
  (let ((n (or n 1)))
    (delete-char n)))

(defun hax/backward-delete-word-no-kill (&optional n)
  "Delete word backward without adding to kill ring."
  (interactive "p")
  (let ((n (or n 1)))
    (dotimes (_ n)
      (let ((start (point)))
        (backward-word 1)
        (delete-region (point) start)))))

(defun hax/delete-word-no-kill (&optional n)
  "Delete word forward without adding to kill ring."
  (interactive "p")
  (let ((n (or n 1)))
    (dotimes (_ n)
      (let ((start (point)))
        (forward-word 1)
        (delete-region start (point))))))

(defun hax/delete-line-backward-no-kill ()
  "Delete from point to beginning of line without adding to kill ring."
  (interactive)
  (delete-region (line-beginning-position) (point)))

(defun hax/delete-line-forward-no-kill ()
  "Delete from point to end of line without adding to kill ring."
  (interactive)
  (delete-region (point) (line-end-position)))

;; Override backspace in all keymaps
(defun hax/setup-no-kill-delete-keys ()
  (interactive)
  "Set up delete keys that don't add to kill ring in all contexts."

  ;; Global bindings
  (global-set-key (kbd "DEL") #'hax/delete-backward-char-no-kill)
  (global-set-key (kbd "<backspace>") #'hax/delete-backward-char-no-kill)
  (global-set-key (kbd "C-<backspace>") #'hax/backward-delete-word-no-kill)
  (global-set-key (kbd "M-<backspace>") #'hax/backward-delete-word-no-kill)
  (global-set-key (kbd "M-DEL") #'hax/backward-delete-word-no-kill)

  ;; Optional: Also override forward delete keys
  (global-set-key (kbd "<delete>") #'hax/delete-forward-char-no-kill)
  (global-set-key (kbd "M-d") #'hax/delete-word-no-kill)
  (global-set-key (kbd "C-k") #'hax/delete-line-forward-no-kill)
  (global-set-key (kbd "C-u") #'hax/delete-line-backward-no-kill)

  ;; Minibuffer specific
  (define-key minibuffer-local-map (kbd "DEL") #'hax/delete-backward-char-no-kill)
  (define-key minibuffer-local-map (kbd "<backspace>") #'hax/delete-backward-char-no-kill)
  (define-key minibuffer-local-map (kbd "C-<backspace>") #'hax/backward-delete-word-no-kill)
  (define-key minibuffer-local-map (kbd "M-<backspace>") #'hax/backward-delete-word-no-kill)
  (define-key minibuffer-local-map (kbd "M-DEL") #'hax/backward-delete-word-no-kill)

  ;; Completion maps
  (with-eval-after-load 'minibuffer
    (define-key minibuffer-local-completion-map (kbd "DEL") #'hax/delete-backward-char-no-kill)
    (define-key minibuffer-local-completion-map (kbd "<backspace>") #'hax/delete-backward-char-no-kill)
    (define-key minibuffer-local-must-match-map (kbd "DEL") #'hax/delete-backward-char-no-kill)
    (define-key minibuffer-local-must-match-map (kbd "<backspace>") #'hax/delete-backward-char-no-kill))

  ;; Read-string and similar prompts
  (define-key read-expression-map (kbd "DEL") #'hax/delete-backward-char-no-kill)
  (define-key read-expression-map (kbd "<backspace>") #'hax/delete-backward-char-no-kill)

  ;; Ivy (if you use it)
  (with-eval-after-load 'ivy
    (define-key ivy-minibuffer-map (kbd "DEL") #'hax/delete-backward-char-no-kill)
    (define-key ivy-minibuffer-map (kbd "<backspace>") #'hax/delete-backward-char-no-kill)
    (define-key ivy-minibuffer-map (kbd "M-DEL") #'hax/backward-delete-word-no-kill))

  ;; Helm (if you use it)
  (with-eval-after-load 'helm
    (define-key helm-map (kbd "DEL") #'hax/delete-backward-char-no-kill)
    (define-key helm-map (kbd "<backspace>") #'hax/delete-backward-char-no-kill)
    (define-key helm-map (kbd "M-DEL") #'hax/backward-delete-word-no-kill))

  ;; Company completion (if you use it)
  (with-eval-after-load 'company
    (define-key company-active-map (kbd "DEL") #'hax/delete-backward-char-no-kill)
    (define-key company-active-map (kbd "<backspace>") #'hax/delete-backward-char-no-kill))

  ;; Vertico (if you use it)
  (with-eval-after-load 'vertico
    (define-key vertico-map (kbd "DEL") #'hax/delete-backward-char-no-kill)
    (define-key vertico-map (kbd "<backspace>") #'hax/delete-backward-char-no-kill)
    (define-key vertico-map (kbd "M-DEL") #'hax/backward-delete-word-no-kill)))

;; Hook to ensure these bindings work in all new buffers
(defun hax/setup-buffer-delete-keys ()
  "Set up delete keys for current buffer."
  (local-set-key (kbd "DEL") #'hax/delete-backward-char-no-kill)
  (local-set-key (kbd "<backspace>") #'hax/delete-backward-char-no-kill)
  (local-set-key (kbd "C-<backspace>") #'hax/backward-delete-word-no-kill)
  (local-set-key (kbd "M-<backspace>") #'hax/backward-delete-word-no-kill)
  (local-set-key (kbd "M-DEL") #'hax/backward-delete-word-no-kill))

;; Apply to all new buffers
(add-hook 'after-change-major-mode-hook #'hax/setup-buffer-delete-keys)
(add-hook 'minibuffer-setup-hook #'hax/setup-buffer-delete-keys)

;; Special handling for eval-expression and other read functions
(defun hax/advice-read-functions (orig-fun &rest args)
  "Advice to ensure our delete keys work in read functions."
  (let ((minibuffer-local-map (copy-keymap minibuffer-local-map)))
    (define-key minibuffer-local-map (kbd "DEL") #'hax/delete-backward-char-no-kill)
    (define-key minibuffer-local-map (kbd "<backspace>") #'hax/delete-backward-char-no-kill)
    (define-key minibuffer-local-map (kbd "M-DEL") #'hax/backward-delete-word-no-kill)
    (apply orig-fun args)))

;; Apply advice to common read functions
(advice-add 'read-string :around #'hax/advice-read-functions)
(advice-add 'read-from-minibuffer :around #'hax/advice-read-functions)

;; Initialize the setup
(hax/setup-no-kill-delete-keys)

;; Optional: If you want to completely disable kill-ring for delete operations
;; you can also override the functions that add to kill-ring
(defun hax/disable-kill-ring-advice (orig-fun &rest args)
  "Advice to prevent functions from adding to kill-ring during delete operations."
  (let ((kill-ring kill-ring)
        (kill-ring-yank-pointer kill-ring-yank-pointer))
    (cl-letf (((symbol-function 'kill-new) (lambda (&rest _) nil))
              ((symbol-function 'kill-append) (lambda (&rest _) nil)))
      (apply orig-fun args))))

;; Uncomment these if you want even more aggressive kill-ring prevention
;; (advice-add 'backward-delete-char-untabify :around #'hax/disable-kill-ring-advice)
;; (advice-add 'delete-backward-char :around #'hax/disable-kill-ring-advice)
