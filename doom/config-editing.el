;;; -*- lexical-binding: t; -*-

(setq scroll-margin 2)
(setq whitespace-line-column 75)
(setq-default fill-column 75)
(global-visual-line-mode t)

;; Don't account for folded section lines
(setq display-line-numbers-type 'visual)
;; Editing operations such as deleting work only on parts of the word instead of
;; cutting off whole identifier at once.
;; https://www.gnu.org/software/emacs/manual/html_node/ccmode/Subword-Movement.html
(global-subword-mode t)

;; https://tecosaur.github.io/emacs-config/config.html#company
(after! company
  (setq company-idle-delay 0
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
