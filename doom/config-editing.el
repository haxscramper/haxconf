;;; -*- lexical-binding: t; -*-

(setq whitespace-line-column 75)
(setq-default fill-column 75)
(setq display-line-numbers-type 'relative)
(global-visual-line-mode t)

;; Editing operations such as deleting work only on parts of the word instead of
;; cutting off whole identifier at once.
;; https://www.gnu.org/software/emacs/manual/html_node/ccmode/Subword-Movement.html
(global-subword-mode t)

;; https://tecosaur.github.io/emacs-config/config.html#company
(after! company
  (setq company-idle-delay 0
        company-minimum-prefix-length 2)
  (setq company-show-numbers t)
  (company-ctags-auto-setup)
  (add-hook 'evil-normal-state-entry-hook #'company-abort))


(setq-default history-length 1000)
(setq-default prescient-history-length 1000)

;; Disable prompts from the file-local variables by default.
(setq enable-local-variables :all)

(map!
 ;; M-> or M-< to indent code blocks in all modes
 :nv "M->" (lambda () (interactive)
             (code-indent-shift-right (get-selected-region-start) (get-selected-region-end) 2))
 :nv "M-<" (lambda () (interactive)
             (code-indent-shift-left (get-selected-region-start) (get-selected-region-end) 2))
 )

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
