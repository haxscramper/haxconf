;; Configuration for general spellchecking

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
