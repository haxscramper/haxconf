;; Configuration for general spellchecking

(setq save-abbrevs 'silent)
(setq flycheck-disabled-checkers '(emacs-lisp-checkdoc))
;; Spell-fu checking can run automatically as I type, but all other
;; corrections are better done when I actually think I finished the chunk
;; of typing. Setting large idle delay is possible, but saving is easier to
;; control.
(setq flycheck-check-syntax-automatically '(save mode-enabled))

(after! flyspell
  (setq
   ispell-program-name "hunspell"
   aspell-program-name "hunspell"
   ispell-personal-dictionary "~/.config/dict"
   ;; Setup for the multiple languages (in that case english and russian -
   ;; config taken from )https://www.linux.org.ru/forum/general/13940489
   ispell-dictionary "ru_RU,en_US")
  (ispell-set-spellchecker-params)
  ;; Note the ordering in the multi-element dictionary must be the same as
  ;; `ispell-dictionary'
  (ispell-hunspell-add-multi-dic "ru_RU,en_US")

  ;; The personal dictionary file has to exist, otherwise hunspell will
  ;; silently not use it.
  (unless (file-exists-p ispell-personal-dictionary)
    (write-region "" nil ispell-personal-dictionary nil 0))


  (defun frog-menu-flyspell-correct (candidates word)
    "Run `frog-menu-read' for the given CANDIDATES.

List of CANDIDATES is given by flyspell for the WORD.

Return selected word to use as a replacement or a tuple
of (command . word) to be used by `flyspell-do-correct'."
    (let* ((verbatim-word (s-wrap word "=" "="))
           (internal-link-word (s-wrap word "[[" "]]"))
           (code-word (s-wrap word "~" "~"))
           (corrects (if flyspell-sort-corrections
                         (sort candidates 'string<)
                       candidates))
           (actions `(("C-s" "Save word"         (save    . ,word))
                      ("C-a" "Accept (session)"  (session . ,word))
                      ("C-b" "Accept (buffer)"   (buffer  . ,word))
                      ("C-=" "Make verbatim"     ,verbatim-word)
                      ("C-`" "Make code"         ,code-word)
                      ("C-]" "Make link"         ,internal-link-word)
                      ("C-c" "Skip"              (skip    . ,word))))
           (prompt   (format "Dictionary: [%s]"  (or ispell-local-dictionary
                                                     ispell-dictionary
                                                     "default")))
           (res      (frog-menu-read prompt corrects actions)))
      (unless res
        (error "Quit"))
      res))

  (setq flyspell-correct-interface #'frog-menu-flyspell-correct)
  ;; (set-face-attribute 'frog-menu-posframe-background-face nil :background (doom-color 'bg))
  ;; (set-face-attribute 'frog-menu-prompt-face nil :background (doom-color 'fg))
  )
