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
    (write-region "" nil ispell-personal-dictionary nil 0)))

