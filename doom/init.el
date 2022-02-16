;;; -*- lexical-binding: t; -*-
(doom! :input
       :completion company (ivy +prescient -childframe +fuzzy +icons)
       :ui
       doom doom-dashboard
       (emoji +unicode)
       hl-todo
       indent-guides
       modeline
       ophints           ; highlight the region an operation acts on
       (popup +defaults) ; tame sudden yet inevitable temporary windows
       unicode
       treemacs
       vc-gutter
       vi-tilde-fringe   ; fringe tildes to mark beyond EOB
       zen
       :editor (evil +everywhere) file-templates fold multiple-cursors
       objed
       word-wrap

       :emacs dired electric ibuffer undo vc
       :term eshell

       ;; Use hunspell and check for spelling and grammar mistakes in the
       ;; code comments as well
       :checkers syntax (spell +everywhere) (grammar +langtool)

       :tools (eval +overlay) gist lookup magit pdf
       :os tty
       :lang cc emacs-lisp json latex markdown python qt rst sh yaml
       (org +dragndrop)

       :config
       ;;literate
       (default +bindings +smartparens))

(when doom-debug-p
  (require 'benchmark-init)
  (add-hook 'doom-first-input-hook #'benchmark-init/deactivate))
