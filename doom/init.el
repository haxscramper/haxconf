                                        ; ;; -*- lexical-binding: t; -*-
(doom! :input
       :completion company (ivy +prescient -childframe +fuzzy +icons)
       :ui
       ;; Main editor/window UI look
       doom doom-dashboard modeline indent-guides
       hl-todo
       ophints
       ;; More manageable popup windows
       (popup
        ;; Make org-node notes, code editing buffers also respect the popup
        ;; rules.
        +all)
       ;; Small UX improvements for unicode/emoji/EOL formatting
       (emoji +unicode) unicode treemacs vc-gutter vi-tilde-fringe
       zen
       :editor (evil +everywhere) file-templates fold multiple-cursors snippets
       ;; Formatting configuration for different modes is configured
       ;; elsewhere. Visual word wrapping
       (format +onsave) word-wrap

       :emacs dired electric ibuffer undo vc
       :term eshell

       ;; Use hunspell and check for spelling and grammar mistakes in the
       ;; code comments as well
       :checkers syntax (spell +everywhere) (grammar +langtool)

       :tools (eval +overlay) gist lookup magit pdf lsp
       :os tty
       :lang
       cc qt sh
       emacs-lisp
       ;; Configuration, serialization
       json yaml
       ;; Markup languages
       latex markdown rst (org +dragndrop)
       (python +lsp)

       :config (default +bindings +smartparens))

(when doom-debug-p
  (require 'benchmark-init)
  (add-hook 'doom-first-input-hook #'benchmark-init/deactivate))
