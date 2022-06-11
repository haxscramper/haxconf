;; -*- lexical-binding: t; -*-

;; https://archive.baty.net/2020/respecting-visual-lines-in-doom-emacs/
(use-package-hook! evil :pre-init
  ;; sane j and k behavior
  (setq evil-respect-visual-line-mode t)
  t)


(doom! :input
       :completion company (ivy +prescient -childframe +fuzzy +icons)
       :ui
       ;; Main editor/window UI look
       doom doom-dashboard modeline
       hl-todo
       ophints
       ;; More manageable popup windows
       (popup
        ;; Make org-node notes, code editing buffers also respect the popup
        ;; rules.
        +all)
       ;; Small UX improvements for unicode/emoji/EOL formatting
       (emoji +unicode) unicode vc-gutter vi-tilde-fringe
       zen
       :editor (evil +everywhere) file-templates fold multiple-cursors snippets
       ;; Formatting configuration for different modes is configured
       ;; elsewhere. Visual word wrapping
       (format +onsave) word-wrap

       :emacs dired electric ibuffer undo vc
       :term eshell

       :tools (eval +overlay) gist (lookup +dictionary +offline) magit pdf lsp
       :os tty
       :lang
       cc qt sh
       arduino
       lua
       emacs-lisp
       ;; Configuration, serialization
       json yaml xml
       ;; Markup languages
       latex markdown rst (org +dragndrop)
       python

       :config (default +bindings +smartparens))

(when doom-debug-p
  (require 'benchmark-init)
  (add-hook 'doom-first-input-hook #'benchmark-init/deactivate))
