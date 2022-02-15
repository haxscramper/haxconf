;;; init.el -*- lexical-binding: t; -*-

;; This file controls what Doom modules are enabled and what order they load
;; in. Remember to run 'doom sync' after modifying it!

;; NOTE Press 'SPC h d h' (or 'C-h d h' for non-vim users) to access Doom's
;;      documentation. There you'll find a "Module Index" link where you'll find
;;      a comprehensive list of Doom's modules and what flags they support.

;; NOTE Move your cursor over a module's name (or its flags) and press 'K' (or
;;      'C-c c k' for non-vim users) to view its documentation. This works on
;;      flags as well (those symbols that start with a plus).
;;
;;      Alternatively, press 'gd' (or 'C-c c d') on a module to browse its
;;      directory (for easy access to its source code).

(doom! :input
       :completion company (ivy +prescient -childframe +fuzzy +icons)
       :ui
       ;;deft              ; notational velocity for Emacs
       doom              ; what makes DOOM look the way it does
       doom-dashboard    ; a nifty splash screen for Emacs
       (emoji +unicode)  ; 🙂
       hl-todo           ; highlight TODO/FIXME/NOTE/DEPRECATED/HACK/REVIEW
       indent-guides     ; highlighted indent columns
       ligatures         ; ligatures and symbols to make your code pretty again
       modeline          ; snazzy, Atom-inspired modeline, plus API
       ophints           ; highlight the region an operation acts on
       (popup +defaults) ; tame sudden yet inevitable temporary windows
       unicode           ; extended unicode support for various languages
       vc-gutter         ; vcs diff in the fringe
       vi-tilde-fringe   ; fringe tildes to mark beyond EOB
       zen               ; distraction-free coding or writing
       :editor (evil +everywhere) file-templates fold multiple-cursors
       ;;objed             ; text object editing for the innocent
       ;;rotate-text       ; cycle region at point between text candidates
       word-wrap         ; soft wrapping with language-aware indent

       :emacs dired electric ibuffer undo vc
       :term eshell

       ;; Use hunspell and check for spelling and grammar mistakes in the
       ;; code comments as well
       :checkers syntax (spell +everywhere) (grammar +langtool)

       :tools (eval +overlay) gist lookup magit pdf
       :os tty
       :lang cc emacs-lisp json latex markdown org python qt rst sh yaml

       :config
       ;;literate
       (default +bindings +smartparens))
