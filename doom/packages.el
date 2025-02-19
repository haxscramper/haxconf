;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; Enumerate open windows, allow to jump to a window by name
(package! winum)

(package! doom-themes)

;; List handing library
(package! dash)

;; ;; I need the latest version of the package
;; (package! spell-fu :recipe (:host gitlab :repo "ideasman42/emacs-spell-fu"))

;; Nim mode for emacs, using krux fork because it is more usable
(package! nim-mode :recipe (:host github :repo "krux02/nim-mode"))

(package! spell-fu :pin "8185467b24f05bceb428a0e9909651ec083cc54e")

;; Using `:lang nim' in the `init.el' would've added nim support for
;; org-mode automatically, but here I have to configure this from scratch.
(package! ob-nim)
(package! ob-mermaid)

;; underscore -> UPCASE -> CamelCase conversion of names
(package! string-inflection)

;; Using `:editor snippets' implicitly adds to company-mode backends and I
;; could not figure out a way how to properly disable this - it is easier
;; to just install the package separately, and maybe enable it in certain
;; modes as I find appropriate.
(package! yasnippet)

;; Elvish shell configuration files and scripts editing
(package! elvish-mode)

;; automatic and manual symbol highlighting,
;; https://github.com/nschum/highlight-symbol.el
(package! highlight-symbol)

(package! keycast)

;; Used only in lisp mode, but this is the best solution I've found so far
;; for keeping the code at least somewhat formatted. Maybe in the future it
;; would be easier to just use tree-sitter and write own code layouter.
(package! aggressive-indent-mode)

;; At some point I will have to check which of the 9k+ packages that I plan
;; to install eventually slows everything to a crawl. I didn't dive too
;; deep into other configuration possibilities, just used first suggestion
;; I found:
;; https://github.com/hlissner/doom-emacs/issues/4498#issuecomment-753692913
(package! benchmark-init)

;; I could not configure built-in spellchecking via spellfu, so using
;; old-fashioned flyspell - at least it works properly.
(package! flyspell)

;; Editing vim configuration files
(package! vimrc-mode)

;; Better interfaces for spellcheck correction and candidate selection
(package! flyspell-correct)
(package! consult-flyspell
  :recipe (:type git :host gitlab :repo "OlMon/consult-flyspell"
           :branch "master"))

(package! uuidgen
  :recipe (:type git :host github :repo "kanru/uuidgen-el"
           :branch "master"))

(package! evil-surround
  :recipe (:type git :host github :repo "emacs-evil/evil-surround"
           :branch "master"))

(package! lorem-ipsum)
(package! origami)

;; Easily setup frame with multiple choice selection at the `(point)'
;; location.
(package! frog-menu)

;; https://github.com/abo-abo/org-download
(package! org-download)

(package! simpleclip)

(package! graphviz-dot-mode)

;; https://alhassy.github.io/org-special-block-extras/
(package! org-special-block-extras)

;; Window deletion and selection
(package! ace-window)

(package! ox-epub)
(package! ox-json)
(package! org-ref)

(package! mini-frame)
(package! org-ql)

(package! ts)
;; https://github.com/alphapapa/org-sidebar

(package! olivetti)
(package! elisp-format)

(package! sdcv)
(package! mw-thesaurus)
(package! ox-gfm)
(package! ox-rst)

(package! mpv)

(package! org-appear)
(package! mermaid-mode)
