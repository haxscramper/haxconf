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

;; Sometimes regex is not enough. I don't really use folding, but I /do/
;; plan to use this for various code manipulation magic.
(package! tree-sitter)
(package! tree-sitter-langs)

(package! flyspell)

;; Editing vim configuration files
(package! vimrc-mode)

;; Emacs as a telegram client
(package! telega)
