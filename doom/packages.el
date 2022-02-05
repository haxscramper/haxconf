;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; Enumerate open windows, allow to jump to a window by name
(package! winum)

(package! doom-themes)

;: List handing library
(package! dash)

;; Nim mode for emacs, using krux fork because it is more usable
(package! nim-mode :recipe (:host github :repo "krux02/nim-mode"))


;; underscore -> UPCASE -> CamelCase conversion of names
(package! string-inflection)

(package! elvish-mode)
