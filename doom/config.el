;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "haxscramper"
      user-mail-address "haxscramper@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "FiraCode" :size 18)
      doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(load! "evil-main.el")
(load! "evil-edit.el")

;; Load helper functions for working with selections, indentation and other
;; elements. They are used later on in different keybindings for various modes.
(load! "lib/selections.el")
(load! "lib/indentation.el")
(load! "lib/fontlock.el")
(load! "lib/searches.el")

(load! "lang-nim.el")
(load! "lang-elvish.el")
(load! "lang-lisp.el")

(load! "config-editing.el")

(setq confirm-kill-emacs nil)

(after! magit
  (setq git-commit-major-mode 'org-mode))

(after! xref
  (setq-default xref-backend-functions '(etags--xref-backend t)))

(after! keycast

  (define-minor-mode keycast-mode
    "Show current command and its key binding in the mode line."
    :global t
    (if keycast-mode
        (add-hook 'pre-command-hook 'keycast--update t)
      (remove-hook 'pre-command-hook 'keycast-mode-line-update)))

  (setq keycast-substitute-alist
        '((evil-next-line nil nil)
          (evil-previous-line nil nil)
          (evil-forward-char nil nil)
          (evil-backward-char nil nil)
          (ivy-done nil nil)
          (self-insert-command nil nil)))

  (add-to-list 'global-mode-string '("" keycast-mode-line)))

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

(after! hl-todo
  (setq hl-todo-keyword-faces
        '(("TODO" warning bold)
          ("FIXME" error bold)
          ("HACK" font-lock-constant-face bold)
          ("REVIEW" font-lock-keyword-face bold)
          ("NOTE" success bold)
          ("DEPRECATED" font-lock-doc-face bold)
          ("BUG" error bold)
          ("MAYBE" warning bold)
          ("XXX" font-lock-constant-face bold))))

   ;; '(("TODO" . "#dc752f")
   ;;   ("NEXT" . "#dc752f")
   ;;   ("THEM" . "#2d9574")
   ;;   ("PROG" . "#4f97d7")
   ;;   ("OKAY" . "#4f97d7")
   ;;   ("REVIEW" . "#4f97d7")
   ;;   ("IDEA" . "#4f97d7")
   ;;   ("REFACTOR" . "#4f97d7")
   ;;   ("DONT" . "#f2241f")
   ;;   ("DOC" . "#f2241f")
   ;;   ;; ("FAIL" quote hax/face::nuclear)
   ;;   ("ERROR" . "#f2241f")
   ;;   ("TEST" quote hax/face::boxed::orange-bold-boxed)
   ;;   ("WARNING" quote hax/face::boxed::red-bold-boxed)
   ;;   ("IMPLEMENT" . "#f2241f")
   ;;   ("DONE" . "#86dc2f")
   ;;   ("NOTE" quote hax/face::boxed::green-bold-boxed)
   ;;   ("QUESTION" quote hax/face::boxed::dim-yellow-bold-boxed)
   ;;   ("MAYBE" quote hax/face::boxed::dim-yellow-bold-boxed)
   ;;   ("STYLE" quote hax/face::boxed::dim-yellow-bold-boxed)
   ;;   ("KLUDGE" quote hax/face::boxed::dim-yellow-bold-boxed)
   ;;   ("HACK" quote hax/face::boxed::dim-yellow-bold-boxed)
   ;;   ("TEMP" . "#9932cc")
   ;;   ("FIXME" quote hax/face::boxed::orange-bold-boxed)
   ;;   ("XXX" . "#a52a2a")
   ;;   ("XXXX" quote hax/face::boxed::red-bold-boxed)))

(winum-mode t)
