;; Configuration for general spellchecking

(setq save-abbrevs 'silent)
(setq flycheck-disabled-checkers '(emacs-lisp-checkdoc))

(if nil
    (require 'flycheck-languagetool)

  (defun flycheck-languagetool--start-server ()
    "Start the LanguageTool server if we didn't already. This
procedure is a redefinition of the one provided in the
flycheck-languagetool failed to work for me. This one expects a
`languagetool --http' script to work, (it is provided in the arch
package)"
    (message "starting languagetool server")
    (unless (process-live-p (get-process "languagetool-server"))
      (let ((process
             (apply #'start-process
                    "languagetool-server"
                    " *LanguageTool server*"
                    "languagetool"
                    "--http"
                    "--port" (format "%s" flycheck-languagetool-server-port)
                    flycheck-languagetool-server-args)))
        (set-process-query-on-exit-flag process nil)
        (while
            (with-current-buffer (process-buffer process)
              (goto-char (point-min))
              (unless (re-search-forward " Server started$" nil t)
                (accept-process-output process 1)
                (process-live-p process))))))))

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
