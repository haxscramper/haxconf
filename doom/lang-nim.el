(setq
 nim-imenu-generic-expression
 '(
   ("object" "^  \\(.*?\\) = .*?object" 1)
   ("object" "^  \\(.*?\\) = .*?enum" 1)
   ("proc" "^ *proc *\\(.*\\)" 1)
   ("func" "^ *func *\\(.*\\)" 1)
   ("macro" "^ *macro*\\(.*\\)" 1)
   ("method" "^ *method*\\(.*\\)" 1)
   ("converter" "^ *converter*\\(.*\\)" 1)
   ("template" "^ *template *\\(.*\\)" 1)
   ("iterator" "^ *iterator *\\(.*\\)" 1)
   ("separator" "^ *\\(#=* [^#]* =*#\\)$" 1)
   ("separator" "^ *\\(#-* [^#]* -*#\\)$" 1)
   ("separator" "^#\\(\\** [^#]* \\**#\\)$" 1)
   ("suite" "suite \\(\".*\"\\):" 1)
   ("test" "  test \\(\".*\"\\):" 1)
   ("test" "  multitest \\(\".*\"\\):" 1)
   ("task" "task \\(\".*\"\\):" 1)
   ("const" "^const \\(.*\\)" 1)
   ("let" "^let \\(.*\\)" 1)
   ("var" "^var \\(.*\\)" 1)))


(font-lock-replace-keywords
 'nim-mode
 `(
   ("notNil" . 'font-lock-builtin-face)
   ;; Highlight function calls as well
   (,(rx
      (| " " "." "]" ")" "(") (group (in lower) (+ word)) "(") .
      (1 font-lock-function-name-face))
   ))

(add-hook!
 nim-mode
 ;: Disable completion from nimsuggest, it is mostly useless
 (nimsuggest-mode 0)
 ;; Flycheck relies on nimsuggest, so disable it as well
 (flycheck-mode 0)
 (electric-indent-mode 0)
 (company-mode t)
 (setq company-backends '(company-capf :with company-etags))
 (setq imenu-generic-expression nim-imenu-generic-expression)

 )
