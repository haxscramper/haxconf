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



(add-hook!
 nim-mode
 (nimsuggest-mode -1)
 (flycheck-mode -1)
 (setq imenu-generic-expression nim-imenu-generic-expression)
 )

