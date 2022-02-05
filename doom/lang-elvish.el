(add-hook!
 elvish-mode
 (setq imenu-generic-expression '(
   ("fn" "fn *\\(.*\\)" 1))))
