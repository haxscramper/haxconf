;;; -*- lexical-binding: t; -*-

;; (after! cc-mode
;;   (define-abbrev-table 'c-mode-abbrev-table
;;     '(("imain" "int\nmain (int ac, char **av[])\n{\n\n}")
;;       ("if"    "if () {\n}\n")
;;       ("elif" "else if () {\n")
;;       ("cauto" "const auto")
;;       ("autor" "const auto&")
;;       ("else"  "else {\n}\n")
;;       ("while" "while () {\n}\n")
;;       ("for"   "for (;;) {\n}\n")
;;       ("pr"    "printf (\"\")"))))
(defun hax/cpp-mode-hook ()
  (interactive)
  (map!
   :leader
   :n "id" (cmd! (insert "/// \\brief ") (evil-insert)))
  (abbrev-mode 0))



(add-hook! 'c++-mode-hook 'hax/cpp-mode-hook)
