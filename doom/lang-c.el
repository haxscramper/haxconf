;;; -*- lexical-binding: t; -*-

(after! cc-mode
  (define-abbrev-table 'c-mode-abbrev-table
    '(("imain" "int\nmain (int ac, char **av[])\n{\n\n}" "C-p TAB")
      ("if"    "if () {\n}\n" "C-M-b C-M-q C-- C-M-d")
      ("else"  "else {\n}\n"  "C-M-b C-M-q C-M-d RET")
      ("while" "while () {\n}\n" "C-M-b C-M-q C-- C-M-d")
      ("for"   "for (;;) {\n}\n" "C-M-b C-M-q C-M-b C-M-d")
      ("pr"    "printf (\"\")" "C-2 C-b"))))
