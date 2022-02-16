;;; -*- lexical-binding: t; -*-

(add-hook! 'emacs-lisp-mode-hook
  (aggressive-indent-mode t)
  (map!
   :map emacs-lisp-mode-map
   :nv ",ef" 'eval-defun
   :desc "Eval last S-expression"
   :nv ",ee" (lambda (a)
               (interactive "P")
               (save-excursion
                 (eval-last-sexp a)))))
