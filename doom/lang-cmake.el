;;; -*- lexical-binding: t; -*-


(defun hax/cmake-mode-hook()
  (interactive)
  (setq imenu-generic-expression
        '(
          ("function" "^ *function *\\(.*\\)" 1)
          ))
  )


(add-hook! 'cmake-mode-hook 'hax/cmake-mode-hook)
