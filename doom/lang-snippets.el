;;; -*- lexical-binding: t; -*-

(require 'savehist)

(defvar hax/yas-org-src-language-history nil)

(add-to-list 'savehist-additional-variables
             'hax/yas-org-src-language-history)

(savehist-mode 1)

(defun hax/yas-org-src-language ()
  (let* ((default (car hax/yas-org-src-language-history))
         (language
          (completing-read
           (format-prompt "Source language" default)
           hax/yas-org-src-language-history
           nil
           nil
           nil
           'hax/yas-org-src-language-history
           default)))
    (setq hax/yas-org-src-language-history
          (cons language
                (delete language hax/yas-org-src-language-history)))
    language))
