;; Key mapping for the evil mode movements

(setq evil-want-fine-undo t)


(evil-define-operator hax/surround-region (beg end type char)
  "Call surround-region, toggling force-new-line"
  (interactive (evil-surround-input-region-char))
  (let* ((pair (evil-surround-pair char)))
    (save-excursion
      (goto-char end)
      (insert (cdr pair))
      (goto-char beg)
      (insert (car pair))))
  ;; (evil-surround-region beg end type char t)
  )

(map!
 ;; Use to move around ';lkj'
 :nv "j" 'evil-next-line
 :nv "k" 'evil-previous-line
 :nv "l" 'evil-backward-char
 :nv ";" 'evil-forward-char

 :nv "r" 'undo-fu-only-redo
 :n "s" 'evil-change
 :v "s" 'hax/surround-region
 :nv "K" nil)

;; (map!
;;  :nv ".." #'evil-repeat
;;  :nv ".f" #'evil-snipe-repeat
;;  )

(map!
 :leader
 ;; transition period - maybe I will change them back to default versions
 ;; when I fully move to doom emacs. for now I don't want to get constant
 ;; brain short-circuits.
 "bb" 'switch-to-buffer
 "hdb" 'describe-bindings
 "hdf" 'describe-function
 "hdc" 'describe-char
 "hdv" 'describe-variable
 )

(map!
 ;;; window management operations

 ;; Keybindings for the `winum-mode'
 :nvi "<f1>" 'winum-select-window-1
 :nvi "<f2>" 'winum-select-window-2
 :nvi "<f3>" 'winum-select-window-3
 :nvi "<f4>" 'winum-select-window-4
 :nvi "<f5>" 'winum-select-window-5
 :nvi "<f6>" 'winum-select-window-6
 :nvi "<f7>" 'winum-select-window-7
 :nvi "<f8>" 'winum-select-window-8
 )

(defun save-all-buffers() (interactive) (save-some-buffers t))

(after!
 evil
 (evil-ex-define-cmd "W" #'save-all-buffers)
 (evil-ex-define-cmd "w" #'save-all-buffers))
