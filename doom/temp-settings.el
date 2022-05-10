;;; -*- lexical-binding: t; -*-

(cl-defun replace-selection
    (txt &optional
         (begin (get-selected-region-start))
         (end (get-selected-region-end)))
  (delete-region begin end)
  (insert txt))

(defun hax/nim-wrap-warnings ()
  (interactive)
  (replace-selection (format "(assert fase; {.warning: \"[FIXME] '%s'\".})"
                             (get-selected-region-text))))




(defun hax/insert-radio-link ()
  (interactive)
  (let ((targets
         (matches-in-buffer
          (rx "<<" (group (* (not ">"))) ">>" (group (* word)))
          (lambda () (cons (match-string 1) (match-string 2))))))
    (ivy-read
     "Select target"
     targets
     :action (lambda (x) (message "Selected tag {%s}" x)))))

(cl-defun hax/hex-to-binary (hex &optional
                                 (word-format "%s")
                                 (word-bits 8))
  (s-join
   " "
   (--map
    (format
     word-format
     (s-join " " (--map
                  (s-pad-left word-bits "0" it)
                  (seq-partition (binary-number-to-string (string-to-number it 16)) word-bits))))
    (s-split  " " hex))))

(defun hax/copy-hex-as-binary ()
  (interactive)
  (let* ((sel (hax/hex-to-binary (get-selected-region-text) "[%s]")))
    (with-temp-buffer
      (insert sel)
      (copy-region-as-kill (point-min) (point-max))
      (message "%s" (buffer-substring (point-min) (point-max)))))
  (deactivate-mark))

(defun org-babel-execute:hex (body params)
  (hax/hex-to-binary body "[%s]"))

(custom-set-variables
 '(mini-frame-show-parameters
   '((top . 200)
     (width . 0.8)
     (height . 0.7)
     (left . 0.5))))

(add-to-list 'auto-mode-alist '("\\.ASM" . asm-mode))


(defun hax/asm-mode-hook ()
  (interactive)
  (setq
   imenu-generic-expression
   `(("label" ,(rx bol (group (+ (| (any word) "_"))) ":") 1))))

(add-hook! asm-mode 'hax/asm-mode-hook)

(defun hax/is-temporary-buffer-p ()
  (s-starts-with-p "*" (buffer-name)))

(map!
 [s-right] #'evil-window-increase-width
 [s-left] #'evil-window-decrease-width
 :desc "increase window height"
 [s-up] (cmd! (if (hax/is-temporary-buffer-p)
                  (evil-window-increase-height 20)
                (evil-window-increase-height 1)))
 :desc "decrease window height"
 [s-down] (cmd! (if (hax/is-temporary-buffer-p)
                    (evil-window-decrease-height 20)
                  (evil-window-decrease-height 1))))


;; (defun restyle-ascii-str (str)
;;   (let* ((result))
;;     (dolist )
;;     result
;;     )
;;   )

(setq
 hax/restyle-ascii-table  ;;
 '( ;;
   ?A ("A" "Ɐ" "𝔄" "𝕬" "𝔸" "𝐀" "𝐴" "𝑨" "𝒜" "𝓐" "𝖠" "𝗔" "𝘈" "𝘼" "𝙰")
   ?B ("B" "B" "𝔅" "𝕭" "𝔹" "𝐁" "𝐵" "𝑩" "ℬ" "𝓑" "𝖡" "𝗕" "𝘉" "𝘽" "𝙱")
   ?C ("C" "Ɔ" "ℭ" "𝕮" "ℂ" "𝐂" "𝐶" "𝑪" "𝒞" "𝓒" "𝖢" "𝗖" "𝘊" "𝘾" "𝙲")
   ?D ("D" "D" "𝔇" "𝕯" "𝔻" "𝐃" "𝐷" "𝑫" "𝒟" "𝓓" "𝖣" "𝗗" "𝘋" "𝘿" "𝙳")
   ?E ("E" "Ǝ" "𝔈" "𝕰" "𝔼" "𝐄" "𝐸" "𝑬" "ℰ" "𝓔" "𝖤" "𝗘" "𝘌" "𝙀" "𝙴")
   ?F ("F" "Ⅎ" "𝔉" "𝕱" "𝔽" "𝐅" "𝐹" "𝑭" "ℱ" "𝓕" "𝖥" "𝗙" "𝘍" "𝙁" "𝙵")
   ?G ("G" "⅁" "𝔊" "𝕲" "𝔾" "𝐆" "𝐺" "𝑮" "𝒢" "𝓖" "𝖦" "𝗚" "𝘎" "𝙂" "𝙶")
   ?H ("H" "H" "ℌ" "𝕳" "ℍ" "𝐇" "𝐻" "𝑯" "ℋ" "𝓗" "𝖧" "𝗛" "𝘏" "𝙃" "𝙷")
   ?I ("I" "I" "ℑ" "𝕴" "𝕀" "𝐈" "𝐼" "𝑰" "ℐ" "𝓘" "𝖨" "𝗜" "𝘐" "𝙄" "𝙸")
   ?J ("J" "ſ" "𝔍" "𝕵" "𝕁" "𝐉" "𝐽" "𝑱" "𝒥" "𝓙" "𝖩" "𝗝" "𝘑" "𝙅" "𝙹")
   ?K ("K" "Ʞ" "𝔎" "𝕶" "𝕂" "𝐊" "𝐾" "𝑲" "𝒦" "𝓚" "𝖪" "𝗞" "𝘒" "𝙆" "𝙺")
   ?L ("L" "Ꞁ" "𝔏" "𝕷" "𝕃" "𝐋" "𝐿" "𝑳" "ℒ" "𝓛" "𝖫" "𝗟" "𝘓" "𝙇" "𝙻")
   ?M ("M" "Ɯ" "𝔐" "𝕸" "𝕄" "𝐌" "𝑀" "𝑴" "ℳ" "𝓜" "𝖬" "𝗠" "𝘔" "𝙈" "𝙼")
   ?N ("N" "N" "𝔑" "𝕹" "ℕ" "𝐍" "𝑁" "𝑵" "𝒩" "𝓝" "𝖭" "𝗡" "𝘕" "𝙉" "𝙽")
   ?O ("O" "O" "𝔒" "𝕺" "𝕆" "𝐎" "𝑂" "𝑶" "𝒪" "𝓞" "𝖮" "𝗢" "𝘖" "𝙊" "𝙾")
   ?P ("P" "Ԁ" "𝔓" "𝕻" "ℙ" "𝐏" "𝑃" "𝑷" "𝒫" "𝓟" "𝖯" "𝗣" "𝘗" "𝙋" "𝙿")
   ?Q ("Q" "Ò" "𝔔" "𝕼" "ℚ" "𝐐" "𝑄" "𝑸" "𝒬" "𝓠" "𝖰" "𝗤" "𝘘" "𝙌" "𝚀")
   ?R ("R" "ᴚ" "ℜ" "𝕽" "ℝ" "𝐑" "𝑅" "𝑹" "ℛ" "𝓡" "𝖱" "𝗥" "𝘙" "𝙍" "𝚁")
   ?S ("S" "S" "𝔖" "𝕾" "𝕊" "𝐒" "𝑆" "𝑺" "𝒮" "𝓢" "𝖲" "𝗦" "𝘚" "𝙎" "𝚂")
   ?T ("T" "Ʇ" "𝔗" "𝕿" "𝕋" "𝐓" "𝑇" "𝑻" "𝒯" "𝓣" "𝖳" "𝗧" "𝘛" "𝙏" "𝚃")
   ?U ("U" "∩" "𝔘" "𝖀" "𝕌" "𝐔" "𝑈" "𝑼" "𝒰" "𝓤" "𝖴" "𝗨" "𝘜" "𝙐" "𝚄")
   ?V ("V" "Ʌ" "𝔙" "𝖁" "𝕍" "𝐕" "𝑉" "𝑽" "𝒱" "𝓥" "𝖵" "𝗩" "𝘝" "𝙑" "𝚅")
   ?W ("W" "ʍ" "𝔚" "𝖂" "𝕎" "𝐖" "𝑊" "𝑾" "𝒲" "𝓦" "𝖶" "𝗪" "𝘞" "𝙒" "𝚆")
   ?X ("X" "X" "𝔛" "𝖃" "𝕏" "𝐗" "𝑋" "𝑿" "𝒳" "𝓧" "𝖷" "𝗫" "𝘟" "𝙓" "𝚇")
   ?Y ("Y" "⅄" "𝔜" "𝖄" "𝕐" "𝐘" "𝑌" "𝒀" "𝒴" "𝓨" "𝖸" "𝗬" "𝘠" "𝙔" "𝚈")
   ?Z ("Z" "Z" "ℨ" "𝖅" "ℤ" "𝐙" "𝑍" "𝒁" "𝒵" "𝓩" "𝖹" "𝗭" "𝘡" "𝙕" "𝚉")
   ?a ("a" "ɐ" "𝔞" "𝖆" "𝕒" "𝐚" "𝑎" "𝒂" "𝒶" "𝓪" "𝖺" "𝗮" "𝘢" "𝙖" "𝚊")
   ?b ("b" "q" "𝔟" "𝖇" "𝕓" "𝐛" "𝑏" "𝒃" "𝒷" "𝓫" "𝖻" "𝗯" "𝘣" "𝙗" "𝚋")
   ?c ("c" "ɔ" "𝔠" "𝖈" "𝕔" "𝐜" "𝑐" "𝒄" "𝒸" "𝓬" "𝖼" "𝗰" "𝘤" "𝙘" "𝚌")
   ?d ("d" "p" "𝔡" "𝖉" "𝕕" "𝐝" "𝑑" "𝒅" "𝒹" "𝓭" "𝖽" "𝗱" "𝘥" "𝙙" "𝚍")
   ?e ("e" "ǝ" "𝔢" "𝖊" "𝕖" "𝐞" "𝑒" "𝒆" "ℯ" "𝓮" "𝖾" "𝗲" "𝘦" "𝙚" "𝚎")
   ?f ("f" "ɟ" "𝔣" "𝖋" "𝕗" "𝐟" "𝑓" "𝒇" "𝒻" "𝓯" "𝖿" "𝗳" "𝘧" "𝙛" "𝚏")
   ?g ("g" "ᵷ" "𝔤" "𝖌" "𝕘" "𝐠" "𝑔" "𝒈" "ℊ" "𝓰" "𝗀" "𝗴" "𝘨" "𝙜" "𝚐")
   ?h ("h" "ɥ" "𝔥" "𝖍" "𝕙" "𝐡" "ℎ" "𝒉" "𝒽" "𝓱" "𝗁" "𝗵" "𝘩" "𝙝" "𝚑")
   ?i ("i" "ᴉ" "𝔦" "𝖎" "𝕚" "𝐢" "𝑖" "𝒊" "𝒾" "𝓲" "𝗂" "𝗶" "𝘪" "𝙞" "𝚒")
   ?j ("j" "f" "𝔧" "𝖏" "𝕛" "𝐣" "𝑗" "𝒋" "𝒿" "𝓳" "𝗃" "𝗷" "𝘫" "𝙟" "𝚓")
   ?k ("k" "ʞ" "𝔨" "𝖐" "𝕜" "𝐤" "𝑘" "𝒌" "𝓀" "𝓴" "𝗄" "𝗸" "𝘬" "𝙠" "𝚔")
   ?l ("l" "ꞁ" "𝔩" "𝖑" "𝕝" "𝐥" "𝑙" "𝒍" "𝓁" "𝓵" "𝗅" "𝗹" "𝘭" "𝙡" "𝚕")
   ?m ("m" "ɯ" "𝔪" "𝖒" "𝕞" "𝐦" "𝑚" "𝒎" "𝓂" "𝓶" "𝗆" "𝗺" "𝘮" "𝙢" "𝚖")
   ?n ("n" "u" "𝔫" "𝖓" "𝕟" "𝐧" "𝑛" "𝒏" "𝓃" "𝓷" "𝗇" "𝗻" "𝘯" "𝙣" "𝚗")
   ?o ("o" "o" "𝔬" "𝖔" "𝕠" "𝐨" "𝑜" "𝒐" "ℴ" "𝓸" "𝗈" "𝗼" "𝘰" "𝙤" "𝚘")
   ?p ("p" "d" "𝔭" "𝖕" "𝕡" "𝐩" "𝑝" "𝒑" "𝓅" "𝓹" "𝗉" "𝗽" "𝘱" "𝙥" "𝚙")
   ?q ("q" "b" "𝔮" "𝖖" "𝕢" "𝐪" "𝑞" "𝒒" "𝓆" "𝓺" "𝗊" "𝗾" "𝘲" "𝙦" "𝚚")
   ?r ("r" "ɹ" "𝔯" "𝖗" "𝕣" "𝐫" "𝑟" "𝒓" "𝓇" "𝓻" "𝗋" "𝗿" "𝘳" "𝙧" "𝚛")
   ?s ("s" "s" "𝔰" "𝖘" "𝕤" "𝐬" "𝑠" "𝒔" "𝓈" "𝓼" "𝗌" "𝘀" "𝘴" "𝙨" "𝚜")
   ?t ("t" "ʇ" "𝔱" "𝖙" "𝕥" "𝐭" "𝑡" "𝒕" "𝓉" "𝓽" "𝗍" "𝘁" "𝘵" "𝙩" "𝚝")
   ?u ("u" "n" "𝔲" "𝖚" "𝕦" "𝐮" "𝑢" "𝒖" "𝓊" "𝓾" "𝗎" "𝘂" "𝘶" "𝙪" "𝚞")
   ?v ("v" "ʌ" "𝔳" "𝖛" "𝕧" "𝐯" "𝑣" "𝒗" "𝓋" "𝓿" "𝗏" "𝘃" "𝘷" "𝙫" "𝚟")
   ?w ("w" "ʍ" "𝔴" "𝖜" "𝕨" "𝐰" "𝑤" "𝒘" "𝓌" "𝔀" "𝗐" "𝘄" "𝘸" "𝙬" "𝚠")
   ?x ("x" "x" "𝔵" "𝖝" "𝕩" "𝐱" "𝑥" "𝒙" "𝓍" "𝔁" "𝗑" "𝘅" "𝘹" "𝙭" "𝚡")
   ?y ("y" "ʎ" "𝔶" "𝖞" "𝕪" "𝐲" "𝑦" "𝒚" "𝓎" "𝔂" "𝗒" "𝘆" "𝘺" "𝙮" "𝚢")
   ?z ("z" "z" "𝔷" "𝖟" "𝕫" "𝐳" "𝑧" "𝒛" "𝓏" "𝔃" "𝗓" "𝘇" "𝘻" "𝙯" "𝚣")
   ?0 ("0" "0" "0" "0" "𝟘" "𝟎" "0" "0" "0" "0" "0" "𝟢" "𝟬" "0" "𝟶")
   ?1 ("1" "1" "1" "1" "𝟙" "𝟏" "1" "1" "1" "1" "1" "𝟣" "𝟭" "1" "𝟷")
   ?2 ("2" "2" "2" "2" "𝟚" "𝟐" "2" "2" "2" "2" "2" "𝟤" "𝟮" "2" "𝟸")
   ?3 ("3" "3" "3" "3" "𝟛" "𝟑" "3" "3" "3" "3" "3" "𝟥" "𝟯" "3" "𝟹")
   ?4 ("4" "4" "4" "4" "𝟜" "𝟒" "4" "4" "4" "4" "4" "𝟦" "𝟰" "4" "𝟺")
   ?5 ("5" "5" "5" "5" "𝟝" "𝟓" "5" "5" "5" "5" "5" "𝟧" "𝟱" "5" "𝟻")
   ?6 ("6" "6" "6" "6" "𝟞" "𝟔" "6" "6" "6" "6" "6" "𝟨" "𝟲" "6" "𝟼")
   ?7 ("7" "7" "7" "7" "𝟟" "𝟕" "7" "7" "7" "7" "7" "𝟩" "𝟳" "7" "𝟽")
   ?8 ("8" "8" "8" "8" "𝟠" "𝟖" "8" "8" "8" "8" "8" "𝟪" "𝟴" "8" "𝟾")
   ?9 ("9" "9" "9" "9" "𝟡" "𝟗" "9" "9" "9" "9" "9" "𝟫" "𝟵" "9" "𝟿")
   ))
