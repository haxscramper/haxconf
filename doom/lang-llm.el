;;; -*- lexical-binding: t; -*-

(defun hax/get-openrouter-api-key-from-home-envrc ()
  (let ((envrc (expand-file-name "~/.envrc")))
    (when (file-exists-p envrc)
      (with-temp-buffer
        (let ((default-directory (expand-file-name "~")))
          (call-process "direnv" nil t nil "exec" "." "sh" "-c" "printf '%s' \"$OPENROUTER_API_KEY\""))
        (buffer-string)))))


(set-popup-rule! "*ort-rt*" :ignore t)

(after! gptel
  ;; OpenRouter offers an OpenAI compatible API
  (setq hax/ort-key (hax/get-openrouter-api-key-from-home-envrc))
  (if (and hax/ort-key (not (string-empty-p hax/ort-key)))
      (progn
        (hax/log "ORT key found OK" :print-stdout)
        (setq gptel-model   'mixtral-8x7b-32768
              gptel-backend
              (gptel-make-openai "ort-rt"               
                :host "openrouter.ai"
                :endpoint "/api/v1/chat/completions"
                :stream t
                :key hax/ort-key
                :models '(anthropic/claude-sonnet-4))))
    (hax/log "ORT key found fail" :print-stderr)))

