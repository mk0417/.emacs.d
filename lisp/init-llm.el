;;; init-llm.el --- LLM -*- lexical-binding: t -*-

(use-package gptel
  :ensure t
  :config
  (setq open-router-key (with-temp-buffer
                          (insert-file-contents "~/.llm_key/openrouter.txt")
                          (string-trim (buffer-string))))
  (setq gptel-model "meta-llama/llama-4-maverick:free"
        gptel-backend
        (gptel-make-openai "OpenRouter"
          :host "openrouter.ai"
          :endpoint "/api/v1/chat/completions"
          :stream t
          :key open-router-key
          :models '(meta-llama/llama-4-maverick:free
                    deepseek/deepseek-chat-v3-0324:free))))

(provide 'init-llm)
