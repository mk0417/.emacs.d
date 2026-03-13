;;; init-ai.el --- AI -*- lexical-binding: t -*-

(use-package gptel
  :ensure t
  :config
  (setq gptel-include-reasoning nil)
  (setq open-router-key (with-temp-buffer
                          (insert-file-contents "~/.ai_key/openrouter.txt")
                          (string-trim (buffer-string))))

  (setq gptel-model 'openrouter/hunter-alpha
        gptel-backend
        (gptel-make-openai "OpenRouter"
          :host "openrouter.ai"
          :endpoint "/api/v1/chat/completions"
          :stream t
          :key open-router-key
          :models '(openrouter/hunter-alpha
                    qwen/qwen3-coder:free))))

(provide 'init-ai)
