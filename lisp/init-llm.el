;;; init-llm.el --- LLM -*- lexical-binding: t -*-

(use-package gptel
  :ensure t
  :config
  (defun p-gpt-rewrite-english ()
    (interactive)
    (require 'gptel)
    (unless (use-region-p)
      (user-error "Please select a region of text to rewrite"))
    (let* ((start (region-beginning))
           (end (region-end))
           (text (buffer-substring-no-properties start end))
           (prompt (format "Please rewrite and improve the following English text:\n\n%s" text)))
      (setq gptel-model "meta-llama/llama-4-maverick:free")
      (gptel-request prompt
        :callback (lambda (response _)
                    (when (and response (not (string-blank-p response)))
                      (save-excursion
                        (delete-region start end)
                        (goto-char start)
                        (insert response)))))))

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
