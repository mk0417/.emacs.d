;;; init-ai.el --- AI -*- lexical-binding: t -*-

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
           (prompt
            (format
             "Rewrite and improve the following English text.
              Return ONLY the rewritten text.
              Do not explain your changes.
              Do not include reasoning.
              Do not use quotes around the result.
             Text:
              %s"
             text)))
      (setq gptel-model "nvidia/nemotron-3.5-lightning:free")
      (gptel-request
          prompt
        :callback
        (lambda (response _metadata)
          (when (stringp response)
            (let ((response (string-trim response)))
              (unless (string-empty-p response)
                (save-excursion
                  (goto-char start)
                  (delete-region start end)
                  (insert response)))))))))

  (setq gptel-include-reasoning nil)
  (setq open-router-key (with-temp-buffer
                          (insert-file-contents "~/.ai_key/openrouter.txt")
                          (string-trim (buffer-string))))

  (setq gptel-model 'openrouter/nvidia/nemotron-3.5-lightning:free
        gptel-backend
        (gptel-make-openai "OpenRouter"
          :host "openrouter.ai"
          :endpoint "/api/v1/chat/completions"
          :stream t
          :key open-router-key
          :models '(nvidia/nemotron-3.5-lightning:free))))

(provide 'init-ai)
