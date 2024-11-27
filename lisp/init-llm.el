;;; init-llm.el --- LLM -*- lexical-binding: t -*-

(use-package gptel
  :ensure t
  :config
  (setq gptel-model 'llama3.2:latest
        gptel-backend (gptel-make-ollama "Ollama"
                                         :host "localhost:11434"
                                         :stream t
                                         :models '(llama3.2:latest)))
  )

(provide 'init-llm)
