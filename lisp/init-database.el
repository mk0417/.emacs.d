;;; init-default.el --- Databases -*- lexical-binding: t -*-

(setq sql-postgres-login-params
      '((user)
        (database :default "wrds")
        (server :default "wrds-pgdata.wharton.upenn.edu")
        (port :default 9737)))

(add-hook 'sql-interactive-mode-hook
          (lambda () (toggle-truncate-lines t)))

(provide 'init-database)
