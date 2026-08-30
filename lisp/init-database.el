;;; init-database.el --- Databases -*- lexical-binding: t -*-

(setq sql-postgres-login-params
      '((user)
        (database :default "wrds")
        (server :default "wrds-pgdata.wharton.upenn.edu")
        (port :default 9737)))

(add-hook 'sql-interactive-mode-hook
          (lambda () (toggle-truncate-lines t)))

(use-package pgsql
  :ensure t)

(use-package clutch
  :ensure t
  :config
  (setq clutch-connect-timeout-seconds 10
        clutch-read-idle-timeout-seconds 30
        clutch-query-timeout-seconds 20)
  (let ((wrds-username (p-read-env "wrds_username"))
        (wrds-password (p-read-env "wrds_password")))
    (setq clutch-connection-alist
          `(("wrds-pg" . (
                          :backend pg
                          :host "wrds-pgdata.wharton.upenn.edu"
                          :port 9737
                          :user ,wrds-username
                          :password ,wrds-password
                          :database "wrds"
                          :sslmode "require"))))))

(provide 'init-database)
