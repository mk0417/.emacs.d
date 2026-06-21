;;; init-abbrev.el --- Abbreviations -*- lexical-binding: t -*-

(progn
  (when (boundp 'python-ts-mode-abbrev-table)
    (clear-abbrev-table python-ts-mode-abbrev-table))
  (define-abbrev-table 'python-ts-mode-abbrev-table
    '(
      ("mywrds"
       "def wrds_uri():
    load_dotenv(Path('~/.env').expanduser())
    wrds_username = os.getenv('wrds_username')
    wrds_password = os.getenv('wrds_password')
    uri = (
        f'postgresql://{wrds_username}:{wrds_password}'
        '@wrds-pgdata.wharton.upenn.edu:9737/wrds?sslmode=require')
    return uri")
      )))

(setq save-abbrevs nil)

(provide 'init-abbrev)
