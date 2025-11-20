;;; init-abbrev.el --- Abbreviations -*- lexical-binding: t -*-

(progn
  (when (boundp 'python-mode-abbrev-table)
    (clear-abbrev-table python-mode-abbrev-table))
  (define-abbrev-table 'python-mode-abbrev-table
    '(
      ("mywrds"
       "with open(Path('~/.pass.yml').expanduser()) as f:
    wrds_username, wrds_password = [*yaml.safe_load(f)['wrds'].values()]")

      ("mywrdsconn"
       "def wrds_connection():
    with open(Path('~/.pass.yml').expanduser()) as f:
        wrds_username, wrds_password = [*yaml.safe_load(f)['wrds'].values()]

    uri = (
        f'postgresql://{wrds_username}:{wrds_password}'
        '@wrds-pgdata.wharton.upenn.edu:9737/wrds?sslmode=require')
    return uri")
      )))

(setq save-abbrevs nil)

(provide 'init-abbrev)
