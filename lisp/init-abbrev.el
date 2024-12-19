;;; init-abbrev.el --- Abbreviations -*- lexical-binding: t -*-

(define-abbrev global-abbrev-table
  "mewrds"
  "def wrds_connection():
    with open(Path('~/.pass.yml').expanduser()) as f:
        wrds_username, wrds_password = [*yaml.safe_load(f)['wrds'].values()]

    uri = (
        f'postgresql://{wrds_username}:{wrds_password}'
        '@wrds-pgdata.wharton.upenn.edu:9737/wrds?sslmode=require')
    return uri")

(provide 'init-abbrev)
