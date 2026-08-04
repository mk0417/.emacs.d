;;; init-abbrev.el --- Abbreviations -*- lexical-binding: t -*-

(define-abbrev-table 'global-abbrev-table
  '(
    ;; Python WRDS uri
    ("pywrds"
     "def gen_wrds_uri():
    load_dotenv(Path('~/.env').expanduser())
    wrds_username = os.getenv('wrds_username')
    wrds_password = os.getenv('wrds_password')
    uri = (
        f'postgresql://{wrds_username}:{wrds_password}'
        '@wrds-pgdata.wharton.upenn.edu:9737/wrds?sslmode=require')
    return uri")

    ;; R WRDS
    ("rwrds"
     "wrds <- dbConnect(
    RPostgres::Postgres(),
    host='wrds-pgdata.wharton.upenn.edu',
    port=9737,
    dbname='wrds',
    user=Sys.getenv('wrds_username'),
    password=Sys.getenv('wrds_password'),
    sslmode='require'")

    ;; SQL query: CRSP
    ("crspsql"
     "select permno, mthcaldt as date, mthret as ret, mthcap as me
    from crsp.msf_v2
    where primaryexch in ('N', 'A', 'Q')
    and conditionaltype='RW'
    and tradingstatusflg='A'
    and sharetype='NS'
    and securitytype='EQTY'
    and securitysubtype='COM'
    and usincflg='Y'
    and issuertype in ('ACOR', 'CORP')")

    ;; SQL query: FF factors and risk-free rate
    ("ffsql"
     "select date, mktrf, smb, hml, rf
     from ff.factors_monthly")

    ;; SQL query: CCM link
    ("ccmsql"
     "select a.gvkey, a.cusip, b.lpermno::int as permno,
    b.lpermco::int as permco, b.linkdt, b.linkenddt
    from comp.names as a
    inner join crsp.ccmxpf_lnkhist as b
    on a.gvkey = b.gvkey
    where b.linktype in ('LC', 'LU') and b.linkprim in ('P', 'C')
    order by a.gvkey")

    ;; SQL query: Compustat
    ("compsql"
     "select gvkey, datadate, at, ni, curcd
    from comp.funda
    where consol='C' and popsrc='D' and datafmt='STD' and indfmt='INDL'")
    ))

(setq save-abbrevs nil)

(provide 'init-abbrev)
