# bctu

Regulated clinical-trial data management for the Birmingham Clinical Trials Unit
(BCTU): pull a data snapshot from a source, check it, and produce reports, with a
human-readable audit trail on every extraction.

bctu is built for occasional users, not R experts. Function names say what they do
(`take_snapshot()`, `check_setup()`, `save_dvr()`), the common task is one obvious
call, configuration is explicit (never guessed from the working directory), and
errors are written in plain English.

## Install

```r
# install.packages("remotes")
remotes::install_github("jackahall/bctu-ng", subdir = "bctu")
```

## The happy path

```r
library(bctu)

# 1. Mark the trial root once. Writes bctu-project.yml (commit it).
bctu_init_project("OCeAN")

# 2. Describe where the data comes from. The token is never written here;
#    declare where it lives and bctu resolves it from the keyring or environment.
src <- datasource_redcap(
  token_id = "ocean",
  url      = "https://redcap.example.org/api/"
)

# 3. Take an immutable, timestamped snapshot. The location is announced, an
#    audit-ledger record is written, and a manifest with a SHA-256 per table is saved.
snap <- take_snapshot(src)

# 4. Load, verify, list, or delete snapshots.
snap <- load_snapshot("latest")
verify_snapshot("latest")     # checks the on-disk SHA-256s
list_snapshots()
```

## Missing-data codes

REDCap exports missing-data codes (`UNK`, `OTH`, ...) as text, which coerces the
whole column to character. Declare them and bctu keeps each column's natural type,
storing the codes as native special missing values.

```r
codes <- special_missing(UNK ~ "a", OTH ~ "b", NASK ~ "c")
src   <- datasource_redcap("ocean", url, missing_codes = codes)
snap  <- take_snapshot(src, formats = c("rds", "csv", "dta", "sas"))
```

The snapshot then exports the codes as Stata `.a` (`.dta`) and SAS `.A`
(`.sas7bdat`/`.xpt`), and always writes a `.sas` import script that recodes the CSV
as the reliable path.

## Tagging an extraction

```r
# Marks the snapshot end to end (manifest, ledger, DVR/report), records the code
# HEAD, commits the metadata, and creates an annotated snap/DMC-2026-08 git tag.
snap <- take_snapshot(src, tag = "DMC-2026-08")
```

## Data validation (DVP / CDI)

A validation plan is one function returning a named list; each element is one check
(any data frame you like).

```r
ocean_dvp <- function(data) {
  list(
    weight_out_of_range = subset(data$records, weight_kg < 30 | weight_kg > 250),
    missing_consent     = subset(data$records, is.na(consent_date))
  )
}

save_dvr(ocean_dvp, after = snap, site_col = "site")             # a fresh DVR
save_dvr(ocean_dvp, after = snap, before = previous, site_col = "site")  # with an update diff
```

`save_dvr()` writes an overall workbook plus one per site, an update set (new /
unchanged / resolved) when a `before` snapshot is given, CSV/TXT copies, and a
manifest. `save_cdi()` is the critical-data equivalent.

## Reporting

```r
report <- bctu_report(
  title    = "OCeAN Monitoring Report",
  sections = list(report_heading("Recruitment"), a_table),
  meta     = snap
)
render_report(report, formats = c("docx", "pdf"))   # one table object, both formats
```

Every rendered report embeds a manifest (snapshot id and SHA-256, data-cut date,
package / pandoc / LaTeX versions, template hash).

## Checking the environment

```r
check_setup()                 # installation and environment check (IQ), PASS/FAIL table
write_setup_report()          # a versioned YAML qualification record
package_risk_report()         # riskmetric-based dependency risk scoring
```

## Design

* Explicit configuration: one committed `bctu-project.yml` marker anchors all path
  resolution; bctu errors rather than guessing a location.
* Human-readable records: manifests and the audit ledger are plain-text YAML.
* Auditable: every extraction and deletion appends to an append-only, hash-chained
  ledger; integrity is SHA-256 in the manifest; provenance is independent of the
  trial git repository.
* One canonical time policy (UTC snapshot ids, explicit-timezone data-cut dates).
