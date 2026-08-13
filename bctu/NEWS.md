# bctu 0.16.0

## New features

* Per-check query text: `save_dvr()`/`save_cdi()`/`run_data_report()` gain
  `check_info` (data frame: `check`, `query`, optional free-text `section` and
  logical `critical`) and `query_column`. The engine writes a `checks_index`
  sheet first in every workbook (overall and per-site), `checks_index.csv`/`.txt`
  beside the findings in every output directory, prepends the query text as the
  first column of each finding row (added after the before/after comparison, so
  rewording a query never shows as new/resolved), and records query, section and
  criticality per check in the manifest. A DVP function can carry the same table
  itself via `attr(findings, "check_info")`; the explicit argument wins.

# bctu 0.15.0

Ground-up rebuild of the package, continuing the version sequence from 0.14.0.
Validated against the OCeAN weekly pipeline (snapshot, DVR, CDI, TMG report)
on 2026-08-13.

## New features

* `special_missing()` declares REDCap missing-data codes (e.g. `UNK`, `OTH`) and
  maps each to a single-letter tag. Pass it to `datasource_redcap(missing_codes =)`
  and the codes are converted at extraction time to native special missing values:
  each column keeps its natural type instead of coercing to character. Snapshots
  export them as Stata `.a` (`.dta`), SAS `.A` (`.sas7bdat`/`.xpt`, best effort), and
  always ship a `.sas` import script that recodes the readable CSV as the reliable
  path.
* `take_snapshot()` / `save_snapshot()` gain `tag =` and `labels =` to mark an
  extraction (e.g. `"DMC-2026-08"`). The tag is recorded on the snapshot and its
  tables, in the manifest, and flows into the DVR/CDI and report manifests. When a
  `tag` is given, bctu also creates an annotated `snap/<tag>` git tag.
* New snapshot payload formats: `"dta"`, `"sas7bdat"`, `"xpt"`, and `"sas"` (an
  import script), alongside `"rds"` and `"csv"`.

## Audit trail and safety

* Git history is the audit trail. Every `take_snapshot()`/`save_snapshot()` and
  every `delete_snapshot()` commits the snapshot metadata (the manifest only,
  never the payload) to the trial repository by default. A destroy is committed
  too, so removing a snapshot still leaves a git record (author, time, reason).
  Use `git = "off"` to skip, or `git = "record"` on save to write HEAD into the
  manifest without committing.
* One store resolver, `snapshot_store()`, which errors when no `bctu-project.yml`
  is found instead of silently falling back to the working directory. Pass an
  explicit `store =` to write outside a project. The former `snapshot_location()`
  is removed.
* Table names are validated before use as directory names (path-traversal guard).
* Same-second snapshots use a `-NN` id suffix instead of distorting the recorded
  time; the manifest is written atomically; `verify_snapshot()` reports `ok = FALSE`
  when there is nothing to verify; loading warns whenever it falls back off the rds.

## Data sources

* REDCap responses are guarded: an HTTP 200 error body can no longer be snapshotted
  as data. Network and toolchain errors are reported in plain English, and `httr2`
  is checked before use with an install hint.
* `datasource_redcap()` gains a per-project keyring `service =`.

## Fixes

* Marker fields are validated with plain-English errors; malformed YAML is reported
  clearly. `has_credential()` no longer warns as a side effect.
