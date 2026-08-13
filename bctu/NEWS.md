# bctu 0.16.4

## Fixes

* REDCap labelling now covers checkbox columns: dictionary rows are matched to
  exported column names through the field-name export map (`exportFieldNames`),
  so `field___N` checkbox columns (and their No/Yes coding) are labelled the
  way the original package labelled them. Under 0.15.0 to 0.16.3 those columns
  were silently left as plain numeric, which made label-string checks return
  zero findings on affected trials; snapshots fetched with those versions carry
  the defective typing and should be re-taken.
* The standalone `checks_index.csv`/`.txt` files are no longer written by
  default: the index lives as the first tab of every workbook. `write_readable
  = TRUE` restores them alongside the per-check CSV/TXT copies.

# bctu 0.16.3

## Changed

* The DVR/CDI delivered record is the Excel workbook, always: `openxlsx` is
  required up front (plain-English error if missing; the silent skip-the-
  workbook fallback is gone) and the per-check CSV/TXT copies are no longer
  written by default. `write_readable = TRUE` (replacing `write_xlsx`) opts
  back in; `checks_index.csv`/`.txt` and the YAML manifest are always written.

# bctu 0.16.2

## Fixes and improvements

* On-disk layout restored to the house convention: each report writes to
  `<path>/<after snapshot id>/v<version>/` (a folder per data state, then a
  folder per controlled document version; unversioned reports write to
  `<path>/<after snapshot id>/`). The report id string, manifest and workbook
  names keep the full `DVR-v<version>-<snapshot id>` identity from 0.16.1.

# bctu 0.16.1

## Fixes and improvements

* Per-site split: a finding that carries `site_col` as one of its own columns
  is sited from that column directly (row by row), falling back to the snapshot
  id-to-site lookup only where it is missing. Trials whose datasets have no
  single id column can now split per site without a snapshot-format change.
* Report identity: the id and output directory are keyed by the data validated,
  `<KIND>-v<version>-<after snapshot id>` (e.g. `DVR-v0.5-2026-08-13T111608Z`),
  carrying the controlled document version when given. The run moment stays in
  the manifest as `created_utc`. A second run on the same snapshot and version
  gets an explicit `_N`-suffixed directory, never a silent overwrite; an
  unsaved after snapshot falls back to the run time as its id.

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
