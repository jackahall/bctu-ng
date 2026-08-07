# bctu 1.0.0.9000

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
  tables, in the manifest and the audit ledger, and flows into the DVR/CDI and
  report manifests. With `git = "commit"` (the default when a tag is given), bctu
  records the repository HEAD, commits the snapshot metadata (manifest + ledger,
  never the payload), and creates an annotated `snap/<tag>` git tag.
* New snapshot payload formats: `"dta"`, `"sas7bdat"`, `"xpt"`, and `"sas"` (an
  import script), alongside `"rds"` and `"csv"`.

## Audit trail and safety

* Restored the append-only, hash-chained audit ledger (`SNAPSHOTS.log.yml`) at the
  store root. Every take and every delete (including `mode = "destroy"`) appends a
  metadata-only record before anything is removed, so a destroyed snapshot still
  leaves a trace. `read_ledger()` and `verify_ledger()` inspect and check it.
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
