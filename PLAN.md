# bctu-ng hardening + features — execution plan

Branch: `rebuild-hardening-features` (off master @ 6e2b4a1). Gate after each phase:
container `R CMD check` + testthat green. One commit per phase. Design of record:
`~/projects/bctu/BCTU_REVIEW_AND_REBUILD_PLAN.md`. Full audit reports:
`<scratchpad>/audit-*.md`.

Container gate:
`docker run --rm -v /home/jack/projects/bctu-ng/bctu:/pkg -v /home/jack/bctu-sandbox:/sandbox bctu-runner:latest <cmd>`
(`_roxygenise.R`, `_check.sh`, `_testrun.sh`).

## Locked decisions (Jack, 2026-08-07)
- Scope = now-actionable fixes + RESTORE the append-only ledger. DEFER parquet
  payload, valtools layer-b scaffolding, summary/baseline table builders, NIST checks.
- Feature B git: bctu commits snapshot METADATA (manifest + ledger, never payload)
  into git, records HEAD SHA, creates an annotated tag, and commits.
- Feature A SAS: write `.xpt` + `.sas7bdat` via haven BEST-EFFORT (wrapped, non-fatal,
  haven's SAS writer is unstable); ALWAYS also generate a `.sas` import script that
  reads the CSV and recodes declared codes to native special missings. The script is
  the reliable deliverable.

## Verified facts (haven 2.5.5, in-container — do not re-litigate)
- Stata `.dta`: tagged NA round-trips with LOWERCASE tags a-z only.
- SAS `.sas7bdat`/`.xpt`: tagged NA round-trips with UPPERCASE tags A-Z (and `_`) only.
- haven normalises tags to lowercase on read => canonical internal tag = lowercase a-z.
- `labelled()` + `tagged_na()` coexist and both survive a dta round-trip.

---

## Feature A — missing-data codes (special_missing)

`special_missing(UNK ~ "a", OTH ~ "b", ...)` (LHS symbol or string; RHS single a-z).
Returns a `bctu_special_missing` object: a data.frame(code, tag) + validation.
Passed to `datasource_redcap(..., missing_codes = special_missing(...))`; applied at
parse time inside the REDCap fetch (Jack confirmed extraction-time).

Apply algorithm (`apply_special_missing(records, mapping)`), source-agnostic:
1. Read REDCap CSV as ALL character (guarantee no premature coercion).
2. Per column: `is_code <- trimws(cell) %in% mapping$code` (whole-cell exact).
3. Blank the coded cells; run readr type-conversion on the cleaned frame (reproduces
   "read_csv variable type formatting" Jack asked for).
4. If the cleaned column is numeric (double/integer): coerce to double, set
   `haven::tagged_na(tag)` at the coded positions. Attach per-column code->tag map as
   an attribute for export reconstruction.
5. If the cleaned column is character or Date (tagged NA is double-only): LEAVE the
   original code string in place, and WARN (special missing applies to numeric fields).
   Record these in a report so the operator sees which fields could not be tagged.
6. Store the whole mapping as `attr(records, "bctu_special_missing")` so exports and
   the SAS script can reconstruct.

Export (snapshot layer, new formats): for each requested format
- `rds`: native (tagged NA preserved).
- `csv`: restore ORIGINAL code strings into tagged cells (human-readable exchange).
- `dta`: `haven::write_dta` as-is (lowercase tags -> Stata `.a`).
- `sas7bdat`/`xpt`: re-case tags to UPPERCASE (`retag_upper`) then best-effort
  `write_sas`/`write_xpt` wrapped in tryCatch; on failure warn, do not abort.
- `.sas` script: always generate `<study>_<table>_import.sas` that PROC IMPORTs / data-
  steps the CSV, sets types, and `IF var IN (...) THEN var = .A;` recodes per the mapping.

Edge decisions: character/Date fields keep the raw code + warn (can't be SAS special
missing). Tags restricted to single lowercase a-z (covers 26; works both targets).

## Feature B — extraction tagging

`take_snapshot(source, tag = NULL, labels = NULL, ...)` / `save_snapshot(...)`.
- `tag`: short slug (e.g. "DMC-2026-08"); `labels`: optional named list of free metadata.
- Stored in `bctu_meta$tag`/`$labels`, written to the manifest, and set as
  `attr(snapshot,"bctu_tag")` AND on each table data.frame (`attr(tbl,"bctu_tag")`).
- Carried into DVR/CDI manifests (checks.R) and report manifests (report.R) wherever a
  snapshot is the input, so a DMC extraction is traceable end-to-end.
- Git (new `commit_snapshot_metadata(snapshot, repo, tag)`): stage manifest + ledger
  (metadata only), `git commit`, record HEAD SHA into the manifest, create annotated
  `git tag snap/<tag>` (or `snap/<id>` if no tag). Guarded: no-op with a clear message
  if not in a git repo; never commits payload; opt-in via `git = TRUE` arg (default TRUE
  when a repo is detected, but never fails the snapshot if git absent).

---

## Phases + DAG

P0 Foundation/safety (SERIAL; config.R + snapshot.R) -> gate -> commit
  - config: snapshot verbs default to erroring resolver; make `snapshot_location` the
    project-or-error resolver (drop silent cwd fallback -> error with hint, per plan §2);
    abort on dir.create failure; validate marker `name`/`snapshot_store` (single non-NA
    non-empty string); wrap YAML read in plain-English error; announce exact returned path.
  - snapshot: restore append-only `SNAPSHOTS.log.yml` ledger at store root (append on take
    + on delete incl destroy, BEFORE unlink); sanitise table names (reject `..`/sep);
    `-NN` suffix via dir.create return (stop second-bump); atomic manifest write (temp+rename);
    verify empty->ok=FALSE; explicit missing-payload error; always-warn on csv fallback.

P1 Datasource + Feature A (sources.R, datasource.R, new missing.R, snapshot.R export)
    depends on P0 -> gate -> commit
  - REDCap error-body guard; guard httr2/DBI/odbc with requireNamespace + install hint;
    friendly connect/perform errors; warn empty export; wire per-source `service`;
    simplify has_credential; `datasource_http` preset (or remove stale claim).
  - special_missing + apply + snapshot dta/sas/xpt/.sas-script export with tag re-casing.

P2 Feature B tagging (snapshot.R, checks.R, report.R) depends on P0/P1 -> gate -> commit

P3 Defect fixes (PARALLEL disjoint: checks.R | table.R+report.R | validation.R)
    depends on P0 -> gate -> commit
  - checks: resolved->NO_SITE via before∪after site map; dedup readable filenames;
    warn per no-site finding; NA sentinel in row key; compare_dvp doc fix.
  - table/report: escape `|`/newline in grid; fix latex backslash ordering; zero-row
    guard; xelatex precheck + plain error; write markdown UTF-8; dedup formats; unique
    figure filenames; document template=DOCX-only + data_cut_date semantics.
  - validation: `fatal = isTRUE(spec$required)`; guard tibble; reword IQ/OQ over-claim to
    match code (or add a tiny render probe); capture renv lock hash if cheap.

P4 Docs + tests (PARALLEL) depends on P1/P2/P3 -> gate -> commit
  - tests: create test-sources.R + test-table.R; FAIL/tamper/edge coverage each lane;
    feature A (codes->tags->dta/sas/csv/.sas) + feature B (tag propagation) tests.
  - docs: @examples on every exported fn; README.md; NEWS.md; vignettes (getting-started,
    add-a-source, missing-data-codes); reconcile snapshot header + CLAUDE.md re ledger.

## Status
- [x] Branch created, plan written, haven verified.
- [x] P0  [ ] P1  [ ] P2  [ ] P3  [ ] P4
