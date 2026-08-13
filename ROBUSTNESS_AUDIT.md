# bctu-ng — adversarial robustness / UX / stability / extensibility audit

Date: 2026-08-07. Branch `rebuild-hardening-features`. Method: ~75 adversarial
probes run against the installed package in the `bctu-runner` container
(`break_test.R`, `break_test2.R`, `break_test3.R`, `ledger_probe.R` in
`~/bctu-sandbox/`), plus three read-only agent audits (docs, extensibility,
old-vs-new) in `AUDIT_docs.md`, `AUDIT_extensibility.md`, `AUDIT_oldvsnew.md`.
Every claim below was verified by me against code or a live run.

## Verdict
Robust on the safety-critical paths. Of ~75 attempts to break it, the core held:
input guards, path-traversal rejection, hash-chain tamper-evidence, graceful
format degradation, and full type round-trips all passed. The defects are at the
edges (one real bug, one doc blocker, some ergonomics), not the spine.

## VERIFIED ROBUST (I tried to break these and could not)
- **Path traversal**: table names `../../evil`, `a/b`, `a\b`, control chars, `""`
  all rejected with a plain-English "Unsafe table name" error.
- **Tamper-evidence (the important one)**: editing a real ledger record
  (`T2`→`ZZ`) makes `verify_ledger()` return `ok=FALSE` at the right `seq`;
  recomputed SHA matches stored SHA on a clean chain. Payload tamper (appending a
  row to a snapshot CSV) makes `verify_snapshot()` return `ok=FALSE`. The hash
  chain is sound. (An earlier "verify passed after edit" was a flaw in my test —
  the ledger stores the datasource name `example`, not the project name I edited.)
- **Messy real-world types** (factor/Date/POSIXct/logical/unicode/NA) snapshot and
  load back with every class intact (via the canonical rds payload).
- **Extraction tag + tagged-NA** both survive a full save→load round-trip; coded
  columns come back `numeric` with `is_tagged_na()` TRUE (not coerced to character).
- **Garbage inputs** to `special_missing`, `report_table`, `run_dvp`, `save_snapshot`
  produce clean, actionable errors (dup tag, uppercase tag, non-formula, NULL data,
  unnamed DVP list, non-data.frame check result, wrong-class mapping, ...).
- **Git provenance never fails a snapshot**: `git="commit"` in a non-git store is
  silently skipped, snapshot still written.
- **Format degradation**: a failed `xpt` write warns clearly and leaves rds/csv/dta
  and the `.sas` script intact.

## DEFECTS (found by breaking it)

### 1. BUG — `delete_snapshot(id)` with no reason throws a raw R error
`delete_snapshot <- function(which, reason, store=..., ...)` — `reason` has no
default, so `delete_snapshot(id)` errors with `argument "reason" is missing, with
no default` BEFORE reaching the intended guard `cli_abort("reason is required")`
(snapshot.R:506). That guard is dead code for the commonest mistake. Violates the
house rule "errors are plain English". Fix: `reason = NULL` default, keep the guard.

### 2. DOC BLOCKER — README happy-path final call is wrong
README:97 `render_report(report, formats = c("docx","pdf"))` omits the REQUIRED
`output_dir` (report.R:150 has no default). A user copying the README verbatim
errors at the last step. The roxygen example and the test both pass `output_dir`;
only the README is stale.

### 3. UX GAP — no simple way to snapshot a data.frame you already have
`save_snapshot()` requires a `bctu_snapshot`; the only way to get one is a
datasource → `fetch_snapshot`. A non-R-expert who has read a CSV into `mydata`
cannot snapshot it without hand-writing `new_datasource("x", fetch=function(...)
list(t=mydata))`. For the stated audience this is real friction. Suggest a
one-liner convenience, e.g. `datasource_data(mydata)` or `snapshot_data(list(...))`.

### 4. EXTENSIBILITY — three closed switches where a registry belongs
(from `AUDIT_extensibility.md`, verified) Data plane is open (`new_datasource` +
`fetch_snapshot` generic — a user's datasource just works). Document plane is
closed:
- export formats: `write_snapshot_payload` is a closed if-ladder (snapshot.R:305);
  adding parquet touches ~3 core functions.
- report elements: `render_section` branches on the `$type` string, not S3 class
  (report.R:251) — the classes are decorative; adding an element needs a core edit.
- table renderers / output formats: hard-coded `if pdf latex else markdown`
  (report.R:260) and `match.arg(c("docx","pdf"))` (report.R:158).
- Undocumented cross-subsystem contract: SAS/tagged export keys off
  `attr(tbl,"bctu_special_missing")`, which a new datasource must set by hand but
  which is not in the `new_datasource` contract.

### 5. DOCUMENTATION
(from `AUDIT_docs.md`, verified) 35/65 exports have no `@example`;
`datasource_example()` (the ideal on-ramp) has no `@return`/`@example` and is not
in the README; only 3/65 have `@seealso` (near-zero discoverability — you can't
get from `save_snapshot` to `delete_snapshot`); ~30 internal plumbing functions
are exported into the same flat help index as the ~12 a user needs; no vignette.

### 6. NICE
- xpt export unreliable (haven "illegal character" on the long self-identifying
  filename) — degrades gracefully; the `.sas` script is the reliable path by design.
- A DVP whose check `stop()`s surfaces the raw message; could wrap as
  `check "<name>" failed: ...`.

## The "don't force proprietary folder formatting" question
NEW does **not** impose a folder LAYOUT. `bctu-project.yml` declares only a name
and a `snapshot_store` path (default `Data/Snapshots`, freely settable to `.` or
anywhere). The only fixed structure is *inside* a snapshot dir
(`<id>/tables/<name>/…`), which is the archive format itself. Crucially, because
`store` defaults to `snapshot_store(create=TRUE)` and R evaluates lazily, passing
an explicit `store=` to any snapshot call **bypasses the marker entirely** — no
project file required.

The one genuinely forced behaviour: with no marker AND no explicit `store=`, every
store call ERRORS ("bctu refuses to guess a location") rather than silently
writing under `getwd()`. That is a deliberate safety decision from the rebuild
(the OLD package's silent `getwd()/Data/Snapshots` fallback is exactly what it
replaced). It is tamper-safety, not folder-forcing — but it sits in slight tension
with "don't force people", so it is a decision point, not a defect.

## Differences from the previous package (verified highlights)
Full table in `AUDIT_oldvsnew.md`. OLD = 25 files/~6041 lines/60 exports; NEW =
10 files/~3536 lines/67 exports. A paradigm rebuild, not a port.

Location model: OLD silently fell back to `getwd()/Data/Snapshots`; NEW requires a
committed marker or an explicit `store=` and errors otherwise.

DROPPED capabilities (verified present in OLD, absent in NEW):
- **UoB branding**: no `theme_bctu_report`, `scale_*_uob`, palettes, portrait/
  landscape fig presets (only `report_figure()` → `ggsave`).
- **Baseline table builders**: `summarise_baseline`, `bl_msd`, `med_iqr`, `n_pct` — none.
- **User `.Rmd` / Word `trial_report()` rendering**: `inst/` is empty (0 files) —
  no reference.docx / Lua template; NEW renders in-package section objects only.
- **Credential writing/prompting**: `set_token`/`kill_token`/interactive prompt gone;
  NEW `credential_spec`/`resolve_credentials` are read-only.
- **Check primitives**: `check_by_regex`/`check_ranges`/`check_integer` — none.
- **Full session `checkpoint`** object + retrieval — NEW's `checkpoint()` is a 5-field stub.
- **`format_for_dta`** Stata name/label sanitisation + audit map — NEW writes `.dta`
  via haven directly, unsanitised.
- **REDCap-dictionary SAS `PROC FORMAT`** value labels — NEW's `.sas` script only
  recodes special-missings.
- Institutional presets (`datasource_redcap_bctu/_itm/_bistc`, `datasource_sql_bctu`).

NEW-in-rebuild: append-only audit ledger, `credential_spec`, `special_missing`/
`apply_special_missing`, native `sas7bdat`/`xpt`, LaTeX/PDF table+report path with
provenance manifests, multi-table SQL + full ODBC surface, `datasource_example`.

Some drops are the deferred backlog (baseline builders). Others — UoB theme, `.Rmd`/
Word rendering, `format_for_dta`, dictionary-based SAS value labels — are real
capabilities that need an explicit keep/drop decision.
