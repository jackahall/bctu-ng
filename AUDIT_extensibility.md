# bctu extensibility & architecture audit

Scope: can a competent R user EXTEND the package without editing its internals?
Read-only audit of `/home/jack/projects/bctu-ng/bctu/R/*.R`. Verdicts are per axis,
each with file:line evidence read from the dispatch code itself (not docstrings).

## Verdict summary

| Axis | Verdict | Priority |
|---|---|---|
| 1. New data source | **OPEN** | — |
| 2. New export format (snapshot payload) | **needs-core-edit** (closed if-ladder) | SHOULD-FIX |
| 3. New check type (DVP) | **OPEN** (with documented positional-key caveat) | — |
| 4a. New report element (heading/para/table/figure) | **needs-core-edit** (type-string switch) | SHOULD-FIX |
| 4b. New table renderer / report output format | **awkward** (writable, not pluggable) | SHOULD-FIX |
| 5. S3 design overall | Mixed: consistent for print/validate, absent at the real extension points | SHOULD-FIX |
| 6. Coupling / global state | No global state anywhere (strong); one undocumented attribute contract | NICE |

No BLOCKERs. The two data-plane extension points users actually care about (new
source, new check) are genuinely open. The two document-plane extension points
(new payload format, new report element/renderer) are closed switch/if-ladders.

---

## 1. New data source — OPEN

`new_datasource()` is a true open composition mechanism, not a fixed enum.

- `new_datasource(type, fetch, creds, config, test, label)` builds an object of
  class `c(paste0("datasource_", type), "datasource")` (`datasource.R:27-31`). The
  behaviour lives entirely in the `fetch` closure the caller supplies; there is no
  per-type method to implement.
- `fetch_snapshot()` is a proper generic (`datasource.R:134 UseMethod`) with a
  single method `fetch_snapshot.datasource` (`:138`) that dispatches on the **base**
  class, so a user's `datasource_mydb` inherits it with zero new methods.
- `datasource_redcap` (`sources.R:114`) and `datasource_sql` are themselves just
  thin presets over `new_datasource()`, confirming the intended extension pattern.
  `datasource_example` (`datasource.R:207`) is the same.

**Contract the object's `fetch` must satisfy:** `function(creds, config, verbose, ...)`
returning a **named list of data frames**, one per table. Enforced at runtime by
`validate_tables()` (`datasource.R:147-155`): a bare data frame is auto-wrapped as
`list(records = ...)`; anything else, or unnamed/empty-named elements, aborts.

**Is the contract documented?** Yes, in three places: the `@param fetch` tag
(`datasource.R:14-16`), the file header (`:4-9`), and `fetch_snapshot`'s roxygen.
Good.

**Worked example (no internals touched):**
```r
datasource_mydb <- function(dsn, query, token_id) {
  fetch <- function(creds, config, verbose = 2L, ...) {
    con <- DBI::dbConnect(odbc::odbc(), dsn = config$dsn, pwd = creds)
    on.exit(DBI::dbDisconnect(con))
    list(records = DBI::dbGetQuery(con, config$query))
  }
  new_datasource("mydb", fetch = fetch,
                 creds  = credential_spec(token_id),
                 config = list(dsn = dsn, query = query, name = "MyDB"))
}
take_snapshot(datasource_mydb("PROD", "select * from crf", "mydb"))  # just works
```
This plugs into `take_snapshot` / `fetch_snapshot` / `save_snapshot` cleanly. The
credential machinery (`credential_spec`/`resolve_credentials`) is reused as-is.

Only gap (see axis 6): if the new source wants special-missing handling, it must
set `attr(records, "bctu_special_missing")` by hand — the redcap source does this
(`sources.R:104`) but the attribute contract is not documented for extenders.

---

## 2. New export format — needs-core-edit (closed if-ladder)

The payload writer is a **closed sequence of hard-coded `if` blocks**, not a
registry.

- `write_snapshot_payload()` (`snapshot.R:305-340`) is literally
  `if ("rds" %in% formats){...}`, `if ("csv" ...){...}`, `if ("dta" ...)`,
  `if ("sas7bdat" ...)`, `if ("xpt" ...)`, `if ("sas" ...)`. Each format is
  inlined; there is no lookup table of `format -> writer`.
- `haven_write()` compounds it with a closed `switch(kind, dta=, sas7bdat=, xpt=)`
  (`snapshot.R:349`).
- The advertised format set is duplicated in prose in **two** roxygen blocks
  (`take_snapshot` `:178-179`, `save_snapshot` `:204-205`) and in the load path,
  `load_snapshot`'s `read_one()` only knows `rds`/`csv` (`snapshot.R:433-441`).

**To add `parquet`, a user must edit core:** add an `if ("parquet" %in% formats)`
block in `write_snapshot_payload`, update the two `@param formats` docs, and
(if they want it readable back) extend `read_one`. Minimum one core function,
realistically three. There is no seam to register a writer from outside.

Fix shape (not requested, logged): a `snapshot_writers` named list of
`function(tbl, path) -> logical`, iterated over `intersect(formats, names(...))`,
would make this OPEN and let a user do
`register_snapshot_writer("parquet", \(tbl, p) arrow::write_parquet(tbl, p))`.

---

## 3. New check type (DVP) — OPEN

A DVP is genuinely open: `run_dvp(dvp, data)` (`checks.R:37`) accepts **any**
`function(data)` and only requires the return to be a named list whose elements are
data frames (or `NULL`/empty) (`:45-63`). No columns, names, or shapes are assumed
of an individual finding frame — the header comment's "nothing about the shape of a
finding is fixed" (`checks.R:6-9`) matches the code.

**Hidden schema assumptions — checked, and they are opt-in, not baked into the
check:**
- `compare_dvp` (`checks.R:134`) keys findings **whole-row positionally** via
  `finding_row_keys` (`:78-86`), so a check must return the same columns in the same
  order across `before`/`after`. This IS documented (`:118-121`) and is a property
  of the differ, not a constraint the check author can violate silently — a
  reordered check just reads as all-changed. Acceptable and disclosed.
- Per-site splitting (`resolve_finding_sites` `:204`, `write_report_set` `:312`)
  assumes an `id_col` (default `"record_id"`) present in the finding rows. But this
  is entirely opt-in: `site_col = NULL` by default (`run_data_report:429`), and both
  `id_col`/`site_col` are parameters. A check that doesn't emit `record_id` simply
  can't be site-split, and rows fall to `NO_SITE` rather than being dropped
  (`:218`). No silent failure.

**Worked example:** any `function(data) list(my_check = <df>)` passed to
`run_dvp`, `save_dvr`, or `save_cdi` works with no core change. Verdict OPEN.

---

## 4a. New report element — needs-core-edit (type-string switch)

Section objects are S3-classed (`bctu_report_heading`, `_paragraph`, `_figure`,
all `bctu_report_section`; `report.R:29-30, 41-42, 64-66`) **but rendering does not
dispatch on class.** `render_section()` (`report.R:248-256`) does:

```r
switch(section$type,
  heading = ..., paragraph = ..., figure = ...,
  cli::cli_abort("Unknown section type: ..."))
```

It branches on the `$type` **string**, so the S3 classes are decorative at render
time. The same type-string switch is repeated in `print.bctu_report`
(`report.R:108-110`) and `build_report_manifest` (`report.R:322-323`).

**To add a `pagebreak` / `callout` / `listing` element, a user must edit core:**
`render_section` (and, for a clean result, the two other switch sites). There is no
way to introduce a new section type from outside the package. SHOULD-FIX: make
section rendering `UseMethod("render_section")` so the existing S3 classes carry
real dispatch and a user can add `render_section.my_callout()`.

## 4b. New table renderer / new report output format — awkward

Two sub-problems:

- **Table renderers are writable but not pluggable.** `render_table_markdown`
  (`table.R:212`) and `render_table_latex` (`table.R:367`) are exported standalone
  functions that both consume the open, documented `bctu_report_table` object — so a
  user CAN write `render_table_html(x)` against a stable contract. But nothing in the
  render path will call it: `render_table_section()` (`report.R:260-264`) hard-codes
  `if (format == "pdf") latex else markdown`. The renderer set is a binary if, not a
  registry.
- **Output formats are a closed enum.** `render_report` does
  `match.arg(formats, c("docx","pdf"))` (`report.R:158`) and hard-wires pandoc +
  xelatex (`:159-165`). Adding HTML/RTF is a core edit to `render_report`,
  `run_pandoc`, and `render_table_section`.

So a user can build a `bctu_report_table` and render it to their own format by hand,
but cannot make `render_report(report, formats = "html")` use it. SHOULD-FIX.

---

## 5. S3 design overall — mixed

**Consistent where it's used for identity/printing.** Seven `print` methods, one per
public object (`datasource`, `bctu_snapshot`, `bctu_report`, `bctu_report_table`,
`bctu_special_missing`, `bctu_setup_qualification`), all registered in NAMESPACE and
all `invisible(x)`-returning. `fetch_snapshot` is a correct generic/method pair
dispatching on the base `datasource` class — the one exemplary extension seam.

**Absent exactly where extension needs it.** The three places a user would want to
extend all type-switch instead of dispatch:
- section rendering switches on `$type` string despite the objects being S3-classed
  (`report.R:251`) — the clearest "if/else where S3 would be cleaner" case;
- table rendering branches on a `format` string (`report.R:260`) rather than
  dispatching a renderer;
- payload writing is an if-ladder over `formats` (`snapshot.R:305-340`).

Net: S3 is used as a validation/printing convention, not as the package's extension
mechanism, except for `fetch_snapshot`. Discoverability of dispatch is fine
(everything is in NAMESPACE, no hidden `.dot` helpers per house rule), but a reader
would reasonably expect the classed section objects to be dispatch targets and find
they are not.

---

## 6. Coupling / global state

**Global state: none — a genuine strength.** `grep` for `getOption`/`options(`
across `R/*.R` returns nothing. All location is anchored to the committed
`bctu-project.yml` marker resolved by `bctu_project`/`snapshot_store`
(`config.R:65,114`); there is no ambient option, no walk-from-cwd dotenv, no global
registry. This matches the CLAUDE.md "explicit configuration, no ambient state"
rule and means one extender's configuration cannot leak into another's.

**Cross-subsystem reach — one place, and it's an undocumented attribute contract.**
`snapshot.R` reaches into `missing.R` internals: `restore_codes_frame`
(`snapshot.R:313,332`), `retag_upper` (`:322,326`), `sas_import_script` (`:336`). All
three key off `attr(tbl, "bctu_special_missing")`. Within the package this is fine,
but it is an implicit contract for extenders: a **new datasource** that wants its
missing-codes rendered into CSV/SAS/Stata exports must set that attribute on each
table itself (as `datasource_redcap` does at `sources.R:104`). This is not stated in
the `new_datasource` contract. NICE-to-fix: document "to get special-missing exports,
attach `attr(., 'bctu_special_missing') <- special_missing(...)` to your fetched
tables", or expose a helper the source can call.

**Report/snapshot coupling** is via documented attributes (`attr(x,"id")`,
`"bctu_tag"`, `"bctu_meta")`; `report.R:293-300`, `checks.R:183-186`), read
defensively with `%||%` fallbacks. No fragile internal reach there.

---

## Prioritised actions (advisory — nothing changed)

- SHOULD-FIX: turn the payload-format if-ladder (`snapshot.R:305-340`) into a
  writer registry so `parquet`/other formats plug in without a core edit.
- SHOULD-FIX: make `render_section` dispatch S3 on the already-existing section
  classes (`report.R:248-256`) so new report elements need no core edit.
- SHOULD-FIX: make report output formats + table renderers a registry rather than
  `match.arg(c("docx","pdf"))` + `if(format=="pdf")` (`report.R:158,260`).
- NICE: document the `bctu_special_missing` attribute as part of the datasource
  `fetch` contract so a new source can opt into special-missing exports.
