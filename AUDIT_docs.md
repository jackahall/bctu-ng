# Documentation audit: bctu package

Scope: DOCUMENTATION only, for a non-R-expert audience (clinical-trial
statisticians, not software engineers). Read-only, advisory. No package file was
edited.

Package: `/home/jack/projects/bctu-ng/bctu` (65 exported functions in NAMESPACE,
7 S3 methods, no vignette).

Method: repetitive checks were scripted, not eyeballed.
- `/tmp/exports.txt`: exported names from NAMESPACE.
- Rd-section presence per export (title/arguments/value/examples/dontrun):
  shell loop over `man/*.Rd` matched by `\alias{}` (output reproduced below).
- `@param` completeness: `/tmp/param_check.R` parses `R/*.R` (parse, not eval),
  compares each function's formals to its Rd `\item{}` names.
- README signatures cross-checked against `grep`'d function definitions.

---

## Priority summary

BLOCKER
1. README happy-path final step is broken: `render_report(report, formats = ...)`
   omits the required `output_dir` argument. A user following the README verbatim
   errors at the reporting step.

SHOULD-FIX
2. 35 of 65 exported functions have NO `@example` (list below). House rule is
   "the common task is one obvious call"; a non-expert cannot see how to call a
   function with no example.
3. `datasource_example()` (the deterministic, no-database on-ramp, the ideal way
   for a clueless user to try the package) is undocumented for use: no `@return`,
   no `@example`, and it is not mentioned anywhere in the README.
4. Discoverability is near zero: only 3 of 65 exports have `@seealso`. A user who
   finds `save_snapshot()` cannot find `delete_snapshot()`/`list_snapshots()`;
   `take_snapshot` has no link to `load_snapshot`/`verify_snapshot`, etc.
5. ~30 low-level plumbing functions are exported and sit in the same flat help
   index as the ~12 the user actually needs (see list). For this audience the
   export surface itself is a documentation problem.
6. `checkpoint()` is the thinnest doc in the package: title only, no description,
   no `@return`, no example.
7. Unwrapped examples depend on the Suggests package `withr`
   (`read_ledger`, `verify_ledger`, `list_snapshots` use `withr::local_tempdir()`).
   `example(read_ledger)` errors for a user who has not installed withr.

NICE
8. No getting-started vignette (see section 4).
9. 4 exports missing `@return`: `checkpoint`, `datasource_example`, `iso8601`,
   `list_snapshots`.
10. Many `@description` blocks just repeat the title (roxygen auto-fill):
    `checkpoint`, `datasource_example`, `iso8601`, `list_snapshots`,
    `snapshot_id`, and others.
11. DESCRIPTION `Title` ends "(Rebuild)" — an internal-process word that does not
    belong in a package title.

---

## 1. Exported-function completeness

Scripted Rd-section presence (Y = present). Full table:

```
FUNC                         TITLE ARGSEC VALUE EXAMP DONT
apply_special_missing        Y     Y      Y     -     -
bctu_config                  Y     Y      Y     Y     Y
bctu_init_project            Y     Y      Y     Y     Y
bctu_project                 Y     Y      Y     -     -
bctu_report                  Y     Y      Y     Y     -
bind_findings                Y     Y      Y     -     -
checkpoint                   Y     -      -     -     -
check_setup                  Y     Y      Y     Y     Y
compare_dvp                  Y     Y      Y     Y     -
credential_spec              Y     Y      Y     Y     -
datasource_example           Y     Y      -     -     -
datasource_redcap            Y     Y      Y     Y     Y
datasource_sql               Y     Y      Y     Y     Y
delete_snapshot              Y     Y      Y     Y     Y
fetch_snapshot               Y     Y      Y     -     -
finding_row_keys             Y     Y      Y     -     -
has_credential               Y     Y      Y     -     -
iso8601                      Y     Y      -     -     -
list_snapshots               Y     Y      -     Y     -
load_snapshot                Y     Y      Y     Y     Y
new_datasource               Y     Y      Y     -     -
nonempty_checks              Y     Y      Y     -     -
package_risk_report          Y     Y      Y     Y     Y
parse_snapshot_id            Y     Y      Y     -     -
read_ledger                  Y     Y      Y     Y     -
redcap_apply_labels          Y     Y      Y     -     -
redcap_field_names           Y     Y      Y     -     -
redcap_metadata              Y     Y      Y     -     -
redcap_parse_records         Y     Y      Y     -     -
redcap_perform               Y     Y      Y     -     -
redcap_request               Y     Y      Y     -     -
render_report                Y     Y      Y     Y     Y
render_table_gridtable       Y     Y      Y     -     -
render_table_latex           Y     Y      Y     -     -
render_table_markdown        Y     Y      Y     Y     -
report_figure                Y     Y      Y     Y     -
report_heading               Y     Y      Y     Y     -
report_paragraph             Y     Y      Y     Y     -
report_table                 Y     Y      Y     Y     -
report_table_data            Y     Y      Y     -     -
report_trial_name            Y     Y      Y     -     -
resolve_credentials          Y     Y      Y     -     -
resolve_finding_sites        Y     Y      Y     -     -
run_data_report              Y     Y      Y     -     -
run_dvp                      Y     Y      Y     Y     -
save_cdi                     Y     Y      Y     Y     Y
save_dvr                     Y     Y      Y     Y     Y
save_snapshot                Y     Y      Y     Y     Y
snapshot_date                Y     Y      Y     -     -
snapshot_fingerprint         Y     Y      Y     -     -
snapshot_id                  Y     Y      Y     Y     -
snapshot_store               Y     Y      Y     Y     Y
special_missing              Y     Y      Y     Y     -
sql_connection               Y     Y      Y     -     -
sql_discover_objects         Y     Y      Y     -     -
sql_guard_dataframes         Y     Y      Y     -     -
sql_odbc_arguments           Y     Y      Y     -     -
sql_read_object              Y     Y      Y     -     -
take_snapshot                Y     Y      Y     Y     Y
verify_ledger                Y     Y      Y     Y     -
verify_snapshot              Y     Y      Y     Y     Y
write_findings_readable      Y     Y      Y     -     -
write_findings_workbook      Y     Y      Y     -     -
write_report_set             Y     Y      Y     -     -
write_setup_report           Y     Y      Y     Y     Y
```

**Title**: all 65 have a title. Good.

**@param completeness**: no genuine gaps. `/tmp/param_check.R` flagged
`bind_findings`, `compare_dvp`, `write_report_set` (combined `\item{x, y}` /
`\item{before, after}` / `\item{id_col, site_col}` blocks — false positives, the
args are documented together) and `fetch_snapshot` (documents `verbose`, which
lives on the method not the generic — acceptable). Verified: every exported
function that has an `\arguments` section documents all its formals.
`checkpoint()` takes no arguments so its lack of an `\arguments` section is
correct.

**Missing @return (4)**: `checkpoint`, `datasource_example`, `iso8601`,
`list_snapshots`.
- `checkpoint.Rd` (R/snapshot.R): title only, no description, no value.
- `datasource_example.Rd`: no `@return` despite returning a datasource object the
  user then passes to `take_snapshot()`.
- `iso8601.Rd`, `list_snapshots.Rd`: return value obvious but undocumented.

**Missing @example (35 of 65)**:
`apply_special_missing`, `bctu_project`, `bind_findings`, `checkpoint`,
`datasource_example`, `fetch_snapshot`, `finding_row_keys`, `has_credential`,
`iso8601`, `new_datasource`, `nonempty_checks`, `parse_snapshot_id`,
`redcap_apply_labels`, `redcap_field_names`, `redcap_metadata`,
`redcap_parse_records`, `redcap_perform`, `redcap_request`,
`render_table_gridtable`, `render_table_latex`, `report_table_data`,
`report_trial_name`, `resolve_credentials`, `resolve_finding_sites`,
`run_data_report`, `snapshot_date`, `snapshot_fingerprint`, `sql_connection`,
`sql_discover_objects`, `sql_guard_dataframes`, `sql_odbc_arguments`,
`sql_read_object`, `write_findings_readable`, `write_findings_workbook`,
`write_report_set`.

Note several of these are user-facing enough to warrant an example on their own
merits, not just plumbing: `datasource_example`, `run_data_report`,
`write_report_set`, `sql_connection`, `resolve_credentials`, `has_credential`.

**Do the examples that exist actually run?** Verified by reading each non-wrapped
block. They are genuine and runnable, using values defined in the example:
- `special_missing(UNK ~ "a", ...)` (R/missing.R:26) — runs.
- `read_ledger`/`verify_ledger`/`list_snapshots` (R/snapshot.R:44,78,394) — build
  a `store <- withr::local_tempdir()` first, then call — run, BUT depend on the
  Suggests package `withr` (SHOULD-FIX #7).
- `snapshot_id(as.POSIXct(...))` (R/utils.R:25) — runs.
- `report_heading`/`report_paragraph`/`report_figure`/`bctu_report`/`report_table`/
  `render_table_markdown`/`run_dvp`/`compare_dvp` — all define their inputs inline
  and run. No undefined objects, no wrong arg names found.
- I/O- and network-touching examples are correctly wrapped: `\dontrun{}` for
  redcap/sql/config/render_report, `\donttest{}` at R/validation.R:358. Verified
  the wrapping table above (DONT column) against grep of
  `@examples|dontrun|donttest` in R/.

No example references an undefined object or a wrong argument name.

## 2. Plain-language / non-expert readability

Overall the prose is good and audience-appropriate: error messages are plain
English (cli aborts with "Declare at least one mapping...", "A DVP must be a
function of the data..."), and names are descriptive. Concerns:

- **Export surface is too large and undifferentiated for the audience.** Around
  30 of the 65 exports are internal plumbing exposed at the top level:
  `redcap_request`, `redcap_perform`, `redcap_parse_records`, `redcap_metadata`,
  `redcap_field_names`, `redcap_apply_labels`, `sql_connection`,
  `sql_discover_objects`, `sql_guard_dataframes`, `sql_odbc_arguments`,
  `sql_read_object`, `render_table_latex`, `render_table_gridtable`,
  `report_table_data`, `report_trial_name`, `resolve_credentials`,
  `resolve_finding_sites`, `finding_row_keys`, `nonempty_checks`, `bind_findings`,
  `new_datasource`, `fetch_snapshot`, `apply_special_missing`, `snapshot_date`,
  `snapshot_fingerprint`, `parse_snapshot_id`, `iso8601`, `snapshot_id`,
  `write_findings_readable`, `write_findings_workbook`. A non-expert opening the
  help index cannot tell these from the ~12 they need
  (`bctu_init_project`, `datasource_redcap`/`datasource_sql`/`datasource_example`,
  `take_snapshot`, `load_snapshot`, `verify_snapshot`, `list_snapshots`,
  `delete_snapshot`, `save_dvr`, `save_cdi`, `bctu_report`, `render_report`,
  `check_setup`). Fix: `@keywords internal` on the plumbing (or a pkgdown
  reference index grouping "Main functions" vs "Internals").
- Unexplained acronyms in titles/params with no expansion at first use: DVP, CDI,
  DVR, IQ (README `check_setup()` comment "(IQ)"). For the target reader these
  need a one-line expansion (Data Validation Plan, Critical Data Items, Data
  Validation Report, Installation Qualification) at least once, e.g. in the README
  or a concepts vignette.
- Titles that just restate the function name in dev-speak: `checkpoint` "A
  lightweight provenance checkpoint" with no description leaves a non-expert none
  the wiser about when to call it.

## 3. README end-to-end path

The README structure (Install -> happy path -> missing codes -> tag -> DVP/CDI ->
reporting -> environment -> design) is genuinely good for the audience, and the
export list it implies matches NAMESPACE (scripted: every `fn(` token in README
that is a package function exists as an export; the only non-matches are base/
remotes calls `c`, `list`, `subset`, `install_github`, `library`).

Defects:
- **BLOCKER — final step is wrong.** README:
  `render_report(report, formats = c("docx", "pdf"))`.
  Actual signature (R/report.R:150): `render_report(report, output_dir, formats = ...)`
  with `output_dir` REQUIRED (no default). The function's own roxygen example is
  correct (`render_report(report, output_dir = "reports", formats = "docx")`,
  R/report.R:147) and the test passes `output_dir = out_dir`
  (tests/testthat/test-report.R:63). So the README is the stale copy. A user
  following it hits "argument \"output_dir\" is missing, with no default" at the
  last step of the happy path.
- **SHOULD-FIX — no on-ramp without a live data source.** The whole README
  requires a real REDCap/SQL source with a token. `datasource_example()` exists
  precisely to let a user run the full snapshot->check->report loop with
  deterministic simulated data and no database, but it is never mentioned. Add a
  "Try it with no database" block using `datasource_example()`.
- NICE — the happy-path comment says "Load, verify, list, or delete snapshots"
  but shows no `delete_snapshot()` call; `delete_snapshot` requires a `reason`
  argument (R/snapshot.R:503) worth showing.
- NICE — `bctu_report(..., meta = snap)` passes a snapshot object as `meta`, while
  `render_report()` has a dedicated `snapshot =` argument and `bctu_report`'s own
  `@param meta` says "named list of extra metadata". Clarify which channel carries
  provenance so a user does not double-supply it.
- Install line `remotes::install_github("jackahall/bctu-ng", subdir = "bctu")`
  is UNVERIFIED: whether that GitHub repo is public/exists was not confirmed here
  (memory notes the GitHub remote was pending). To confirm:
  `gh repo view jackahall/bctu-ng`.

## 4. Getting-started narrative / vignette

None. `ls` shows no `vignettes/` directory and DESCRIPTION has no `VignetteBuilder`.
For this audience the README is currently the only narrative, and it is not
installed as a vignette (not available via `vignette(package = "bctu")` or
`browseVignettes`). This is a real gap.

A getting-started vignette should contain:
1. The concepts and their acronyms once, in plain English: project marker
   (`bctu-project.yml`), snapshot, store, manifest, audit ledger, DVP/CDI/DVR, IQ.
2. A runnable, no-credentials end-to-end walkthrough built on
   `datasource_example()`: init -> take_snapshot -> load/verify/list ->
   write a tiny DVP -> save_dvr -> build a report -> render_report (with a real
   `output_dir`, in `tempdir()`).
3. The missing-data-codes story (`special_missing`) with a runnable example.
4. The tagging/provenance story and what lands in the ledger.
5. A "which function do I actually call" map separating the ~12 main functions
   from the plumbing.

## 5. NEWS.md and DESCRIPTION

NEWS.md: accurate against the current API. It references `special_missing()`,
`take_snapshot`/`save_snapshot` `tag=`/`labels=`, the new formats, the restored
hash-chained ledger with `read_ledger()`/`verify_ledger()`, `snapshot_store()`
replacing the removed `snapshot_location()`, `datasource_redcap()` `service=` —
all consistent with NAMESPACE and the signatures verified. No stale claims found.
Single-version file (1.0.0.9000) so no historical drift to check.

DESCRIPTION:
- `Title`: "BCTU Clinical-Trial Data Management (Rebuild)" — drop "(Rebuild)"; a
  parenthetical process note does not belong in a package title (and `R CMD check`
  will note title style).
- `Description`: clear and accurate; mentions the DTA/immutable-snapshot design well.
- Imports (`cli`, `digest`, `yaml`) vs Suggests: correct. Every optional backend
  (`DBI`, `odbc`, `RSQLite`, `httr2`, `keyring`, `readr`, `haven`, `openxlsx`,
  `ggplot2`, `rmarkdown`, `tinytex`, `riskmetric`, `renv`, `sessioninfo`,
  `tibble`, `withr`) is a Suggests, matching a "resolve backend on demand" design.
  One doc-adjacent caveat: because unwrapped examples use `withr` (Suggests),
  either move `withr` to Imports or guard those examples; a user without withr
  cannot run `example(read_ledger)`.

## 6. Cross-references and discoverability

Weak. Scripted `@seealso` count: 3 of 65 exports
(`datasource_redcap`, `datasource_sql`, `sql_connection`). Specific gaps a
non-expert will hit:
- The snapshot family is not interlinked: `save_snapshot`/`take_snapshot` do not
  point to `load_snapshot`, `list_snapshots`, `verify_snapshot`, `delete_snapshot`,
  `snapshot_store`. A user who found one cannot navigate to the others.
- The DVP/CDI family (`run_dvp`, `compare_dvp`, `save_dvr`, `save_cdi`,
  `run_data_report`, `write_findings_workbook`) is not cross-linked.
- The report family (`bctu_report`, `report_heading`/`paragraph`/`table`/`figure`,
  `render_report`, `render_table_*`) is only partly linked: `bctu_report`'s
  `@param sections` uses `[report_heading()]` etc. inline (good), but there is no
  `@seealso` binding `render_report` back to `bctu_report`.
- To the specific question: a user at `save_snapshot` CANNOT currently find
  `delete_snapshot` — no `@seealso`, no inline link between them.
  Recommend an `@seealso` (or shared `@family`) on each functional cluster:
  snapshots, credentials/datasources, validation, reporting.

---

## Reproduce

- `/tmp/exports.txt` — exported names.
- Rd-section table: the shell loop in this audit over `man/*.Rd`.
- `/tmp/param_check.R` — `Rscript /tmp/param_check.R` (formals vs documented args).
- README signature check: `grep -nP '^(save_dvr|...) <- function' R/*.R`.
- Broken README call confirmed against R/report.R:150 (`output_dir` required),
  R/report.R:147 (correct example), tests/testthat/test-report.R:63.
