# bctu OLD vs NEW — Difference Audit

Read-only, advisory. Compares the reference package
`/home/jack/projects/bctu/bctu/R/` (OLD, v0.14.0) against the rebuild
`/home/jack/projects/bctu-ng/bctu/R/` (NEW). Every claim is anchored to code
with `file:line`. No files were edited.

OLD: 25 R files, ~6041 lines, 60 exports.
NEW: 10 R files, ~3536 lines, 67 exports.

---

## 1. FEATURE PARITY TABLE

Status key: **present** (same capability), **renamed**, **redesigned**
(same goal, materially different API/behaviour), **DROPPED** (no equivalent),
**new-in-rebuild**.

### Configuration / location resolution
| OLD capability | file:line | NEW | Status |
|---|---|---|---|
| `snapshots_dir()` option→dotenv→cwd resolution | `snapshot-dirs.R:101` | `snapshot_store()` marker-relative, errors if no marker | redesigned |
| `set_snapshots_dir()` writes `.bctu.env`, sets sticky option | `snapshot-dirs.R:132` | none; declare `snapshot_store` in `bctu-project.yml` | redesigned/DROPPED (no setter) |
| (none) | — | `bctu_init_project()` writes marker | `config.R:28` | new-in-rebuild |
| (none) | — | `bctu_project()` / `bctu_config()` read+print resolved config | `config.R:65,138` | new-in-rebuild |

### Machine / package qualification
| OLD | file:line | NEW | Status |
|---|---|---|---|
| `bctu_doctor(file, verbose)` PASS/WARN/FAIL table + IQ record | `doctor.R:24` | `check_setup()` IQ table + `write_setup_report()` YAML | `validation.R:362,442` | renamed + redesigned |
| (none) | — | `package_risk_report()` (riskmetric wrapper) | `validation.R:507` | new-in-rebuild |

### Credentials / tokens
| OLD | file:line | NEW | Status |
|---|---|---|---|
| `get_token()` keyring→env→interactive-prompt | `token.R:75` | `resolve_credentials()` keyring→env (no prompt) | `datasource.R:78` | redesigned |
| `has_token()` | `token.R:158` | `has_credential()` | `datasource.R:110` | renamed |
| `set_token()` store in keyring | `token.R:64` | none | **DROPPED** |
| `kill_token()` delete from keyring | `token.R:132` | none | **DROPPED** |
| `print.bctu_token` masked print | `token.R:179` | none (no token object) | **DROPPED** |
| (none) | — | `credential_spec()` declares where a secret lives | `datasource.R:59` | new-in-rebuild |

### Checkpoints / provenance object
| OLD | file:line | NEW | Status |
|---|---|---|---|
| `checkpoint(trial, analysis, notes, print)` full session object (session id, script detection, git state, sessionInfo, pkg versions) | `checkpoint.R:266` | `checkpoint()` 5-field list (created_utc, r_version, bctu_version, user, host) | `snapshot.R:165` | redesigned (heavily reduced) |
| `get_checkpoint()` retrieve from snapshot | `checkpoint.R:350` | none | **DROPPED** |
| `print.checkpoint()` audit summary | `checkpoint.R:370` | none | **DROPPED** |

### Snapshot workflow
| OLD | file:line | NEW | Status |
|---|---|---|---|
| `take_snapshot()` | `snapshot-save.R` (generic) | `take_snapshot()` | `snapshot.R:193` | present (redesigned internals) |
| `fetch_snapshot()` generic + redcap/sql methods | `snapshot-fetch.R` | `fetch_snapshot()` generic + `.datasource` method | `datasource.R:134` | present (redesigned dispatch) |
| `save_snapshot()` (+ `.default` saveRDS) | `snapshot-save.R` | `save_snapshot()` (bctu_snapshot only) | `snapshot.R:223` | present; `.default` arbitrary-object save **DROPPED** |
| `load_snapshot()` selectors incl checkpoint/POSIXt | `snapshot-load.R` | `load_snapshot()` selectors latest/penult/id/int, `table=` | `snapshot.R:428` | redesigned (fewer selectors, +table) |
| `list_snapshots()` | `snapshot-load.R` | `list_snapshots()` | `snapshot.R:399` | present |
| `verify_snapshot()` full/quick, git-trailer compare, deletion-aware | `snapshot-verify.R` | `verify_snapshot()` SHA-vs-manifest only | `snapshot.R:464` | redesigned (reduced) |
| `verify_snapshots()` store sweep | `snapshot-verify.R` | none | **DROPPED** |
| `delete_snapshot()` retire/destroy, git record-first | `snapshot-delete.R:50` | `delete_snapshot()` retire/destroy, ledger record-first | `snapshot.R:503` | redesigned (ledger, not git commit) |
| `resolve_snapshot()` reuse-or-refetch by age | `trial_helpers.R:23` | none | **DROPPED** |
| `snapshot_date()` from attached checkpoint | `trial_helpers.R:86` | `snapshot_date()` from snapshot id, tz Europe/London | `utils.R:65` | redesigned |
| (none) | — | audit ledger: `read_ledger`/`verify_ledger` (append-only, hash-chained) | `snapshot.R:49,83` | new-in-rebuild |
| (none) | — | `snapshot_id`/`parse_snapshot_id`/`snapshot_store`/`snapshot_fingerprint` | various | new-in-rebuild |

### Data sources
| OLD | file:line | NEW | Status |
|---|---|---|---|
| `datasource_redcap()` | `datasource.R:67` | `datasource_redcap()` (multi-helper, missing_codes) | `sources.R:66` | present (redesigned) |
| `datasource_redcap_bctu/_itm/_bistc()` URL presets | `datasource.R:130,150,170` | none | **DROPPED** |
| `default_redcap_url` constant | `datasource.R:63` | none | **DROPPED** |
| `datasource_sql()` single `table` | `datasource.R:190` | `datasource_sql()` multi-table / discovery | `sources.R:370` | redesigned |
| `datasource_sql_bctu()` server preset | `datasource.R:234` | none | **DROPPED** |
| (none) | — | `new_datasource()` composition base | `datasource.R:21` | new-in-rebuild |
| (none) | — | `datasource_example()` simulated source | `datasource.R:177` | new-in-rebuild |
| (none) | — | `sql_connection`, `sql_odbc_arguments`, `sql_discover_objects`, `sql_read_object`, `sql_guard_dataframes` | `sources.R:308–474` | new-in-rebuild |
| (none) | — | `redcap_request/perform/metadata/field_names/parse_records/apply_labels` (exported) | `sources.R` | new-in-rebuild |

### REDCap labelling
| OLD | file:line | NEW | Status |
|---|---|---|---|
| `redcap_labelled(data, clean_value_labels)` attr-driven (dictionary+fieldnames) | `redcap.R:138` | `redcap_apply_labels(records, dictionary)` dictionary-driven | `sources.R:238` | redesigned |
| `dictionary()` / `set_dictionary()` attach/read metadata | `redcap.R:71,80` | none (dictionary attached automatically at fetch) | **DROPPED** |
| `make_format_matrix()` (internal) | `redcap.R:103` | `redcap_parse_choices()` (internal) | `sources.R:265` | renamed |
| `clean_value_labels` / HTML+brace stripping | `redcap.R:131,219` | none | **DROPPED** |

### DVP / DVR / CDI checks
| OLD | file:line | NEW | Status |
|---|---|---|---|
| `save_checks()` S3 generic (df/snapshot/default) | `checks.R` | none (generic removed) | **DROPPED (name)** |
| `save_dvr()` / `save_cdi()` | `checks.R` | `save_dvr()` / `save_cdi()` | `checks.R:391,412` | present (redesigned) |
| `since`-selector diff (default `"penultimate"`) | `checks.R` (save_checks) | explicit `before`/`after` snapshots | `checks.R:391,134` | redesigned |
| check_fn returns named list of df | `checks.R` | `run_dvp()` validates same contract | `checks.R:37` | present |
| `check_by_regex/check_ranges/check_integer/stock_numeric_regex` primitives | `check_helpers.R:40–172` | none | **DROPPED** |
| site split via `site_id` column in check output | `checks.R` | `resolve_finding_sites()` looks up site from snapshot | `checks.R:204` | redesigned |
| xlsx workbooks (openxlsx) | `checks.R` | `write_findings_workbook()` (optional) + always CSV/TXT | `checks.R:259,233` | redesigned (Excel now optional) |
| (none) | — | `compare_dvp`, `bind_findings`, `finding_row_keys`, `nonempty_checks`, `run_data_report`, `write_report_set`, `report_trial_name` | `checks.R` | new-in-rebuild |

### Report tables
| OLD | file:line | NEW | Status |
|---|---|---|---|
| `render_table()` grid table w/ autoformat, full_width | `report_table.R:261` | `report_table()` object + `render_table_markdown()` | `table.R:48,212` | redesigned |
| `grid_table()` low-level builder | `report_table.R:220` | `render_table_gridtable()` (alias) | `table.R:300` | renamed |
| grouped header (`super`) | `report_table.R:178` | `group_headers` | `table.R:137` | present |
| banner rows (`span_rows`) | `report_table.R:143` | `banner_rows` | `table.R:158` | present |
| row-merge (`merge_cols`+`block`) | `report_table.R:158,220` | none | **DROPPED** |
| in-cell line-break multi-line cells | `report_table.R:116` | collapsed to a space | `table.R:323` | **DROPPED** |
| `full_width` narrow-table stretch | `report_table.R:295` | none | **DROPPED** |
| autoformat nbsp-indent detection/bolding | `report_table.R:270` | none | **DROPPED** |
| `n_pct()` / `indent()` / `stack_indented()` | `report_table.R:68,80,91` | none | **DROPPED** |
| `col_widths()` | `report_table.R:249` | internal only | **DROPPED (export)** |
| (none) | — | `render_table_latex()` PDF path + `report_table_data()` | `table.R:367,86` | new-in-rebuild |

### Baseline tables
| OLD | file:line | NEW | Status |
|---|---|---|---|
| `summarise_baseline()` grouped baseline builder | `report_baseline.R:105` | none | **DROPPED** |
| `bl_msd/med_iqr/gest_med_iqr/bl_levels()` formatters | `report_baseline.R:57–100` | none | **DROPPED** |

### UoB theming / figures
| OLD | file:line | NEW | Status |
|---|---|---|---|
| `uob_palettes` + `uob_tint` + `pal_uob` | `uob_scales.R:14,42,62` | none | **DROPPED** |
| `scale_color/fill_uob(_c)` (+ `colour` aliases) | `uob_scales.R:85–118` | none | **DROPPED** |
| `show_uob_palettes()` | `uob_scales.R:127` | none | **DROPPED** |
| `theme_bctu_report()` | `report_theme.R:16` | none | **DROPPED** |
| `fig_portrait()` / `fig_landscape()` + knitr `opts_template` registration | `fig.R:22,28`; `zzz.R` | none (no `zzz.R`) | **DROPPED** |

### Word / document rendering
| OLD | file:line | NEW | Status |
|---|---|---|---|
| `trial_report()` rmarkdown word_document output format (bundled reference.docx + title_page.lua) | `render.R:39` | none | **DROPPED** |
| RStudio "From Template" + `inst/rmarkdown/templates/` | (inst) | none (empty `inst/`) | **DROPPED** |
| `render_trial_report()` render a user `.Rmd`, snapshot-stamped, multi-dest | `render.R:94` | none | **DROPPED** |
| (none) | — | `bctu_report()` explicit-section spec + `report_heading/paragraph/figure/table` | `report.R:89,25,39,58` | new-in-rebuild |
| (none) | — | `render_report()` direct-pandoc docx+pdf + provenance manifest | `report.R:150` | new-in-rebuild |

### Stata / SAS export
| OLD | file:line | NEW | Status |
|---|---|---|---|
| `format_for_dta()` Stata name(32)/label(80) sanitisation + `name_map`/`dta_export_map` audit | `dta.R:43` | none | **DROPPED** |
| `.write_sas_program()` imports `.dta` (`dbms=dta`) + PROC FORMAT from REDCap dictionary | `sas.R:5` | `sas_import_script()` imports CSV (`dbms=csv`) + special-missing recode only | `missing.R:177` | redesigned (no value-label formats) |
| dta format on save | `snapshot-save.R` | `dta`, plus `sas7bdat`, `xpt` native (haven best-effort) | `snapshot.R:316–327` | present + extended |
| (none) | — | `special_missing()` / `apply_special_missing()` REDCap codes→tagged NA→SAS/Stata specials | `missing.R:28,78` | new-in-rebuild |

### Utilities
| OLD | file:line | NEW | Status |
|---|---|---|---|
| `iso8601(x, filename=)` colon/colon-free toggle | `utils.R:124` | `iso8601(time, tz=)` colon-only; colon-free is `snapshot_id()` | `utils.R:52,27` | redesigned |
| `dir.copy()` | `utils.R:11` | none (uses `file.copy` inline) | **DROPPED (export)** |
| `path_rel()` / `regex_escape_path()` | `utils.R:101,110` | `relative_to()`/`regex_escape()` internal | `snapshot.R:374,377` | renamed (unexported) |
| `apply_site_labels()` `"(CODE) Name"` builder | `trial_helpers.R:122` | none | **DROPPED** |

---

## 2. DROPPED CAPABILITIES (most important section)

A user of OLD could do each of these; a user of NEW cannot. Each tagged
**deliberate** (evidence of a redesign choice) or **omission** (no sign it was
replaced or intended).

1. **Store or delete a keyring credential from R.** OLD `set_token()`
   (`token.R:64`) stores a token in the keyring (interactively), `kill_token()`
   (`token.R:132`) deletes one, and `get_token()` step 3–4 (`token.R:117–127`)
   prompts and stores on first interactive use. NEW only *reads*
   (`resolve_credentials`, `datasource.R:78`); there is no writer, deleter, or
   prompt. The secret must be provisioned outside the package (keyring CLI / env
   var). **Deliberate** for the resolver split (the module header at
   `datasource.R:44` names one spec + one resolver), but the *store/delete/prompt*
   affordances are an **omission** — nothing in NEW replaces them.

2. **The full session `checkpoint` object and `get_checkpoint()`.** OLD
   `checkpoint()` (`checkpoint.R:266`) captured trial/analysis/notes, a
   persistent session id, script-path detection, git branch/commit, full
   `sessionInfo()` and package versions, with a printed audit summary
   (`print.checkpoint`, `checkpoint.R:370`) and retrieval from a snapshot
   (`get_checkpoint`, `checkpoint.R:350`). NEW `checkpoint()` (`snapshot.R:165`)
   returns only `created_utc, r_version, bctu_version, user, host`. There is no
   `get_checkpoint`, no `print` method, no analysis/notes/session-id/script/
   sessionInfo capture. **Deliberate** in part (provenance now lives in the
   manifest + ledger), but the loss of `trial`/`analysis`/`notes` tagging,
   `sessionInfo` capture and `get_checkpoint` retrieval is an **omission** with
   no NEW equivalent.

3. **Reusable DVP check primitives.** OLD `check_by_regex`, `check_ranges`,
   `check_integer`, `stock_numeric_regex` (`check_helpers.R:40–172`) produced a
   standard `(record_id, field, value, reason)` finding row and a library of
   numeric-format regexes. NEW has no primitives; a DVP is any
   `function(data)` returning a named list (`checks.R:37`). Users must hand-write
   range/regex/integer checks. **Deliberate** (NEW's free-form finding model is
   documented at `checks.R:1–16`) but the *primitive library* itself is an
   **omission**.

4. **Baseline-characteristics tables.** `summarise_baseline()` and the
   `bl_msd/med_iqr/gest_med_iqr/bl_levels` formatters (`report_baseline.R`) —
   grouped baseline tables with mean(SD)/median[IQR]/n(%)/gestational-age forms,
   "under review" special-missing handling — have **no** NEW equivalent.
   **Omission** (nothing in `table.R`/`report.R` builds a baseline summary).

5. **All UoB branding.** `uob_palettes`, `uob_tint`, `pal_uob`,
   `scale_*_uob(_c)`, `show_uob_palettes` (`uob_scales.R`), `theme_bctu_report`
   (`report_theme.R`), `fig_portrait`/`fig_landscape` and their knitr
   `opts_template` registration (`fig.R`, `zzz.R`). NEW has no `zzz.R`, no
   ggplot2 helpers, no figure-dimension helpers. **Omission** — no NEW code
   references UoB colours, the report theme, or figure sizing.

6. **The Word `trial_report()` output format and `.Rmd` rendering.** OLD
   shipped an `rmarkdown::word_document`-based output format with a bundled
   `reference.docx` and `title_page.lua` filter (`render.R:39`), an RStudio
   "From Template" tree, and `render_trial_report()` to render an arbitrary
   user `.Rmd` to snapshot-stamped filenames across multiple destinations
   (`render.R:94`). NEW's `render_report()` (`report.R:150`) renders an
   in-package `bctu_report` section-object via direct pandoc; it **cannot render
   a user-authored `.Rmd`**, has no reference.docx/Lua title-page system, and
   `inst/` is empty. **Deliberate** paradigm change (explicit sections, see
   `report.R:1–15`), but the ability to *render an existing trial `.Rmd`* and the
   BCTU-styled Word template are **dropped** with no replacement.

7. **Stata name/label sanitisation with an audit map.** `format_for_dta()`
   (`dta.R:43`) enforced Stata's 32-byte name / 80-char label limits, resolved
   duplicate names, optionally coerced labelled→factor, and emitted `name_map`
   and `dta_export_map` audit tables. NEW writes `.dta` via `haven::write_dta`
   directly (`snapshot.R:316`, `haven_write` at `snapshot.R:344`) with **no**
   name/label sanitisation and no audit map. **Omission** (a long variable name
   or over-length label is now haven's problem, silently).

8. **REDCap-dictionary-driven SAS value-label formats.** OLD `.write_sas_program`
   (`sas.R:5`) built `PROC FORMAT` blocks from the REDCap dictionary
   (yesno/checkbox/radio/dropdown) and applied them to the imported dataset. NEW
   `sas_import_script` (`missing.R:177`) only PROC IMPORTs the CSV and recodes
   *special-missing* values; it does **not** emit value-label formats.
   **Omission** — coded fields arrive in SAS without their labels.

9. **`dictionary()` / `set_dictionary()`** to attach/read REDCap metadata on an
   arbitrary object (`redcap.R:71,80`), and the `clean_value_labels` HTML/brace
   stripping (`redcap.R:131`). NEW attaches the dictionary automatically at fetch
   (`sources.R:102`) but exposes no manual attach/read and no label-cleaning
   toggle. **Deliberate** (auto-attach) for the read path; the *label cleaning*
   is an **omission**.

10. **`resolve_snapshot()`** reuse-or-refetch-by-age policy
    (`trial_helpers.R:23`) used by scheduled/weekly runs — **DROPPED**, no NEW
    equivalent. **Omission** (relevant given the weekly-report infrastructure).

11. **`apply_site_labels()`** `"(CODE) Name"` display-label builder
    (`trial_helpers.R:122`) — **DROPPED**. NEW's `resolve_finding_sites`
    (`checks.R:204`) is a different thing (row→site lookup, not label
    construction). **Omission**.

12. **`verify_snapshots()`** whole-store verification sweep — **DROPPED**
    (NEW verifies one snapshot at a time; `verify_ledger` checks the chain, not
    each payload). **Omission**.

13. **Institutional presets** `datasource_redcap_bctu/_itm/_bistc`,
    `datasource_sql_bctu`, `default_redcap_url` (`datasource.R:63,130,150,170,234`)
    — **DROPPED**. NEW requires the URL/server to be passed explicitly.
    **Deliberate** (explicit-configuration principle in the NEW CLAUDE.md), but
    it removes the one-argument convenience constructors trial code used.

14. **Report-table row-merging, multi-line cells, autoformat and full-width
    stretch** (`report_table.R:158,116,270,295`) — **DROPPED** from NEW's
    `render_table_markdown` (newlines collapse to spaces at `table.R:323`). Mostly
    **deliberate** simplification, but row-merging and multi-line cells have no
    NEW substitute.

15. **`save_snapshot.default`** (saveRDS of an arbitrary object) and the
    **checkpoint / POSIXt `load_snapshot` selectors** — **DROPPED**
    (NEW `save_snapshot` requires a `bctu_snapshot`, `snapshot.R:226`; NEW
    `which` accepts only latest/penultimate/id/integer, `snapshot.R:405`).
    **Deliberate** narrowing.

16. **Exported utilities `dir.copy()`, `col_widths()`** — **DROPPED** as exports
    (`utils.R:11`, `report_table.R:249`). **Deliberate** (internalised).

---

## 3. BEHAVIOURAL DIFFERENCES (capabilities in both)

### Config / location resolution
- OLD `snapshots_dir()` (`snapshot-dirs.R:101–127`) resolves in order:
  `getOption("snapshots_dir")` → nearest `.bctu.env` walking up from `getwd()`
  → **silent working-directory fallback** `file.path(getwd(), "Data",
  "Snapshots")` (`snapshot-dirs.R:121`). It never errors; a missing config just
  falls back to the cwd and prints an info alert. `bctu_doctor` and every
  snapshot call take `path = snapshots_dir()` as a default argument.
- NEW `snapshot_store()` (`config.R:114–128`) resolves **only** relative to the
  `bctu-project.yml` marker found by walking up (`find_project_marker`,
  `config.R:48`). With no marker it **errors loudly** (`bctu_project`,
  `config.R:67–72`: *"bctu refuses to guess a location: nothing was read or
  written."*). There is no option, no dotenv, and no cwd fallback (stated
  explicitly in the file header, `config.R:4–8`). The resolved store is
  announced (`config.R:125–126`).
- Net: OLD is permissive and can silently write under `getwd()`; NEW is strict
  and refuses to act without a committed marker. Snapshot store location is
  declared in the marker's `snapshot_store` field (default `"Data/Snapshots"`,
  `config.R:80`).

### Snapshot on-disk layout / filenames
- OLD: `<snapshots_dir>/<YYYY-MM-DDTHHMMSSZ>/` with subdirs `rds/ csv/ dta/
  sas/` holding `<name>_<ts>.<ext>` and a `metadata.yml` (per-file rel path,
  size, SHA-256). Single-table.
- NEW (`snapshot.R:4–17`): `<store>/<id>/manifest.yml` +
  `tables/<table>/<study>_<table>_<id>.<ext>`. **Multi-table** (one subdir per
  table). Filenames are **self-identifying** (`<study>_<table>_<id>`,
  `snapshot.R:248`). Id is UTC `YYYY-MM-DDTHHMMSSZ` with a **collision suffix**
  `-NN` if two land in the same second (`snapshot.R:236–242`). A store-root
  **append-only, hash-chained audit ledger** `SNAPSHOTS.log.yml` records every
  take and delete (`snapshot.R:37,263`). Formats: `rds,csv` default, plus
  `dta,sas7bdat,xpt,sas` (`snapshot.R:305–338`).
- Read-only protection: OLD applied `Sys.chmod` read-only to payloads after
  commit; NEW **deliberately never** makes payloads read-only ("ordinary git and
  delete operations never fight the filesystem", `snapshot.R:16–17`).

### Snapshot git provenance
- OLD (`snapshot-save.R:324–396`): `commit = TRUE` **by default**; path-scoped
  `git commit --only -- <files>` with a `Bctu-Snapshot-Id: <ts>` message
  trailer; commits `metadata.yml` + the SAS import script; fallback identity if
  `user.name/email` unset.
- NEW (`snapshot.R:269–285`): three modes `record`/`commit`/`off`, default
  `commit` **only when a `tag` is given**, else `record` (`snapshot.R:271`).
  `record` writes `git_head`+`git_dirty` into the manifest; `commit` additionally
  commits **manifest + ledger only (never payload)** and creates an **annotated
  tag** `snap/<tag-or-id>` (`snapshot.R:146–147`). Git failure never fails a
  snapshot (`snapshot.R:210–213`).

### Snapshot verification
- OLD `verify_snapshot` (`snapshot-verify.R`): levels `full`/`quick`,
  corrupt-metadata verdicts, deletion-awareness, and a **git-trailer comparison**
  of the committed `metadata.yml` against on-disk (tamper detection vs the git
  record). `verify_snapshots` sweeps the store.
- NEW `verify_snapshot` (`snapshot.R:464–480`): recomputes SHA-256 of each
  payload file and compares to the manifest — **on-disk integrity only**, no git
  comparison, no full/quick level, no deletion verdict. Tamper-evidence of the
  *sequence* is separate: `verify_ledger` (`snapshot.R:83`) checks the ledger
  hash chain.

### Delete
- Both are record-first retire/destroy. OLD records via a **git commit**
  (`Bctu-Snapshot-Deleted` trailer, commit must succeed before removal,
  `snapshot-delete.R:119–171`). NEW records via the **append-only ledger** before
  touching the directory (`snapshot.R:509–514`), then moves to `_deleted/<id>/`
  with a `deletion-note.yml` (retire) or `unlink`s (destroy). NEW requires a
  non-empty `reason` (`snapshot.R:506`); OLD's destroy of a committed snapshot
  required `force = TRUE`.

### DVR / CDI checks
- Diff mechanism: OLD `save_checks` diffs against a `since` **selector**
  (default `"penultimate"`, resolved via `load_snapshot`). NEW takes **explicit
  `before`/`after` snapshot objects** (`checks.R:391`) and compares findings
  **whole-row** (`compare_dvp`, `checks.R:134`; status `new`/`unchanged`/
  `resolved`). NEW `save_cdi` does **not** accept `before` (`checks.R:412`),
  whereas OLD `save_cdi` inherited `since`.
- Output tree: OLD `<path>/<timestamp>[/v<version>]/full|update/` with per-site
  subfolders and a `_summary.txt`. NEW `<path>/<KIND>-<id>[_n]/full|update/`
  with `sites/<site>/` subfolders and a `manifest.yml` (`checks.R:498–517`).
- Excel: OLD always wrote xlsx (openxlsx). NEW **always** writes readable
  CSV + TXT (`write_findings_readable`, `checks.R:233`) and treats the xlsx
  workbook as optional (`write_xlsx`, warns if openxlsx absent, `checks.R:259`).
- Site assignment: OLD split on a `site_id` column carried in each check's
  output. NEW resolves each finding's site by looking up `id_col`→`site_col` in
  the snapshot(s) (`resolve_finding_sites`, `checks.R:204`), unioning `before`
  and `after`, labelling unresolved rows `NO_SITE` with a warning.

### SAS / Stata writing
- OLD wrote `.dta` via `format_for_dta()` then `haven::write_dta`, and a SAS
  program that imports the `.dta` (`dbms=dta`) and applies REDCap-derived
  `PROC FORMAT` value labels (`sas.R`). NEW writes `.dta`/`.sas7bdat`/`.xpt`
  via haven directly (best-effort, never aborting a save, `snapshot.R:344–358`)
  and a SAS script that imports the **CSV** (`dbms=csv`) and recodes only
  special-missing values (`missing.R:177–202`). NEW adds native `sas7bdat`/`xpt`
  (with uppercase tag re-tagging, `snapshot.R:322,326`); OLD did neither.

### `iso8601`
- OLD `iso8601(x, filename = FALSE)` (`utils.R:124`): one function; `filename =
  TRUE` yields the colon-free form. NEW splits this: `iso8601(time, tz = "UTC")`
  is always colon form (`utils.R:52`); the colon-free filesystem id is a separate
  canonical function `snapshot_id()` (`utils.R:27`) with its own strict parser
  `parse_snapshot_id()` (`utils.R:38`).

### Dependencies
- OLD `Imports: cli, DBI, digest, haven, keyring, readr, stats, tools, utils,
  yaml`. NEW `Imports: cli, digest, yaml` only; DBI/odbc/RSQLite/httr2/keyring/
  haven/readr all moved to **Suggests** and are `requireNamespace`-guarded at
  point of use (e.g. `haven_write` at `snapshot.R:344`, `redcap_perform` at
  `sources.R:129`, `resolve_credentials` at `datasource.R:81`).

---

## 4. THE FOLDER / LOCATION MODEL (verbatim)

**OLD — permissive, three-tier, silent cwd fallback.** `snapshots_dir()`
(`snapshot-dirs.R:101–127`):

```r
snapshots_dir <- function() {
  p <- getOption("snapshots_dir", NA_character_)
  if (!is.na(p) && nzchar(p)) {
    return(normalizePath(p, winslash = "/", mustWork = FALSE))
  }
  env_file <- .bctu_find_env_file()
  p <- .bctu_read_env_value(env_file)
  if (!is.na(p) && nzchar(p)) { ...notify...; return(p) }
  p <- normalizePath(file.path(getwd(), "Data", "Snapshots"), winslash = "/", mustWork = FALSE)
  .snapshots_dir_notify(p, "No configured snapshots directory found; using working-directory fallback {.path {p}}")
  p
}
```

So OLD **does** impose a folder convention *as a fallback only*: with nothing
configured it writes under `getwd()/Data/Snapshots` and merely prints an info
alert. It never refuses. `set_snapshots_dir()` (`snapshot-dirs.R:132`) makes a
choice sticky by writing `SNAPSHOTS_DIR=` into a `.bctu.env` and setting the
`snapshots_dir` option.

**NEW — strict, marker-anchored, errors on ambiguity.** `bctu_project()`
(`config.R:65–73`) and `snapshot_store()` (`config.R:114–128`):

```r
bctu_project <- function(start = getwd()) {
  file <- find_project_marker(start)
  if (is.na(file))
    cli::cli_abort(c(
      "No {.file {project_marker_name}} found at or above {.file {normalizePath(start)}}.",
      "i" = "Run {.code bctu_init_project(<name>)} at the trial root first.",
      "x" = "bctu refuses to guess a location: nothing was read or written."
    ))
  ...
}

snapshot_store <- function(start = getwd(), verbose = 1L, create = FALSE) {
  p <- bctu_project(start)
  store <- p$snapshot_store
  store <- if (is_absolute_path(store)) store else file.path(p$root, store)
  store <- normalize_path_lenient(store)
  ...
  if (verbose >= 1L)
    cli::cli_alert_info("snapshot store: {.file {store}}  (from {.file {p$file}})")
  store
}
```

The store is resolved **relative to the marker's directory**, defaulting to
`Data/Snapshots` under the trial root (`config.R:80`), or an absolute path if
the marker gives one (`config.R:117`). The file header states the contract
directly (`config.R:4–8`):

```
# There is NO global option, NO walk-from-cwd dotenv, and NO silent
# "Data/Snapshots under getwd()" fallback. If the marker cannot be found,
# resolution errors loudly. Every resolved path is announced, so a snapshot
# can never be written somewhere unnoticed.
```

Net difference: **OLD forces a folder convention only as a last-resort default
and will silently use it; NEW forces an explicit, committed marker and refuses
to read or write without one.** This also flows into `check_setup`, which
reports the store check as an informational **SKIP** (not a failure) when no
marker is found (`validation.R:188–193`), and into every snapshot/report call,
whose `store`/`paths` default to the marker-resolved location.
