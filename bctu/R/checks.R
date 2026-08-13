# ---------------------------------------------------------------------------
# Checks: DVP (Data Validation) and CDI (Critical Data Items)
# ---------------------------------------------------------------------------
# A DVP is a single function of a snapshot's data that returns a NAMED list of
# findings. Each named element is one check; the name becomes the worksheet
# name. Findings are free-form data frames: a check returns whatever columns it
# needs (one field, several fields, or cross-record combinations). Nothing about
# the shape of a finding is fixed, so a check can report whatever combination of
# variables and records raised it.
#
# To see what has changed, the SAME DVP function is run on two datasets (a
# `before` and an `after` snapshot) and the findings are compared whole-row per
# check: a finding is `new` if its entire row appears in `after` but not
# `before`, `resolved` if it appears in `before` but not `after`, `unchanged` if
# in both. There is no baseline ledger and no fixed finding key; the two
# datasets you pass ARE the comparison.

# --- running a DVP ----------------------------------------------------------
#' Run a DVP function on one dataset and validate its output
#'
#' A DVP is `function(data)` returning a named list, one element per check. Each
#' element is a data frame of findings (or `NULL` / an empty data frame when the
#' check finds nothing). The function is rejected with a clear error if it does
#' not return a named list, so a malformed DVP fails immediately rather than
#' producing an unusable report.
#' @param dvp A function of the data returning a named list of findings.
#' @param data The data the DVP reads: usually a snapshot (a named list of data
#'   frames), but any object the DVP function accepts.
#' @return A named list of findings data frames (empty frames kept in place).
#' @examples
#' data <- data.frame(id = 1:3, weight_kg = c(65, 999, 70))
#' dvp <- function(data) {
#'   list(weight_range = data[data$weight_kg > 300, ])
#' }
#' run_dvp(dvp, data)
#' @export
run_dvp <- function(dvp, data) {
  if (!is.function(dvp))
    cli::cli_abort(c(
      "A DVP must be a function of the data.",
      "i" = "Write it as {.code function(data) list(check_one = ..., check_two = ...)}."))

  findings <- dvp(data)

  ok_named_list <- is.list(findings) && !is.data.frame(findings) &&
    length(findings) > 0L && !is.null(names(findings)) &&
    all(nzchar(names(findings))) && !anyDuplicated(names(findings))
  if (!ok_named_list)
    cli::cli_abort(c(
      "A DVP function must return a named list of findings, one element per check.",
      "x" = "Each element needs a non-empty, unique name (used as the worksheet name).",
      "i" = "For example: {.code function(data) list(weight_range = ..., visit_order = ...)}."))

  for (nm in names(findings)) {
    el <- findings[[nm]]
    if (is.null(el)) {
      findings[[nm]] <- data.frame()
    } else if (!is.data.frame(el)) {
      cli::cli_abort(c(
        "Check {.val {nm}} must return a data frame of findings.",
        "i" = "Return an empty data frame (or {.code NULL}) when the check finds nothing."))
    }
  }
  findings
}

# --- before / after comparison ---------------------------------------------
#' Whole-row identity keys for a findings data frame
#'
#' Joins every column of each row into one string, so two findings are the same
#' only when their entire rows match. Findings are free-form, so identity is the
#' whole row and nothing is assumed about column names. A genuine `NA` is
#' encoded with a reserved control character, distinct from the literal string
#' `"NA"`, so the two can never collide in the key.
#' @param df A findings data frame.
#' @return A character vector of one key per row (empty for a zero-row frame).
#' @export
finding_row_keys <- function(df) {
  if (nrow(df) == 0L) return(character(0))
  cols <- lapply(df, function(col) {
    ch <- as.character(col)
    ch[is.na(col)] <- "\x02"
    ch
  })
  do.call(paste, c(cols, sep = "\x1f"))
}

#' Row-bind two findings frames, tolerating differing columns
#'
#' The `before` and `after` findings for one check share the same columns, so
#' this is a plain `rbind` in normal use; the column union only matters for the
#' edge case where a check appears in one run but not the other.
#' @param x,y Findings data frames.
#' @return One data frame with the union of columns.
#' @export
bind_findings <- function(x, y) {
  if (nrow(x) == 0L && nrow(y) == 0L) {
    cols <- union(names(x), names(y))
    return(as.data.frame(
      stats::setNames(replicate(length(cols), character(0), simplify = FALSE), cols),
      stringsAsFactors = FALSE))
  }
  if (nrow(x) == 0L) return(y)
  if (nrow(y) == 0L) return(x)
  cols <- union(names(x), names(y))
  for (col in setdiff(cols, names(x))) x[[col]] <- NA
  for (col in setdiff(cols, names(y))) y[[col]] <- NA
  out <- rbind(x[cols], y[cols])
  rownames(out) <- NULL
  out
}

#' Compare a DVP's findings between two datasets (before vs after)
#'
#' Runs `dvp` on `before` and on `after` and, for every check, labels each
#' finding whole-row: `new` (row present in `after` but not `before`),
#' `resolved` (row present in `before` but not `after`) or `unchanged` (row in
#' both). Rows are keyed positionally over the columns each frame returns, so a
#' check must return the same columns, in the same order, on both runs; a check
#' that returns different or reordered columns between `before` and `after` is
#' not reconciled and every row will read as changed.
#' @param dvp A DVP function (see [run_dvp()]).
#' @param before,after The two datasets (snapshots) to compare.
#' @return A named list; each element is that check's findings with an added
#'   `status` column (`new` / `unchanged` / `resolved`).
#' @examples
#' before <- data.frame(id = 1:3, weight_kg = c(65, 999, 70))
#' after  <- data.frame(id = 1:3, weight_kg = c(65, 72, 70))
#' dvp <- function(data) {
#'   list(weight_range = data[data$weight_kg > 300, ])
#' }
#' compare_dvp(dvp, before, after)
#' @export
compare_dvp <- function(dvp, before, after) {
  b <- run_dvp(dvp, before)
  a <- run_dvp(dvp, after)

  has_status <- function(d) is.data.frame(d) && "status" %in% names(d)
  offenders <- unique(c(names(a)[vapply(a, has_status, logical(1))],
                        names(b)[vapply(b, has_status, logical(1))]))
  if (length(offenders))
    cli::cli_abort(c(
      "A check returned a reserved column named {.field status}: {.val {offenders}}.",
      "i" = "{.fn compare_dvp} adds its own {.field status} column (new / unchanged / resolved).",
      "x" = "Rename that column in your DVP (for example {.field query_status})."))

  checks <- union(names(a), names(b))
  out <- stats::setNames(vector("list", length(checks)), checks)
  for (nm in checks) {
    bf <- if (is.null(b[[nm]])) data.frame() else b[[nm]]
    af <- if (is.null(a[[nm]])) data.frame() else a[[nm]]
    bk <- finding_row_keys(bf)
    ak <- finding_row_keys(af)

    current <- af
    current$status <- if (nrow(af)) ifelse(ak %in% bk, "unchanged", "new") else character(0)

    resolved <- bf[!(bk %in% ak), , drop = FALSE]
    resolved$status <- if (nrow(resolved)) "resolved" else character(0)

    out[[nm]] <- bind_findings(current, resolved)
  }
  attr(out, "check_info") <- attr(a, "check_info") %||% attr(b, "check_info")
  out
}

# --- per-check query text (check info) --------------------------------------
#' Validate a check-info table (per-check query text and labels)
#'
#' A check-info table describes the checks in DM-facing language: one row per
#' check with the query text a data manager acts on, plus optional grouping and
#' criticality labels. Required columns: `check` (unique, matching the names the
#' DVP function returns) and `query` (non-empty text). Optional columns:
#' `section` (free text) and `critical` (logical), settable independently.
#' Checks with findings but no info row, and info rows matching no check, are
#' warned about (not errors), so a partially annotated DVP still reports.
#' @param info A data frame as described above.
#' @param check_names The check names the DVP produced this run.
#' @return `info`, with columns ordered `check`, `section`, `critical`, `query`
#'   (those present), row order preserved.
#' @keywords internal
validate_check_info <- function(info, check_names) {
  if (!is.data.frame(info) || !all(c("check", "query") %in% names(info)))
    cli::cli_abort(c(
      "{.arg check_info} must be a data frame with columns {.field check} and {.field query}.",
      "i" = "Optional columns: {.field section} (text) and {.field critical} (logical)."))
  chk <- as.character(info$check); qry <- as.character(info$query)
  if (anyNA(chk) || !all(nzchar(trimws(chk))))
    cli::cli_abort("Every {.field check} in {.arg check_info} must be a non-empty name.")
  if (anyDuplicated(chk))
    cli::cli_abort("Duplicate {.field check} name{?s} in {.arg check_info}: {.val {unique(chk[duplicated(chk)])}}.")
  if (anyNA(qry) || !all(nzchar(trimws(qry))))
    cli::cli_abort("Every {.field query} in {.arg check_info} must be non-empty text.")
  if ("critical" %in% names(info) && !is.logical(info$critical))
    cli::cli_abort("{.field critical} in {.arg check_info} must be logical (TRUE/FALSE).")

  missing_info <- setdiff(check_names, chk)
  if (length(missing_info))
    cli::cli_warn(c("Check{?s} with no {.arg check_info} row: {.val {missing_info}}.",
                    "i" = "Their findings are written without query text."))
  unknown <- setdiff(chk, check_names)
  if (length(unknown))
    cli::cli_warn(c("{.arg check_info} row{?s} matching no check this run: {.val {unknown}}.",
                    "i" = "Kept in the index (an all-clear check still belongs in the catalogue)."))

  ord <- intersect(c("check", "section", "critical", "query"), names(info))
  info[c(ord, setdiff(names(info), ord))]
}

#' Prepend the query text as the first column of each finding frame
#'
#' The query travels with the row, so a data manager filtering or copying
#' findings (especially from a per-site workbook) keeps the DM-facing text next
#' to the record. Added AFTER any before/after comparison, so editing a query's
#' wording never makes findings read as new/resolved.
#' @param sheets Named list of findings frames.
#' @param info A validated check-info table.
#' @return `sheets` with a `query` first column on every non-empty frame whose
#'   check has an info row.
#' @keywords internal
add_query_column <- function(sheets, info) {
  clash <- names(sheets)[vapply(sheets, function(d) "query" %in% names(d), logical(1))]
  if (length(clash))
    cli::cli_abort(c(
      "Check{?s} {.val {clash}} returned a reserved column named {.field query}.",
      "i" = "With {.arg check_info}, the engine adds its own {.field query} column.",
      "x" = "Rename that column in your DVP (for example {.field query_status})."))
  for (nm in names(sheets)) {
    d <- sheets[[nm]]
    row <- match(nm, info$check)
    if (nrow(d) == 0L || is.na(row)) next
    sheets[[nm]] <- cbind(query = as.character(info$query[row]), d)
  }
  sheets
}

# --- snapshot fingerprint ---------------------------------------------------
#' A single integrity fingerprint for a saved snapshot
#'
#' Derived from the per-table SHA-256 values in the snapshot's own manifest, so
#' it does not re-hash the payload and matches what the snapshot recorded.
#' @param snapshot A saved `bctu_snapshot` (must carry its on-disk directory).
#' @return A hex string, or `NA` if the snapshot is not on disk.
#' @export
snapshot_fingerprint <- function(snapshot) {
  dir <- attr(snapshot, "dir")
  if (is.null(dir) || !dir.exists(dir)) return(NA_character_)
  man <- yaml::read_yaml(file.path(dir, manifest_filename))
  shas <- unlist(lapply(man$tables, function(t)
    vapply(t$files, function(f) f$sha256 %||% "", character(1))))
  digest::digest(sort(unname(shas)), algo = "sha256")
}

# --- trial name and per-site resolution ------------------------------------
#' The study/trial name carried on a snapshot
#'
#' Read from the snapshot's own metadata (which the datasource sets), so a
#' report never needs the trial name passed in by hand. Falls back to
#' `"snapshot"` when a snapshot carries no name.
#' @param snapshot A `bctu_snapshot`.
#' @return A filesystem-safe study/trial token.
#' @export
report_trial_name <- function(snapshot) {
  meta <- attr(snapshot, "bctu_meta")
  sanitise_study_name(if (is.list(meta)) meta$name else NULL)
}

#' Site label for each finding row, from the findings themselves or a snapshot
#'
#' A finding that carries `site_col` as one of its own columns is sited from
#' that column directly, row by row (a trial whose dataset has no single id
#' column can still split per site by selecting the site into each finding).
#' Rows without their own site are resolved from the snapshot(s): any table
#' holding both `id_col` and `site_col` maps a record to a site. Pass both the
#' `before` and `after` snapshots when comparing, so a `resolved` finding whose
#' record was removed from `after` is still sited from `before`. Findings with
#' no site by either route are labelled `NO_SITE` so they are never silently
#' dropped from the split.
#' @param findings A findings data frame.
#' @param snapshots A list of snapshots to union (earlier snapshots take
#'   priority when a record's site conflicts across snapshots).
#' @param id_col Name of the record-id column shared by findings and data.
#' @param site_col Name of the site column in the findings and/or the data.
#' @return A character vector of sites, one per finding row.
#' @export
resolve_finding_sites <- function(findings, snapshots, id_col, site_col) {
  own <- if (site_col %in% names(findings)) as.character(findings[[site_col]])
         else rep(NA_character_, nrow(findings))
  map <- character(0)
  for (snap in snapshots) {
    for (nm in names(snap)) {
      tab <- snap[[nm]]
      if (is.data.frame(tab) && all(c(id_col, site_col) %in% names(tab))) {
        add <- stats::setNames(as.character(tab[[site_col]]), as.character(tab[[id_col]]))
        map <- c(map, add[!names(add) %in% names(map)])
      }
    }
  }
  ids <- if (id_col %in% names(findings)) as.character(findings[[id_col]])
         else rep(NA_character_, nrow(findings))
  looked_up <- unname(map[ids])
  site <- ifelse(!is.na(own) & nzchar(own), own, looked_up)
  site[is.na(site) | !nzchar(site)] <- "NO_SITE"
  site
}

# --- writers ----------------------------------------------------------------
#' Write each check's findings as a readable CSV and plain-text copy
#'
#' One pair of files per check, named by the check. Gives a fully testable,
#' Excel-free record of every finding. Names that sanitise to the same file
#' stem (e.g. `"a/b"` and `"a b"` both sanitise to `"a_b"`) are de-duplicated
#' with a trailing underscore, the same way the workbook sheet names are.
#' @param sheets A named list of findings data frames.
#' @param dir Directory to write into (created if missing).
#' @return Invisibly, the directory.
#' @export
write_findings_readable <- function(sheets, dir) {
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  used <- character(0)
  for (nm in names(sheets)) {
    safe <- gsub("[^A-Za-z0-9_-]+", "_", nm)
    while (safe %in% used) safe <- paste0(safe, "_")
    used <- c(used, safe)
    df <- as.data.frame(sheets[[nm]])
    utils::write.csv(df, file.path(dir, paste0(safe, ".csv")), row.names = FALSE, na = "")
    txt <- if (nrow(df)) utils::capture.output(print(df, row.names = FALSE)) else "(no findings)"
    writeLines(txt, file.path(dir, paste0(safe, ".txt")))
  }
  invisible(dir)
}

#' Write a set of checks to one Excel workbook, one worksheet per check
#'
#' A convenience only: the CSV/TXT copies carry the same content, so the DVP
#' logic is fully testable without Excel. Uses `openxlsx` if installed; if not,
#' warns and does nothing. Worksheet names are the check names, trimmed to
#' Excel's 31-character limit (de-duplicated if trimming collides).
#' @param sheets A named list of findings data frames.
#' @param path Workbook path (`.xlsx`).
#' @param index Optional check-info table written as a leading `checks_index`
#'   worksheet (query text catalogue; no counts, so it is identical between a
#'   full and an update set).
#' @return Invisibly, the path (or `NULL` if `openxlsx` is unavailable or there
#'   is nothing to write).
#' @export
write_findings_workbook <- function(sheets, path, index = NULL) {
  if (!length(sheets)) return(invisible(NULL))
  if (!requireNamespace("openxlsx", quietly = TRUE))
    cli::cli_abort(c(
      "The {.pkg openxlsx} package is required to write the findings workbook.",
      "i" = "Install it with {.code install.packages(\"openxlsx\")}, then rerun."))
  wb <- openxlsx::createWorkbook()
  used <- character(0)
  if (!is.null(index)) {
    openxlsx::addWorksheet(wb, "checks_index")
    openxlsx::writeData(wb, "checks_index", as.data.frame(index))
    used <- "checks_index"
  }
  for (nm in names(sheets)) {
    sheet <- unique_sheet_name(nm, used)
    used <- c(used, sheet)
    openxlsx::addWorksheet(wb, sheet)
    openxlsx::writeData(wb, sheet, as.data.frame(sheets[[nm]]))
  }
  openxlsx::saveWorkbook(wb, path, overwrite = FALSE)
  invisible(path)
}

#' Write the checks index (per-check query text) as CSV and TXT
#'
#' One catalogue of the DVP's checks in DM-facing language, written alongside
#' the findings in every report set and per-site directory, so query text is
#' never stranded away from the findings it explains. Lists every check in the
#' info table, including all-clear checks with no findings this run.
#' @param info A validated check-info table (see [validate_check_info()]).
#' @param dir Directory to write into.
#' @return Invisibly, the directory.
#' @keywords internal
write_checks_index <- function(info, dir) {
  df <- as.data.frame(info)
  utils::write.csv(df, file.path(dir, "checks_index.csv"), row.names = FALSE, na = "")
  writeLines(utils::capture.output(print(df, row.names = FALSE)),
             file.path(dir, "checks_index.txt"))
  invisible(dir)
}

#' Make an Excel-safe, unique worksheet name (<= 31 chars)
#'
#' Excel worksheet names are capped at 31 characters and cannot repeat. This
#' sanitises the name and, on a clash, appends a numeric suffix that SHORTENS the
#' stem so the result always changes and always stays within 31 characters (the
#' naive "append and re-truncate" never terminates once the name is already 31).
#' @keywords internal
unique_sheet_name <- function(nm, used) {
  base <- substr(gsub("[^A-Za-z0-9_ -]", "_", nm), 1L, 31L)
  if (!nzchar(base)) base <- "sheet"
  cand <- base
  i <- 1L
  while (cand %in% used) {
    sfx  <- paste0("_", i)
    cand <- paste0(substr(base, 1L, 31L - nchar(sfx)), sfx)
    i <- i + 1L
  }
  cand
}

#' Drop checks with no findings
#'
#' Empty checks get no worksheet, matching the delivered DVR (an all-clear check
#' is not a blank tab). The names are preserved for the checks that remain.
#' @param sheets A named list of findings data frames.
#' @return The subset of `sheets` with at least one row.
#' @export
nonempty_checks <- function(sheets) {
  sheets[vapply(sheets, function(d) nrow(d) > 0L, logical(1))]
}

#' Write one report set: an overall workbook plus (optionally) per-site output
#'
#' The delivered record is one Excel workbook per set: an overall workbook of
#' all findings (one worksheet per non-empty check), plus a per-site workbook
#' under `sites/` for each centre when `site_col` is given, so a centre
#' receives only its own queries alongside the overall set. Per-check CSV/TXT
#' copies are written only when `write_readable` is `TRUE`. Sites are resolved from `snapshot`, and from `before_snapshot`
#' too when given, so a `resolved` finding whose record was removed from
#' `snapshot` is still sited correctly. A `cli_warn` is raised per check that
#' has findings with no resolvable site.
#' @param sheets A named list of findings data frames (empty checks omitted).
#' @param snapshot The snapshot the findings came from (for site lookup).
#' @param dir Directory to write this set into.
#' @param base_name File stem for the workbooks.
#' @param id_col,site_col Columns used to split by site; `site_col = NULL`
#'   writes the overall workbook only.
#' @param write_readable Also write per-check CSV/TXT copies of the findings?
#'   Default `FALSE`: the workbook is the delivered record.
#' @param before_snapshot Optional earlier snapshot, unioned with `snapshot`
#'   for site lookup (see [resolve_finding_sites()]).
#' @param check_info Optional validated check-info table; when given, the
#'   checks index (sheet, CSV and TXT) is written with this set and every
#'   per-site output, so query text always accompanies the findings.
#' @return Invisibly, the directory.
#' @export
write_report_set <- function(sheets, snapshot, dir, base_name,
                             id_col = "record_id", site_col = NULL,
                             write_readable = FALSE, before_snapshot = NULL,
                             check_info = NULL) {
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  filled <- nonempty_checks(sheets)

  if (isTRUE(write_readable)) write_findings_readable(sheets, dir)
  if (!is.null(check_info)) write_checks_index(check_info, dir)

  if (length(filled))
    write_findings_workbook(filled, file.path(dir, paste0(base_name, ".xlsx")),
                            index = check_info)

  if (!is.null(site_col) && length(filled)) {
    snapshots <- if (is.null(before_snapshot)) list(snapshot) else list(before_snapshot, snapshot)
    site_of <- lapply(filled, function(d)
      resolve_finding_sites(d, snapshots, id_col, site_col))

    for (nm in names(filled)) {
      no_site_n <- sum(site_of[[nm]] == "NO_SITE")
      if (no_site_n > 0L)
        cli::cli_warn(c(
          "Check {.val {nm}}: {no_site_n} finding{?s} could not be mapped to a site.",
          "i" = "Written under {.val NO_SITE} in the per-site split."))
    }

    all_sites <- sort(unique(unlist(site_of)))
    for (s in all_sites) {
      per_check <- list()
      for (nm in names(filled)) {
        rows <- filled[[nm]][site_of[[nm]] == s, , drop = FALSE]
        if (nrow(rows)) per_check[[nm]] <- rows
      }
      if (!length(per_check)) next
      safe_site <- gsub("[^A-Za-z0-9_-]+", "_", s)
      sdir <- file.path(dir, "sites", safe_site)
      dir.create(sdir, recursive = TRUE, showWarnings = FALSE)
      if (isTRUE(write_readable)) write_findings_readable(per_check, sdir)
      if (!is.null(check_info)) write_checks_index(check_info, sdir)
      write_findings_workbook(per_check,
        file.path(sdir, paste0(base_name, "_", safe_site, ".xlsx")),
        index = check_info)
    }
  }
  invisible(dir)
}

# --- the report engine ------------------------------------------------------
#' Build a Data Validation Report (DVR) from a DVP function and a snapshot
#'
#' Runs `dvp` on the `after` snapshot and writes, under every directory in
#' `paths`, the house layout `<path>/<after snapshot id>/v<version>/` (a folder
#' per data state, then a folder per controlled document version; without a
#' `version`, `<path>/<after snapshot id>/` directly): an overall workbook (one
#' worksheet per non-empty check) plus per-site workbooks when `site_col` is
#' given, the readable CSV/TXT copies, and an auditable YAML manifest. A rerun
#' of the same snapshot and version suffixes the leaf folder `_N`, never
#' silently overwriting. When a `before` snapshot is supplied, each finding
#' is labelled `new` / `unchanged` / `resolved` by a whole-row comparison of the
#' two runs (see [compare_dvp()]) and an additional `update/` set of the changed
#' rows (new or resolved) is written alongside the full set. The trial name is
#' taken from the snapshot itself (see [report_trial_name()]); nothing about the
#' trial has to be passed in.
#' @param dvp A DVP function: `function(data)` returning a named list of
#'   findings (see [run_dvp()]).
#' @param after The current snapshot the report is about.
#' @param before Optional earlier snapshot to compare against; when `NULL` the
#'   report lists the current findings with no change labelling and no `update/`.
#' @param paths One or more directories to receive the report (the report is
#'   written to each). Defaults to the working directory.
#' @param id_col Record-id column used to map findings to sites. Default
#'   `"record_id"`.
#' @param site_col Site column in the data; when `NULL` (default) no per-site
#'   split is written.
#' @param version Optional DVP version string, recorded and added to file names.
#' @param operator Person issuing the report (recorded); default the OS user.
#' @param check_info Optional data frame describing the checks in DM-facing
#'   language: columns `check` (matching the names the DVP returns) and `query`
#'   (the text a data manager acts on), plus optional `section` (free text) and
#'   `critical` (logical). When given, a `checks_index` sheet leads every
#'   workbook (overall and per-site), `checks_index.csv`/`.txt` are written
#'   alongside the findings in every output directory, and each check's query,
#'   section and criticality are recorded in the manifest. The index lists
#'   every check in the table, including all-clear checks with no findings.
#'   When `NULL`, the DVP function may supply the same table itself via
#'   `attr(findings, "check_info")` on the list it returns; an explicit
#'   `check_info` argument wins over the attribute.
#' @param query_column Prepend each check's query text as the first column of
#'   its finding rows (so the text travels with a row into a data manager's
#'   query log)? Default `TRUE` when check info is available. The column is
#'   added after any before/after comparison, so rewording a query never makes
#'   findings read as new or resolved. A check returning its own `query` column
#'   is an error while check info is in use.
#' @param write_readable Also write per-check CSV/TXT copies of the findings?
#'   Default `FALSE`: the delivered record is the workbook (one worksheet per
#'   check), and `openxlsx` is required up front. The checks index and the YAML
#'   manifest are always written.
#' @param verbose Verbosity.
#' @return Invisibly, a list with the report id, directories written, sheets,
#'   and per-check counts.
#' @examples
#' \dontrun{
#' dvp <- function(data) list(weight_range = data$records[data$records$weight_kg > 300, ])
#' save_dvr(dvp, after = load_snapshot("latest"))
#' }
#' @export
save_dvr <- function(dvp, after, before = NULL, paths = getwd(),
                     id_col = "record_id", site_col = NULL, version = NULL,
                     operator = NULL, check_info = NULL, query_column = TRUE,
                     write_readable = FALSE, verbose = 2L) {
  run_data_report(dvp, after, before, paths, kind = "dvr", id_col = id_col,
                  site_col = site_col, version = version, operator = operator,
                  check_info = check_info, query_column = query_column,
                  write_readable = write_readable, verbose = verbose)
}

#' Build a Critical Data Items (CDI) report from a DVP function and a snapshot
#'
#' Identical machinery to [save_dvr()] with its own label. A CDI has no update
#' comparison, so `before` is not accepted.
#' @inheritParams save_dvr
#' @return Invisibly, a list with the report id, directories written, sheets,
#'   and per-check counts.
#' @examples
#' \dontrun{
#' dvp <- function(data) list(weight_range = data$records[data$records$weight_kg > 300, ])
#' save_cdi(dvp, after = load_snapshot("latest"))
#' }
#' @export
save_cdi <- function(dvp, after, paths = getwd(), id_col = "record_id",
                     site_col = NULL, version = NULL, operator = NULL,
                     check_info = NULL, query_column = TRUE,
                     write_readable = FALSE, verbose = 2L) {
  run_data_report(dvp, after, before = NULL, paths, kind = "cdi", id_col = id_col,
                  site_col = site_col, version = version, operator = operator,
                  check_info = check_info, query_column = query_column,
                  write_readable = write_readable, verbose = verbose)
}

#' Shared engine behind [save_dvr()] and [save_cdi()]
#'
#' Explicitly named (no hidden helper): the DVR and CDI wrappers differ only in
#' their `kind` label and whether an update comparison is offered.
#' @inheritParams save_dvr
#' @param kind `"dvr"` or `"cdi"`.
#' @return Invisibly, a list describing the written report.
#' @export
run_data_report <- function(dvp, after, before = NULL, paths = getwd(),
                            kind = c("dvr", "cdi"), id_col = "record_id",
                            site_col = NULL, version = NULL, operator = NULL,
                            check_info = NULL, query_column = TRUE,
                            write_readable = FALSE, verbose = 2L) {
  kind <- match.arg(kind)
  if (!requireNamespace("openxlsx", quietly = TRUE))
    cli::cli_abort(c(
      "The {.pkg openxlsx} package is required: the {toupper(kind)} is delivered as one Excel workbook.",
      "i" = "Install it with {.code install.packages(\"openxlsx\")}, then rerun."))
  paths <- as.character(paths)
  if (!length(paths))
    cli::cli_abort("{.arg paths} must name at least one output directory.")
  operator <- operator %||% unname(Sys.info()[["user"]]) %||% "unknown"
  trial <- report_trial_name(after)
  compared <- !is.null(before)

  sheets <- if (compared) compare_dvp(dvp, before, after) else run_dvp(dvp, after)

  info <- check_info %||% attr(sheets, "check_info")
  if (!is.null(info)) {
    info <- validate_check_info(info, names(sheets))
    if (isTRUE(query_column)) sheets <- add_query_column(sheets, info)
  }

  update_sheets <- NULL
  if (compared) {
    update_sheets <- stats::setNames(lapply(sheets, function(d) {
      if (!("status" %in% names(d)) || nrow(d) == 0L) return(d[0, , drop = FALSE])
      d[d$status %in% c("new", "resolved"), , drop = FALSE]
    }), names(sheets))
  }

  # ---- per-check counts (with status tallies when compared) ----
  check_summaries <- lapply(names(sheets), function(nm) {
    df <- sheets[[nm]]
    rec <- list(name = nm, rows = nrow(df))
    if (compared && "status" %in% names(df)) {
      rec$new       <- sum(df$status == "new")
      rec$unchanged <- sum(df$status == "unchanged")
      rec$resolved  <- sum(df$status == "resolved")
    }
    if (!is.null(info)) {
      row <- match(nm, info$check)
      if (!is.na(row)) {
        rec$query <- as.character(info$query[row])
        if ("section" %in% names(info) && !is.na(info$section[row]))
          rec$section <- as.character(info$section[row])
        if ("critical" %in% names(info) && !is.na(info$critical[row]))
          rec$critical <- info$critical[row]
      }
    }
    rec
  })
  total_rows <- sum(vapply(sheets, nrow, integer(1)))

  # ---- one report id shared across every destination ----
  now <- utc_now()
  # The report is keyed by the DATA it validated: the id and directory carry
  # the after-snapshot's id (and the document version when given). A second
  # run on the same snapshot and version gets an explicit _N-suffixed
  # directory (the while-loop below), never a silent overwrite. The run moment
  # is recorded as created_utc in the manifest. An unsaved after snapshot has
  # no id, so the run time stands in for it.
  id <- attr(after, "id") %||% snapshot_id(now)
  ver <- if (!is.null(version)) paste0("v", sub("^[vV]", "", as.character(version))) else NULL
  dvr_id <- paste(Filter(Negate(is.null), list(toupper(kind), ver, id)), collapse = "-")
  base <- paste(Filter(nzchar, c(trial, toupper(kind), id, ver %||% "")), collapse = "_")

  versions <- list(
    bctu = tryCatch(as.character(utils::packageVersion("bctu")), error = function(e) NA_character_),
    r    = R.version.string)
  snapshot_ref <- function(s) {
    if (is.null(s)) return(NULL)
    list(id = attr(s, "id") %||% NA_character_,
         sha256 = tryCatch(snapshot_fingerprint(s), error = function(e) NA_character_))
  }
  manifest <- list(
    schema = "bctu-dvr/1",
    kind = kind,
    dvr_id = dvr_id,
    trial = trial,
    tag = (attr(after, "bctu_tag") %||% attr(after, "bctu_meta")$tag) %||% NA_character_,
    version = version %||% NA_character_,
    created_utc = iso8601(now),
    operator = operator,
    compared = compared,
    after_snapshot = snapshot_ref(after),
    before_snapshot = snapshot_ref(before),
    id_col = id_col,
    site_col = site_col %||% NA_character_,
    checks = check_summaries,
    total_findings = total_rows,
    versions = versions
  )

  written_dirs <- character(0)
  for (p in paths) {
    if (!dir.exists(p)) dir.create(p, recursive = TRUE, showWarnings = FALSE)
    # House layout: <path>/<after-snapshot-id>/v<version>/ (a folder per data
    # state, then a folder per controlled document version); an unversioned
    # report writes <path>/<after-snapshot-id>/ directly. A rerun of the same
    # snapshot and version gets an explicit _N suffix on the leaf folder,
    # never a silent overwrite.
    base_dir <- if (!is.null(ver)) file.path(p, id) else p
    leaf     <- ver %||% id
    report_dir <- file.path(base_dir, leaf)
    suffix <- 0L
    while (dir.exists(report_dir)) {
      suffix <- suffix + 1L
      report_dir <- file.path(base_dir, paste0(leaf, "_", suffix))
    }
    dir.create(report_dir, recursive = TRUE, showWarnings = FALSE)

    write_report_set(sheets, after, file.path(report_dir, "full"), base,
                     id_col = id_col, site_col = site_col,
                     write_readable = write_readable,
                     before_snapshot = before, check_info = info)
    if (compared)
      write_report_set(update_sheets, after, file.path(report_dir, "update"),
                       paste0(base, "_Update"), id_col = id_col,
                       site_col = site_col, write_readable = write_readable,
                       before_snapshot = before, check_info = info)
    yaml::write_yaml(manifest, file.path(report_dir, manifest_filename))
    written_dirs <- c(written_dirs, report_dir)
  }

  if (verbose >= 1L) {
    cli::cli_alert_success("{toupper(kind)} {.val {dvr_id}} issued -> {length(written_dirs)} location{?s}")
    if (verbose >= 2L)
      cli::cli_alert_info("{length(nonempty_checks(sheets))} check{?s} with findings, {total_rows} finding{?s}{if (compared) ' (compared to a before snapshot)' else ''}.")
  }

  invisible(list(
    dvr_id = dvr_id, kind = kind, trial = trial, dirs = written_dirs,
    compared = compared, sheets = sheets, update = update_sheets,
    checks = check_summaries, total_findings = total_rows, manifest = manifest))
}
