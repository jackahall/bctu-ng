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
#' @export
compare_dvp <- function(dvp, before, after) {
  b <- run_dvp(dvp, before)
  a <- run_dvp(dvp, after)

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
  out
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

#' Site label for each finding row, looked up from one or more snapshots
#'
#' Findings carry only what the check selected, so sites are resolved from the
#' snapshot(s): any table holding both `id_col` and `site_col` maps a record to
#' a site. Pass both the `before` and `after` snapshots when comparing, so a
#' `resolved` finding whose record was removed from `after` is still sited from
#' `before`. Findings whose record has no site in any snapshot (or whose frame
#' has no `id_col`) are labelled `NO_SITE` so they are never silently dropped
#' from the split.
#' @param findings A findings data frame.
#' @param snapshots A list of snapshots to union (earlier snapshots take
#'   priority when a record's site conflicts across snapshots).
#' @param id_col Name of the record-id column shared by findings and data.
#' @param site_col Name of the site column in the data.
#' @return A character vector of sites, one per finding row.
#' @export
resolve_finding_sites <- function(findings, snapshots, id_col, site_col) {
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
  site <- unname(map[ids])
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
#' @return Invisibly, the path (or `NULL` if `openxlsx` is unavailable or there
#'   is nothing to write).
#' @export
write_findings_workbook <- function(sheets, path) {
  if (!length(sheets)) return(invisible(NULL))
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    cli::cli_warn(c("{.pkg openxlsx} is not installed; skipping the Excel workbook.",
                    "i" = "The CSV and TXT copies contain the same findings."))
    return(invisible(NULL))
  }
  wb <- openxlsx::createWorkbook()
  used <- character(0)
  for (nm in names(sheets)) {
    sheet <- substr(gsub("[^A-Za-z0-9_ -]", "_", nm), 1L, 31L)
    while (sheet %in% used) sheet <- substr(paste0(sheet, "_"), 1L, 31L)
    used <- c(used, sheet)
    openxlsx::addWorksheet(wb, sheet)
    openxlsx::writeData(wb, sheet, as.data.frame(sheets[[nm]]))
  }
  openxlsx::saveWorkbook(wb, path, overwrite = FALSE)
  invisible(path)
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
#' Writes the readable CSV/TXT copies and, when `write_xlsx`, an overall
#' workbook of all findings. When `site_col` is given, each finding is assigned
#' a site and a per-site CSV/TXT pair (plus, when `write_xlsx`, a per-site
#' workbook) is written under `sites/`, so a centre receives only its own
#' queries alongside the overall set; this per-site split does not require
#' `openxlsx`. Sites are resolved from `snapshot`, and from `before_snapshot`
#' too when given, so a `resolved` finding whose record was removed from
#' `snapshot` is still sited correctly. A `cli_warn` is raised per check that
#' has findings with no resolvable site.
#' @param sheets A named list of findings data frames (empty checks omitted).
#' @param snapshot The snapshot the findings came from (for site lookup).
#' @param dir Directory to write this set into.
#' @param base_name File stem for the workbooks.
#' @param id_col,site_col Columns used to split by site; `site_col = NULL`
#'   writes the overall workbook only.
#' @param write_xlsx Write Excel workbooks (needs `openxlsx`)?
#' @param before_snapshot Optional earlier snapshot, unioned with `snapshot`
#'   for site lookup (see [resolve_finding_sites()]).
#' @return Invisibly, the directory.
#' @export
write_report_set <- function(sheets, snapshot, dir, base_name,
                             id_col = "record_id", site_col = NULL,
                             write_xlsx = TRUE, before_snapshot = NULL) {
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  filled <- nonempty_checks(sheets)

  write_findings_readable(sheets, dir)

  if (isTRUE(write_xlsx) && length(filled))
    write_findings_workbook(filled, file.path(dir, paste0(base_name, ".xlsx")))

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
      write_findings_readable(per_check, sdir)
      if (isTRUE(write_xlsx))
        write_findings_workbook(per_check,
          file.path(sdir, paste0(base_name, "_", safe_site, ".xlsx")))
    }
  }
  invisible(dir)
}

# --- the report engine ------------------------------------------------------
#' Build a Data Validation Report (DVR) from a DVP function and a snapshot
#'
#' Runs `dvp` on the `after` snapshot and writes, to every directory in `paths`,
#' an overall workbook (one worksheet per non-empty check) plus per-site
#' workbooks when `site_col` is given, the readable CSV/TXT copies, and an
#' auditable YAML manifest. When a `before` snapshot is supplied, each finding
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
#' @param write_xlsx Also write Excel workbooks if `openxlsx` is available?
#' @param verbose Verbosity.
#' @return Invisibly, a list with the report id, directories written, sheets,
#'   and per-check counts.
#' @export
save_dvr <- function(dvp, after, before = NULL, paths = getwd(),
                     id_col = "record_id", site_col = NULL, version = NULL,
                     operator = NULL, write_xlsx = TRUE, verbose = 2L) {
  run_data_report(dvp, after, before, paths, kind = "dvr", id_col = id_col,
                  site_col = site_col, version = version, operator = operator,
                  write_xlsx = write_xlsx, verbose = verbose)
}

#' Build a Critical Data Items (CDI) report from a DVP function and a snapshot
#'
#' Identical machinery to [save_dvr()] with its own label. A CDI has no update
#' comparison, so `before` is not accepted.
#' @inheritParams save_dvr
#' @return Invisibly, a list with the report id, directories written, sheets,
#'   and per-check counts.
#' @export
save_cdi <- function(dvp, after, paths = getwd(), id_col = "record_id",
                     site_col = NULL, version = NULL, operator = NULL,
                     write_xlsx = TRUE, verbose = 2L) {
  run_data_report(dvp, after, before = NULL, paths, kind = "cdi", id_col = id_col,
                  site_col = site_col, version = version, operator = operator,
                  write_xlsx = write_xlsx, verbose = verbose)
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
                            write_xlsx = TRUE, verbose = 2L) {
  kind <- match.arg(kind)
  paths <- as.character(paths)
  if (!length(paths))
    cli::cli_abort("{.arg paths} must name at least one output directory.")
  operator <- operator %||% unname(Sys.info()[["user"]]) %||% "unknown"
  trial <- report_trial_name(after)
  compared <- !is.null(before)

  sheets <- if (compared) compare_dvp(dvp, before, after) else run_dvp(dvp, after)

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
    rec
  })
  total_rows <- sum(vapply(sheets, nrow, integer(1)))

  # ---- one report id shared across every destination ----
  now <- utc_now()
  id <- snapshot_id(now)
  dvr_id <- paste0(toupper(kind), "-", id)
  ver <- if (!is.null(version)) paste0("v", sub("^[vV]", "", as.character(version))) else NULL
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
    report_dir <- file.path(p, dvr_id)
    suffix <- 0L
    while (dir.exists(report_dir)) {
      suffix <- suffix + 1L
      report_dir <- file.path(p, paste0(dvr_id, "_", suffix))
    }
    dir.create(report_dir, recursive = TRUE, showWarnings = FALSE)

    write_report_set(sheets, after, file.path(report_dir, "full"), base,
                     id_col = id_col, site_col = site_col, write_xlsx = write_xlsx,
                     before_snapshot = before)
    if (compared)
      write_report_set(update_sheets, after, file.path(report_dir, "update"),
                       paste0(base, "_Update"), id_col = id_col,
                       site_col = site_col, write_xlsx = write_xlsx,
                       before_snapshot = before)
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
