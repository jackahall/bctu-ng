# ---------------------------------------------------------------------------
# Checks: DVP (Data Validation) and CDI (Critical Data Items)
# ---------------------------------------------------------------------------
# A check reads a snapshot's tables and returns a tidy set of findings. Every
# finding has the same fixed shape, so the identity of a finding is always
# well-defined and two reports can be compared reliably.
#
# The central design point is the UPDATE DIFF. When a Data Validation Report
# (DVR) is issued, we record which snapshot it was built from in a DVR ledger.
# The next report's "what is new since last time" is computed against THAT
# recorded snapshot, never against "whatever snapshot happens to be second
# newest on disk". A stray snapshot taken between two reports can therefore
# never become the baseline. If the recorded baseline snapshot cannot be
# loaded, we stop with a clear error rather than silently reporting everything
# as new.

# --- the fixed finding schema ----------------------------------------------
# Every finding is one row with exactly these columns, in this order.
finding_columns <- c(
  "record_id", "event", "form", "field", "value", "reason",
  "check_id", "severity"
)

# A check author must always supply at least these; the rest default to NA.
required_finding_columns <- c("record_id", "field", "reason")

#' A clean (zero-finding) findings table
#'
#' Use this to say "this check found nothing". It has the correct columns and
#' no rows.
#' @return An empty findings data frame with the fixed schema.
#' @export
no_findings <- function() {
  empty <- stats::setNames(
    replicate(length(finding_columns), character(0), simplify = FALSE),
    finding_columns
  )
  structure(as.data.frame(empty, stringsAsFactors = FALSE),
            class = c("bctu_findings", "data.frame"))
}

#' Build a valid findings table
#'
#' The convenient way for a check to report findings. Supply one entry per
#' finding (vectors are recycled). Only `record_id`, `field` and `reason` are
#' required; `event`, `form`, `value`, `check_id` and `severity` default to
#' missing and are usually filled in for you when the check runs.
#' @param record_id Participant/record identifier.
#' @param field The data field the finding is about.
#' @param reason Plain-English description of the problem.
#' @param value The offending value (optional).
#' @param event Event/visit (optional).
#' @param form CRF/form name (optional).
#' @param check_id Check identifier (optional; stamped from the check).
#' @param severity Severity label (optional; stamped from the check).
#' @return A findings data frame with the fixed schema.
#' @export
new_findings <- function(record_id, field, reason, value = NA,
                         event = NA, form = NA, check_id = NA, severity = NA) {
  df <- data.frame(
    record_id = as.character(record_id),
    event     = as.character(event),
    form      = as.character(form),
    field     = as.character(field),
    value     = as.character(value),
    reason    = as.character(reason),
    check_id  = as.character(check_id),
    severity  = as.character(severity),
    stringsAsFactors = FALSE
  )
  as_findings(df)
}

#' Validate and normalise a findings table at the check boundary
#'
#' Accepts a data frame produced by a check and returns it in the fixed
#' schema. Rejects any column that is not part of the schema (so a check cannot
#' quietly introduce a column that would break the update diff), and requires
#' the mandatory columns. Missing optional columns are added as `NA`. All
#' values are stored as text so findings compare cleanly across reports.
#' @param x A data frame, a findings table, or `NULL`/zero rows (treated clean).
#' @return A `bctu_findings` data frame.
#' @export
as_findings <- function(x) {
  if (is.null(x)) return(no_findings())
  if (!is.data.frame(x))
    cli::cli_abort(c(
      "A check must return a data frame of findings.",
      "i" = "Build it with {.fn new_findings} or return {.fn no_findings}."))
  if (nrow(x) == 0L) return(no_findings())

  unknown <- setdiff(names(x), finding_columns)
  if (length(unknown))
    cli::cli_abort(c(
      "A check returned column{?s} not allowed in the finding schema: {.val {unknown}}.",
      "i" = "Allowed columns are: {.val {finding_columns}}.",
      "x" = "Arbitrary columns are rejected so the update-diff key stays well-defined."))

  missing_required <- setdiff(required_finding_columns, names(x))
  if (length(missing_required))
    cli::cli_abort("A check is missing required finding column{?s}: {.val {missing_required}}.")

  for (col in finding_columns)
    x[[col]] <- if (col %in% names(x)) as.character(x[[col]]) else NA_character_

  x <- x[, finding_columns, drop = FALSE]
  rownames(x) <- NULL
  structure(x, class = c("bctu_findings", "data.frame"))
}

#' @export
print.bctu_findings <- function(x, ...) {
  cli::cli_text("{.strong bctu findings}: {nrow(x)} row{?s}")
  if (nrow(x)) print(as.data.frame(x), row.names = FALSE)
  invisible(x)
}

# --- a check ----------------------------------------------------------------
#' Define a data-validation check
#'
#' A check is a named rule that reads a snapshot's tables and returns findings.
#' The `run` function receives the snapshot's tables (a named list of data
#' frames) and must return a findings table (use [new_findings()] or
#' [no_findings()]).
#' @param id Short identifier, e.g. `"weight_range"`. Stamped onto findings.
#' @param run `function(tables)` returning a findings table.
#' @param description Plain-English description of what the check looks for.
#' @param default_severity Severity stamped onto findings that do not set one.
#' @return A `bctu_check` object.
#' @export
bctu_check <- function(id, run, description = "", default_severity = "query") {
  if (!is_string(id)) cli::cli_abort("{.arg id} must be a single string.")
  if (!is.function(run)) cli::cli_abort("{.arg run} must be a function of {.arg tables}.")
  if (!is_string(default_severity))
    cli::cli_abort("{.arg default_severity} must be a single string.")
  structure(
    list(id = id, run = run, description = description,
         default_severity = default_severity),
    class = "bctu_check"
  )
}

#' @export
print.bctu_check <- function(x, ...) {
  cli::cli_rule("bctu check {.val {x$id}}")
  if (nzchar(x$description)) cli::cli_text(x$description)
  cli::cli_text("default severity: {.val {x$default_severity}}")
  invisible(x)
}

#' Content hash of a check (identifies the exact rule that ran)
#'
#' Two checks with the same id but different logic hash differently, so the
#' audit record pins the rule that actually produced the findings.
#' @param check A [bctu_check()].
#' @return A short hex string.
#' @export
check_hash <- function(check) {
  if (!inherits(check, "bctu_check")) cli::cli_abort("{.arg check} must be a {.cls bctu_check}.")
  digest::digest(list(
    id = check$id,
    default_severity = check$default_severity,
    body = paste(deparse(body(check$run)), collapse = "\n"),
    args = names(formals(check$run))
  ), algo = "sha256")
}

#' Run a check on a snapshot's tables and return stamped findings
#'
#' Executes the check, validates its output against the fixed schema, and
#' stamps the check id and default severity onto any finding that did not set
#' them.
#' @param check A [bctu_check()].
#' @param tables A named list of data frames (a snapshot is one).
#' @return A `bctu_findings` data frame.
#' @export
run_check <- function(check, tables) {
  if (!inherits(check, "bctu_check")) cli::cli_abort("{.arg check} must be a {.cls bctu_check}.")
  findings <- as_findings(check$run(tables))
  if (nrow(findings)) {
    blank <- function(v) is.na(v) | !nzchar(v)
    findings$check_id[blank(findings$check_id)] <- check$id
    findings$severity[blank(findings$severity)] <- check$default_severity
  }
  findings
}

# --- update diff (the corrected baseline logic) ----------------------------
# Finding identity is check_id + record_id + field. It deliberately does NOT
# include value or reason, so a still-open finding whose value was re-derived
# to a slightly different float is recognised as the SAME finding (classified
# "changed"), not dropped and re-added.

#' Stable identity key of each finding (check_id + record_id + field)
#' @param findings A findings table.
#' @return A character vector of identity keys.
#' @export
finding_identity <- function(findings) {
  paste(findings$check_id, findings$record_id, findings$field, sep = "\x1f")
}

#' Classify current findings against a baseline set
#'
#' Every current finding is labelled `new` (identity absent from baseline),
#' `changed` (identity present but value or reason differs) or `unchanged`.
#' Every baseline finding whose identity is gone is labelled `resolved`. All
#' four classes are returned (resolutions are audit-relevant).
#' @param current Current findings.
#' @param baseline Baseline findings (zero rows if there was no prior report).
#' @return A findings data frame with an added `change_class` column.
#' @export
classify_findings <- function(current, baseline) {
  current  <- as_findings(current)
  baseline <- as_findings(baseline)

  cur_key  <- finding_identity(current)
  base_key <- finding_identity(baseline)

  cur_class <- character(nrow(current))
  hit <- match(cur_key, base_key)
  for (i in seq_len(nrow(current))) {
    j <- hit[i]
    if (is.na(j)) {
      cur_class[i] <- "new"
    } else {
      same_value  <- identical(current$value[i],  baseline$value[j])
      same_reason <- identical(current$reason[i], baseline$reason[j])
      cur_class[i] <- if (same_value && same_reason) "unchanged" else "changed"
    }
  }
  current$change_class <- cur_class

  resolved_idx <- which(!(base_key %in% cur_key))
  resolved <- baseline[resolved_idx, , drop = FALSE]
  resolved$change_class <- rep("resolved", nrow(resolved))

  out <- rbind(current, resolved)
  rownames(out) <- NULL
  structure(out, class = c("bctu_findings", "data.frame"))
}

# --- DVR ledger (records which snapshot each issued report was built from) --
#' Read the append-only DVR ledger for a checks output directory
#' @param output_dir Directory holding the reports and the ledger.
#' @param kind `"dvr"` or `"cdi"` (each kind has its own ledger file).
#' @return A list of ledger records, oldest first.
#' @export
read_dvr_ledger <- function(output_dir, kind = c("dvr", "cdi")) {
  kind <- match.arg(kind)
  path <- file.path(output_dir, paste0(toupper(kind), ".log.yml"))
  if (!file.exists(path)) return(list())
  txt <- paste(readLines(path), collapse = "\n")
  docs <- strsplit(txt, "(^|\n)---\\s*\n")[[1]]
  docs <- docs[nzchar(trimws(docs))]
  lapply(docs, yaml::yaml.load)
}

#' Append one record to a DVR ledger (append-only)
#' @keywords internal
append_dvr_ledger <- function(output_dir, kind, record) {
  path <- file.path(output_dir, paste0(toupper(kind), ".log.yml"))
  cat("---\n", yaml::as.yaml(record), sep = "", file = path, append = TRUE)
  invisible(path)
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

# --- site resolution (for optional per-site workbooks) ---------------------
#' Resolve a site for each finding from findings and data
#'
#' The finding schema has no site column, so sites are looked up from the
#' snapshot's data: any table carrying both `record_id` and `site_field`
#' provides the map. Findings whose record has no site come back as `NA`.
#' @param findings A findings table.
#' @param tables The snapshot's tables (a named list of data frames).
#' @param site_field Name of the site column in the data. Default `"site"`.
#' @return A character vector of sites, one per finding row.
#' @export
resolve_sites <- function(findings, tables, site_field = "site") {
  site_of <- character(0)
  for (nm in names(tables)) {
    tab <- tables[[nm]]
    if (all(c("record_id", site_field) %in% names(tab))) {
      map <- stats::setNames(as.character(tab[[site_field]]), as.character(tab$record_id))
      site_of <- c(site_of, map[!names(map) %in% names(site_of)])
    }
  }
  unname(site_of[as.character(findings$record_id)])
}

# --- readable + optional xlsx writers --------------------------------------
#' Write a findings table as a readable CSV and a plain-text copy
#' @keywords internal
write_findings_readable <- function(findings, path_no_ext) {
  utils::write.csv(as.data.frame(findings), paste0(path_no_ext, ".csv"),
                   row.names = FALSE, na = "")
  txt <- if (nrow(findings)) utils::capture.output(print(as.data.frame(findings), row.names = FALSE))
         else "(no findings)"
  writeLines(txt, paste0(path_no_ext, ".txt"))
  invisible(path_no_ext)
}

#' Optionally write findings to an Excel workbook (thin formatter)
#'
#' A convenience only: the CSV/TXT copies carry the same content, so the check
#' logic is fully testable without Excel. Uses `openxlsx` if installed; if not,
#' warns and does nothing. With `per_site`, one sheet per site is written and
#' findings with no site go to a `NO_SITE` sheet.
#' @keywords internal
write_findings_xlsx <- function(sheets, path, sites = NULL, per_site = FALSE) {
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    cli::cli_warn(c("{.pkg openxlsx} is not installed; skipping the Excel workbook.",
                    "i" = "The CSV and TXT copies contain the same findings."))
    return(invisible(NULL))
  }
  wb <- openxlsx::createWorkbook()
  for (nm in names(sheets)) {
    openxlsx::addWorksheet(wb, nm)
    openxlsx::writeData(wb, nm, as.data.frame(sheets[[nm]]))
  }
  if (per_site && !is.null(sites)) {
    full <- sheets[["Full"]]
    site_vec <- ifelse(is.na(sites) | !nzchar(sites), "NO_SITE", sites)
    for (s in sort(unique(site_vec))) {
      sheet <- substr(gsub("[^A-Za-z0-9_ -]", "_", s), 1L, 31L)
      openxlsx::addWorksheet(wb, sheet)
      openxlsx::writeData(wb, sheet, as.data.frame(full[site_vec == s, , drop = FALSE]))
    }
  }
  openxlsx::saveWorkbook(wb, path, overwrite = FALSE)
  invisible(path)
}

# --- the report engine ------------------------------------------------------
#' Build a Data Validation Report (DVR) from a snapshot and a check
#'
#' Runs `check` on `snapshot`, writes the FULL findings set and an UPDATE set
#' (new / changed / resolved since the last issued DVR), records an auditable
#' manifest and a DVR-ledger entry, and returns the result invisibly. The
#' update diff is computed against the snapshot the LAST ISSUED DVR was built
#' from (read from the DVR ledger), never against a directory position.
#' @param snapshot A saved `bctu_snapshot` (must carry `id` and `dir`).
#' @param check A [bctu_check()].
#' @param output_dir Directory to hold DVRs and the DVR ledger.
#' @param operator Person issuing the report (recorded); default the OS user.
#' @param write_xlsx Also write an Excel workbook if `openxlsx` is available?
#' @param per_site Write per-site sheets in the workbook?
#' @param site_field Name of the site column in the data. Default `"site"`.
#' @param verbose Verbosity.
#' @return Invisibly, a list with the report id, paths, and per-class counts.
#' @export
save_dvr <- function(snapshot, check, output_dir, operator = NULL,
                     write_xlsx = TRUE, per_site = FALSE, site_field = "site",
                     verbose = 2L) {
  run_data_report(snapshot, check, output_dir, kind = "dvr", operator = operator,
                  write_xlsx = write_xlsx, per_site = per_site,
                  site_field = site_field, verbose = verbose)
}

#' Build a Critical Data Items (CDI) report from a snapshot and a check
#'
#' Identical machinery to [save_dvr()] with its own ledger and file naming.
#' @inheritParams save_dvr
#' @return Invisibly, a list with the report id, paths, and per-class counts.
#' @export
save_cdi <- function(snapshot, check, output_dir, operator = NULL,
                     write_xlsx = TRUE, per_site = FALSE, site_field = "site",
                     verbose = 2L) {
  run_data_report(snapshot, check, output_dir, kind = "cdi", operator = operator,
                  write_xlsx = write_xlsx, per_site = per_site,
                  site_field = site_field, verbose = verbose)
}

#' Shared engine behind [save_dvr()] and [save_cdi()]
#'
#' Explicitly named (no hidden helper): the DVR and CDI wrappers differ only in
#' their `kind` label, ledger file, and output naming.
#' @inheritParams save_dvr
#' @param kind `"dvr"` or `"cdi"`.
#' @return Invisibly, a list describing the written report.
#' @export
run_data_report <- function(snapshot, check, output_dir, kind = c("dvr", "cdi"),
                            operator = NULL, write_xlsx = TRUE, per_site = FALSE,
                            site_field = "site", verbose = 2L) {
  kind <- match.arg(kind)
  if (!inherits(snapshot, "bctu_snapshot"))
    cli::cli_abort("{.arg snapshot} must be a {.cls bctu_snapshot}.")
  if (!inherits(check, "bctu_check"))
    cli::cli_abort("{.arg check} must be a {.cls bctu_check}.")

  current_id  <- attr(snapshot, "id")
  current_dir <- attr(snapshot, "dir")
  if (is.null(current_id) || is.null(current_dir))
    cli::cli_abort(c(
      "This snapshot has not been saved to a store, so it has no id to report from.",
      "i" = "Take it with {.fn take_snapshot} (or {.fn save_snapshot}) first."))
  store <- dirname(current_dir)
  operator <- operator %||% unname(Sys.info()[["user"]]) %||% "unknown"

  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  chash <- check_hash(check)
  current_findings <- run_check(check, snapshot)

  # ---- resolve the baseline from the DVR ledger (NOT directory position) ----
  ledger <- read_dvr_ledger(output_dir, kind)
  prior <- Filter(function(r) identical(r$kind, kind) &&
                              identical(r$check_id, check$id), ledger)

  if (length(prior) == 0L) {
    baseline_id <- NA_character_
    baseline_sha <- NA_character_
    baseline_dvr_id <- NA_character_
    baseline_findings <- no_findings()
    baseline_decision <- paste0(
      "No prior issued ", toupper(kind), " for check '", check$id,
      "': every finding is new.")
  } else {
    last <- prior[[length(prior)]]
    baseline_id <- last$snapshot_id
    baseline_dvr_id <- last$dvr_id
    baseline_snapshot <- tryCatch(
      load_snapshot(baseline_id, store = store, verbose = 0L),
      error = function(e) e)
    if (inherits(baseline_snapshot, "condition"))
      cli::cli_abort(c(
        "The baseline snapshot for the update diff could not be loaded.",
        "x" = "Last issued {toupper(kind)} {.val {baseline_dvr_id}} was built from snapshot {.val {baseline_id}}, which is missing or unreadable in {.file {store}}.",
        "i" = "Restore that snapshot, or re-issue a baseline; bctu will NOT silently report every finding as new.",
        "!" = conditionMessage(baseline_snapshot)))
    baseline_sha <- snapshot_fingerprint(baseline_snapshot)
    if (!identical(last$check_hash, chash))
      cli::cli_warn(c(
        "The check content has changed since the baseline {toupper(kind)} was issued.",
        "i" = "Baseline findings are re-derived with the CURRENT check on snapshot {.val {baseline_id}}."))
    baseline_findings <- run_check(check, baseline_snapshot)
    baseline_decision <- paste0(
      "Baseline = snapshot '", baseline_id, "' from last issued ", toupper(kind),
      " '", baseline_dvr_id, "' (check '", check$id, "').")
  }

  classified <- classify_findings(current_findings, baseline_findings)

  full   <- classified[classified$change_class %in% c("new", "changed", "unchanged"), , drop = FALSE]
  update <- classified[classified$change_class %in% c("new", "changed", "resolved"), , drop = FALSE]
  rownames(full) <- NULL; rownames(update) <- NULL

  counts <- c(
    new       = sum(classified$change_class == "new"),
    changed   = sum(classified$change_class == "changed"),
    unchanged = sum(classified$change_class == "unchanged"),
    resolved  = sum(classified$change_class == "resolved")
  )

  # ---- append-only report directory (never overwrite an issued report) ----
  now <- utc_now()
  dvr_id <- paste0(toupper(kind), "-", snapshot_id(now))
  report_dir <- file.path(output_dir, dvr_id)
  while (dir.exists(report_dir)) {
    now <- now + 1L
    dvr_id <- paste0(toupper(kind), "-", snapshot_id(now))
    report_dir <- file.path(output_dir, dvr_id)
  }
  dir.create(report_dir, recursive = TRUE, showWarnings = FALSE)

  write_findings_readable(full,   file.path(report_dir, "findings_full"))
  write_findings_readable(update, file.path(report_dir, "findings_update"))

  # ---- site coverage (built from findings UNION data) ----
  sites <- resolve_sites(full, snapshot, site_field = site_field)
  no_site_count <- sum(is.na(sites) | !nzchar(sites))
  site_note <- if (nrow(full) == 0L) "no findings"
    else if (!any(vapply(snapshot, function(t) site_field %in% names(t), logical(1))))
      paste0("no '", site_field, "' column in the snapshot: all ", nrow(full),
             " finding(s) are unassigned to a site")
    else paste0(no_site_count, " of ", nrow(full),
                " finding(s) have a record with no site")

  if (isTRUE(write_xlsx))
    write_findings_xlsx(list(Full = full, Update = update),
                        file.path(report_dir, "findings.xlsx"),
                        sites = sites, per_site = per_site)

  # ---- auditable, human-readable YAML manifest ----
  current_sha <- snapshot_fingerprint(snapshot)
  versions <- list(
    bctu = tryCatch(as.character(utils::packageVersion("bctu")), error = function(e) NA_character_),
    r    = R.version.string)
  manifest <- list(
    schema = "bctu-dvr/1",
    kind = kind,
    dvr_id = dvr_id,
    created_utc = iso8601(now),
    operator = operator,
    check = list(id = check$id, description = check$description, hash = chash),
    current_snapshot = list(id = current_id, sha256 = current_sha),
    baseline_snapshot = list(id = baseline_id, sha256 = baseline_sha,
                             dvr_id = baseline_dvr_id),
    baseline_decision = baseline_decision,
    counts = as.list(counts),
    full_count = nrow(full),
    update_count = nrow(update),
    site_field = site_field,
    site_coverage = site_note,
    findings_no_site = no_site_count,
    versions = versions
  )
  yaml::write_yaml(manifest, file.path(report_dir, manifest_filename))

  # ---- append the DVR-ledger record (this issued report is now the baseline
  #      for the next one) ----
  append_dvr_ledger(output_dir, kind, list(
    dvr_id = dvr_id,
    kind = kind,
    check_id = check$id,
    check_hash = chash,
    snapshot_id = current_id,
    snapshot_sha256 = current_sha,
    baseline_snapshot_id = baseline_id,
    baseline_dvr_id = baseline_dvr_id,
    counts = as.list(counts),
    operator = operator,
    bctu_version = versions$bctu,
    r_version = versions$r,
    at = iso8601(now)
  ))

  if (verbose >= 1L) {
    cli::cli_alert_success("{toupper(kind)} {.val {dvr_id}} issued -> {.file {report_dir}}")
    cli::cli_alert_info("baseline: {baseline_decision}")
    cli::cli_alert_info("full: {nrow(full)} | update: {nrow(update)} (new {counts[['new']]}, changed {counts[['changed']]}, resolved {counts[['resolved']]}); unchanged {counts[['unchanged']]}")
  }

  invisible(list(
    dvr_id = dvr_id, kind = kind, dir = report_dir,
    current_snapshot_id = current_id, baseline_snapshot_id = baseline_id,
    counts = counts, full = full, update = update, manifest = manifest))
}
