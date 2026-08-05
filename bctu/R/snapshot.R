# ---------------------------------------------------------------------------
# Snapshots
# ---------------------------------------------------------------------------
# A snapshot is an immutable, timestamped capture of one or more named tables.
# On disk:
#   <store>/<id>/
#     manifest.yml                     human-readable + machine-readable (GCP)
#     tables/<table>/<table>.rds       typed canonical payload
#     tables/<table>/<table>.csv       readable exchange copy
# Integrity is SHA-256 recorded in the manifest (the authoritative control).
# Every extraction and deletion is appended to an append-only audit ledger at
# <store>/SNAPSHOTS.log.yml. Nothing is committed to git and nothing is made
# read-only, so ordinary git and delete operations never fight the filesystem.

manifest_filename     <- "manifest.yml"
ledger_filename       <- "SNAPSHOTS.log.yml"
snapshot_schema       <- "bctu-snapshot/1"

# --- in-memory snapshot object ---------------------------------------------
#' @keywords internal
as_snapshot <- function(tables, source, name) {
  structure(tables,
            class = c("bctu_snapshot", "list"),
            bctu_meta = list(name = name, source = source,
                             fetched_utc = iso8601(),
                             checkpoint = checkpoint()))
}
#' @export
print.bctu_snapshot <- function(x, ...) {
  m <- attr(x, "bctu_meta")
  cli::cli_rule("bctu snapshot {.val {m$name}}{if (!is.null(attr(x,'id'))) paste0(' [', attr(x,'id'), ']') else ''}")
  for (nm in names(x))
    cli::cli_text("{.field {nm}}: {nrow(x[[nm]])} rows x {ncol(x[[nm]])} cols")
  invisible(x)
}

#' A lightweight provenance checkpoint
#' @export
checkpoint <- function() {
  ver <- tryCatch(as.character(utils::packageVersion("bctu")), error = function(e) NA_character_)
  list(created_utc = iso8601(), r_version = R.version.string,
       bctu_version = ver,
       user = unname(Sys.info()[["user"]]) %||% "unknown",
       host = unname(Sys.info()[["nodename"]]) %||% "unknown")
}

# --- take + save ------------------------------------------------------------
#' Fetch a source and save it as an immutable snapshot
#' @param source A [datasource].
#' @param store Snapshot store directory; default resolved from the project.
#' @param formats Payload formats to write; default `c("rds","csv")`.
#' @param verbose Verbosity.
#' @return The saved snapshot, with its `id` attached, invisibly.
#' @export
take_snapshot <- function(source, store = snapshot_store(), formats = c("rds", "csv"),
                          verbose = 2L) {
  snap <- fetch_snapshot(source, verbose = verbose)
  save_snapshot(snap, store = store, formats = formats, verbose = verbose)
}

#' Save an in-memory snapshot to the store
#' @param x A `bctu_snapshot`.
#' @param store Snapshot store directory.
#' @param formats Payload formats.
#' @param verbose Verbosity.
#' @return `x` with `id` attached, invisibly.
#' @export
save_snapshot <- function(x, store = snapshot_store(), formats = c("rds", "csv"),
                          verbose = 2L) {
  if (!inherits(x, "bctu_snapshot")) cli::cli_abort("{.arg x} must be a {.cls bctu_snapshot}.")
  meta <- attr(x, "bctu_meta")
  # collision-safe id: bump the whole second until the directory is free
  now <- utc_now(); id <- snapshot_id(now); dir <- file.path(store, id)
  while (dir.exists(dir)) { now <- now + 1L; id <- snapshot_id(now); dir <- file.path(store, id) }
  dir.create(file.path(dir, "tables"), recursive = TRUE, showWarnings = FALSE)

  tbl_meta <- list()
  for (nm in names(x)) {
    tdir <- file.path(dir, "tables", nm)
    dir.create(tdir, showWarnings = FALSE)
    files <- list()
    if ("rds" %in% formats) {
      p <- file.path(tdir, paste0(nm, ".rds")); saveRDS(x[[nm]], p)
      files$rds <- file_entry(p, dir)
    }
    if ("csv" %in% formats) {
      p <- file.path(tdir, paste0(nm, ".csv")); utils::write.csv(x[[nm]], p, row.names = FALSE, na = "")
      files$csv <- file_entry(p, dir)
    }
    tbl_meta[[nm]] <- list(n_rows = nrow(x[[nm]]), n_cols = ncol(x[[nm]]), files = files)
  }

  manifest <- list(
    schema = snapshot_schema, id = id, name = meta$name,
    created_utc = iso8601(now), fetched_utc = meta$fetched_utc,
    source = meta$source, checkpoint = meta$checkpoint, tables = tbl_meta
  )
  yaml::write_yaml(manifest, file.path(dir, manifest_filename))

  ledger_append(store, list(
    event = "extract", id = id, name = meta$name, at = iso8601(now),
    source_type = meta$source$type,
    tables = lapply(tbl_meta, function(t) list(n_rows = t$n_rows,
                                               sha256 = t$files[[1]]$sha256)),
    user = meta$checkpoint$user
  ))

  attr(x, "id") <- id; attr(x, "dir") <- dir
  if (verbose >= 1L) cli::cli_alert_success("snapshot {.val {id}} saved -> {.file {dir}}")
  invisible(x)
}

#' @keywords internal
file_entry <- function(path, base) {
  list(path = relative_to(path, base),
       size_bytes = as.integer(file.info(path)$size),
       sha256 = sha256_file(path))
}
#' @keywords internal
relative_to <- function(path, base) sub(paste0("^", regex_escape(normalizePath(base, winslash = "/")), "/?"),
                                 "", normalizePath(path, winslash = "/"))
#' @keywords internal
regex_escape <- function(x) gsub("([.\\\\+*?\\[^\\]$(){}=!<>|:#-])", "\\\\\\1", x, perl = TRUE)

# --- resolve / list ---------------------------------------------------------
#' List snapshot ids in a store (newest last)
#' @param store Snapshot store directory.
#' @export
list_snapshots <- function(store = snapshot_store(verbose = 0L)) {
  d <- list.dirs(store, recursive = FALSE, full.names = FALSE)
  sort(d[grepl(snapshot_id_regex, d)])
}

#' @keywords internal
resolve_snapshot_which <- function(which, store) {
  ids <- list_snapshots(store)
  if (length(ids) == 0L) cli::cli_abort("No snapshots in {.file {store}}.")
  pick <- if (identical(which, "latest")) utils::tail(ids, 1L)
    else if (identical(which, "penultimate")) { if (length(ids) < 2L) cli::cli_abort("No penultimate snapshot.") ; utils::tail(ids, 2L)[1L] }
    else if (is.numeric(which)) utils::tail(ids, which)[1L]
    else if (is_string(which) && grepl(snapshot_id_regex, which)) { if (!which %in% ids) cli::cli_abort("Snapshot {.val {which}} not found."); which }
    else cli::cli_abort("Unrecognised {.arg which}: {.val {which}}.")
  pick
}

# --- load -------------------------------------------------------------------
#' Load a snapshot from the store
#' @param which `"latest"`, `"penultimate"`, an id, or an integer (1 = newest).
#' @param store Snapshot store directory.
#' @param table Optionally, return only this table.
#' @param verbose Verbosity.
#' @return A `bctu_snapshot` (or a single data frame if `table` is given).
#' @export
load_snapshot <- function(which = "latest", store = snapshot_store(verbose = 0L),
                          table = NULL, verbose = 2L) {
  id <- resolve_snapshot_which(which, store)
  dir <- file.path(store, id)
  man <- yaml::read_yaml(file.path(dir, manifest_filename))
  read_one <- function(nm) {
    f <- man$tables[[nm]]$files
    if (!is.null(f$rds)) readRDS(file.path(dir, f$rds$path))
    else { if (verbose >= 1L) cli::cli_warn("Table {.val {nm}}: no rds, reading csv (types/labels may differ).")
           utils::read.csv(file.path(dir, f$csv$path), stringsAsFactors = FALSE) }
  }
  if (!is.null(table)) return(read_one(table))
  tables <- stats::setNames(lapply(names(man$tables), read_one), names(man$tables))
  x <- structure(tables, class = c("bctu_snapshot", "list"),
                 bctu_meta = list(name = man$name, source = man$source,
                                  fetched_utc = man$fetched_utc, checkpoint = man$checkpoint),
                 id = id, dir = dir)
  if (verbose >= 1L) cli::cli_alert_info("loaded snapshot {.val {id}} ({length(tables)} table{?s})")
  x
}

# --- verify -----------------------------------------------------------------
#' Verify a snapshot's on-disk integrity against its manifest (SHA-256)
#' @param which Snapshot selector.
#' @param store Snapshot store directory.
#' @return A list with `ok` (logical) and a per-file `details` data frame.
#' @export
verify_snapshot <- function(which = "latest", store = snapshot_store(verbose = 0L)) {
  id <- resolve_snapshot_which(which, store); dir <- file.path(store, id)
  man <- yaml::read_yaml(file.path(dir, manifest_filename))
  rows <- list()
  for (nm in names(man$tables)) for (fmt in names(man$tables[[nm]]$files)) {
    e <- man$tables[[nm]]$files[[fmt]]; p <- file.path(dir, e$path)
    exists <- file.exists(p)
    sha_ok <- exists && identical(sha256_file(p), e$sha256)
    rows[[length(rows) + 1L]] <- data.frame(table = nm, format = fmt,
      exists = exists, sha256_ok = sha_ok, stringsAsFactors = FALSE)
  }
  details <- do.call(rbind, rows)
  list(ok = all(details$exists & details$sha256_ok), id = id, details = details)
}

# --- delete (first-class, safe, audited) -----------------------------------
#' Delete or retire a snapshot, recording the deletion in the audit ledger
#' @param which Snapshot selector.
#' @param reason Free-text reason (required; goes in the audit ledger).
#' @param store Snapshot store directory.
#' @param mode `"destroy"` (remove) or `"retire"` (move to `_deleted/`).
#' @param verbose Verbosity.
#' @return The deleted id, invisibly.
#' @export
delete_snapshot <- function(which, reason, store = snapshot_store(verbose = 0L),
                            mode = c("destroy", "retire"), verbose = 2L) {
  mode <- match.arg(mode)
  if (!is_string(reason) || !nzchar(reason))
    cli::cli_abort("{.arg reason} is required (recorded in the audit trail).")
  id <- resolve_snapshot_which(which, store); dir <- file.path(store, id)
  Sys.chmod(list.files(dir, recursive = TRUE, full.names = TRUE, include.dirs = TRUE), "0777")
  Sys.chmod(dir, "0777")
  if (mode == "retire") {
    dest <- file.path(store, "_deleted"); dir.create(dest, showWarnings = FALSE)
    file.rename(dir, file.path(dest, id))
  } else {
    unlink(dir, recursive = TRUE, force = TRUE)
  }
  ledger_append(store, list(event = "delete", id = id, at = iso8601(),
                             mode = mode, reason = reason,
                             user = unname(Sys.info()[["user"]])))
  if (verbose >= 1L) cli::cli_alert_success("snapshot {.val {id}} {mode}d (recorded in ledger).")
  invisible(id)
}

# --- append-only audit ledger (human-readable YAML documents) --------------
#' @keywords internal
ledger_append <- function(store, record) {
  path <- file.path(store, ledger_filename)
  cat("---\n", yaml::as.yaml(record), sep = "", file = path, append = TRUE)
  invisible(path)
}
#' Read the append-only snapshot audit ledger
#' @param store Snapshot store directory.
#' @return A list of ledger records, oldest first.
#' @export
read_ledger <- function(store = snapshot_store(verbose = 0L)) {
  path <- file.path(store, ledger_filename)
  if (!file.exists(path)) return(list())
  txt <- paste(readLines(path), collapse = "\n")
  docs <- strsplit(txt, "(^|\n)---\\s*\n")[[1]]
  docs <- docs[nzchar(trimws(docs))]
  lapply(docs, yaml::yaml.load)
}
