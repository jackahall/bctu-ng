# ---------------------------------------------------------------------------
# Snapshots
# ---------------------------------------------------------------------------
# A snapshot is an immutable, timestamped capture of one or more named tables.
# On disk:
#   <store>/<id>/
#     manifest.yml                                    human-readable + machine-readable (GCP)
#     tables/<table>/<study>_<table>_<id>.rds         typed canonical payload
#     tables/<table>/<study>_<table>_<id>.csv         readable exchange copy
# Payload files are self-identifying: the study name, table name, and snapshot id
# are in the filename, so a file stays recognisable if it is moved out of the
# store. Integrity is SHA-256 recorded in the manifest (the authoritative
# control). The per-snapshot manifest is itself the immutable, timestamped audit
# record of the extraction; a snapshot retired with delete_snapshot() keeps its
# manifest as the deletion record. Nothing is committed to git and nothing is
# made read-only, so ordinary git and delete operations never fight the
# filesystem.

manifest_filename     <- "manifest.yml"
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
take_snapshot <- function(source, store = snapshot_location(), formats = c("rds", "csv"),
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
save_snapshot <- function(x, store = snapshot_location(), formats = c("rds", "csv"),
                          verbose = 2L) {
  if (!inherits(x, "bctu_snapshot")) cli::cli_abort("{.arg x} must be a {.cls bctu_snapshot}.")
  meta <- attr(x, "bctu_meta")
  study <- sanitise_study_name(meta$name)
  # collision-safe id: bump the whole second until the directory is free
  now <- utc_now(); id <- snapshot_id(now); dir <- file.path(store, id)
  while (dir.exists(dir)) { now <- now + 1L; id <- snapshot_id(now); dir <- file.path(store, id) }
  dir.create(file.path(dir, "tables"), recursive = TRUE, showWarnings = FALSE)

  tbl_meta <- list()
  for (nm in names(x)) {
    tdir <- file.path(dir, "tables", nm)
    dir.create(tdir, showWarnings = FALSE)
    # payload files are self-identifying: <study>_<table>_<id>.<ext>
    stem <- paste(study, nm, id, sep = "_")
    files <- list()
    if ("rds" %in% formats) {
      p <- file.path(tdir, paste0(stem, ".rds")); saveRDS(x[[nm]], p)
      files$rds <- file_entry(p, dir)
    }
    if ("csv" %in% formats) {
      p <- file.path(tdir, paste0(stem, ".csv")); utils::write.csv(x[[nm]], p, row.names = FALSE, na = "")
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

#' Turn a study name into a filesystem-safe token for payload filenames
#'
#' Replaces any run of non-alphanumeric characters with a single underscore and
#' trims leading/trailing underscores. Falls back to `"snapshot"` when nothing
#' usable remains.
#' @keywords internal
sanitise_study_name <- function(name) {
  token <- if (is_string(name)) name else "snapshot"
  token <- gsub("[^A-Za-z0-9]+", "_", token)
  token <- gsub("^_+|_+$", "", token)
  if (!nzchar(token)) "snapshot" else token
}

# --- resolve / list ---------------------------------------------------------
#' List snapshot ids in a store (newest last)
#' @param store Snapshot store directory.
#' @export
list_snapshots <- function(store = snapshot_location(verbose = 0L)) {
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
load_snapshot <- function(which = "latest", store = snapshot_location(verbose = 0L),
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
verify_snapshot <- function(which = "latest", store = snapshot_location(verbose = 0L)) {
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

# --- delete (first-class, safe, self-documenting) --------------------------
#' Delete or retire a snapshot
#'
#' The default `mode = "retire"` moves the snapshot directory to
#' `<store>/_deleted/<id>/`, keeping its immutable `manifest.yml` as the
#' self-documenting record of what was removed, and writes a small
#' `deletion-note.yml` alongside it capturing the `reason`. `mode = "destroy"`
#' removes the directory outright.
#' @param which Snapshot selector.
#' @param reason Free-text reason (required; recorded in `deletion-note.yml`
#'   for a retired snapshot).
#' @param store Snapshot store directory.
#' @param mode `"retire"` (move to `_deleted/`, keeping the manifest) or
#'   `"destroy"` (remove).
#' @param verbose Verbosity.
#' @return The deleted id, invisibly.
#' @export
delete_snapshot <- function(which, reason, store = snapshot_location(verbose = 0L),
                            mode = c("retire", "destroy"), verbose = 2L) {
  mode <- match.arg(mode)
  if (!is_string(reason) || !nzchar(reason))
    cli::cli_abort("{.arg reason} is required (recorded with the deleted snapshot).")
  id <- resolve_snapshot_which(which, store); dir <- file.path(store, id)
  Sys.chmod(list.files(dir, recursive = TRUE, full.names = TRUE, include.dirs = TRUE), "0777")
  Sys.chmod(dir, "0777")
  if (mode == "retire") {
    dest <- file.path(store, "_deleted"); dir.create(dest, showWarnings = FALSE)
    retired <- file.path(dest, id)
    file.rename(dir, retired)
    yaml::write_yaml(
      list(id = id, deleted_utc = iso8601(), mode = mode, reason = reason,
           user = unname(Sys.info()[["user"]])),
      file.path(retired, "deletion-note.yml")
    )
  } else {
    unlink(dir, recursive = TRUE, force = TRUE)
  }
  if (verbose >= 1L) cli::cli_alert_success("snapshot {.val {id}} {mode}d.")
  invisible(id)
}
