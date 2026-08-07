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
# store. Integrity is SHA-256 recorded in the manifest. The audit trail is git:
# every take and every delete commits the snapshot metadata (manifest only, never
# payload) to the trial repository, so the sequence of extractions and deletions
# is recorded in git history (author, time, and diff) and a destroyed snapshot
# still leaves a commit. Payloads are never made read-only, so ordinary git and
# delete operations never fight the filesystem.

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

# --- git provenance: the audit trail (metadata only; never fails a snapshot) ---
#' @keywords internal
git_available <- function() nzchar(Sys.which("git"))

#' @keywords internal
git_run <- function(root, args) {
  # shQuote every argument: a commit message (or path) can contain spaces, which
  # system2 would otherwise split into separate arguments.
  res <- suppressWarnings(tryCatch(
    system2("git", shQuote(c("-C", root, args)), stdout = TRUE, stderr = TRUE),
    error = function(e) structure("", status = 1L)))
  st <- attr(res, "status")
  list(ok = is.null(st) || identical(as.integer(st), 0L), out = res)
}

#' @keywords internal
git_root <- function(path) {
  r <- git_run(path, c("rev-parse", "--show-toplevel"))
  if (r$ok && length(r$out)) r$out[1L] else NA_character_
}

#' @keywords internal
git_head <- function(root) {
  r <- git_run(root, c("rev-parse", "HEAD"))
  if (r$ok && length(r$out)) r$out[1L] else NA_character_
}

#' @keywords internal
git_dirty <- function(root) {
  r <- git_run(root, c("status", "--porcelain"))
  isTRUE(r$ok) && any(nzchar(r$out))
}

#' Commit a snapshot's metadata (manifest only, never payload) to the trial repo
#'
#' Git history is the audit trail: every saved snapshot commits its manifest. An
#' annotated `snap/<tag>` tag is added only when the extraction carries an explicit
#' `tag` (routine snapshots are not tagged, to avoid a tag per snapshot).
#' @keywords internal
commit_snapshot_metadata <- function(root, meta_files, id, tag = NULL, verbose = 1L) {
  rels <- vapply(meta_files, function(f) relative_to(f, root), character(1))
  if (!git_run(root, c("add", "--", rels))$ok) {
    if (verbose >= 1L)
      cli::cli_warn(c("Could not stage snapshot metadata for commit (is it git-ignored?).",
                      "i" = "The snapshot itself was saved; only the git record was skipped."))
    return(invisible(FALSE))
  }
  msg <- sprintf("snapshot %s%s", id, if (!is.null(tag)) paste0(" [", tag, "]") else "")
  ci  <- git_run(root, c("commit", "-m", msg, "--", rels))
  if (!ci$ok) {
    if (verbose >= 1L)
      cli::cli_warn(c("git commit of snapshot metadata failed.",
                      "i" = paste(utils::tail(ci$out, 1L), collapse = " ")))
    return(invisible(FALSE))
  }
  if (!is.null(tag)) {
    tagname <- paste0("snap/", tag)
    tg <- git_run(root, c("tag", "-a", tagname, "-m", msg))
    if (verbose >= 1L && !tg$ok)
      cli::cli_warn("Committed metadata, but tag {.val {tagname}} could not be created (it may already exist).")
  }
  if (verbose >= 1L)
    cli::cli_alert_success("committed snapshot metadata for {.val {id}}{if (!is.null(tag)) paste0(' (tag ', tag, ')') else ''}.")
  invisible(TRUE)
}

#' Commit a snapshot's deletion (metadata only) to the trial repo
#'
#' The deletion commit is the audit record that a snapshot was retired or
#' destroyed: it stages the metadata change (the manifest's removal, plus the
#' retained `deletion-note.yml` when retiring) and commits it with the id, mode
#' and reason in the message. Never fails the deletion: a failed commit warns.
#' @keywords internal
commit_snapshot_deletion <- function(root, rels, id, reason, mode, verbose = 1L) {
  git_run(root, c("add", "-A", "--", rels))
  msg <- sprintf("delete snapshot %s (%s): %s", id, mode, reason)
  ci  <- git_run(root, c("commit", "-m", msg, "--", rels))
  if (!ci$ok) {
    if (verbose >= 1L)
      cli::cli_warn(c("git commit of the snapshot deletion failed (deletion still done).",
                      "i" = paste(utils::tail(ci$out, 1L), collapse = " ")))
    return(invisible(FALSE))
  }
  if (verbose >= 1L) cli::cli_alert_success("committed deletion of {.val {id}}.")
  invisible(TRUE)
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
#' @param store Snapshot store directory; default resolved from the project marker.
#' @param formats Payload formats to write; default `c("rds","csv")`. Other
#'   values (`"dta"`, `"sas7bdat"`, `"xpt"`, `"sas"`) are written by
#'   [save_snapshot()].
#' @param tag Optional short tag for this extraction (e.g. `"DMC-2026-08"`),
#'   recorded on the snapshot, its tables and the manifest, and used to annotate
#'   the git tag `snap/<tag>`.
#' @param labels Optional named list of extra free-text metadata to record.
#' @param git Git provenance mode; see [save_snapshot()].
#' @param verbose Verbosity.
#' @return The saved snapshot, with its `id` attached, invisibly.
#' @examples
#' \dontrun{
#' ds <- datasource_example()
#' take_snapshot(ds)
#' }
#' @export
take_snapshot <- function(source, store = snapshot_store(create = TRUE),
                          formats = c("rds", "csv"), tag = NULL, labels = NULL,
                          git = NULL, verbose = 2L) {
  snap <- fetch_snapshot(source, verbose = verbose)
  save_snapshot(snap, store = store, formats = formats, tag = tag,
                labels = labels, git = git, verbose = verbose)
}

#' Save an in-memory snapshot to the store
#' @param x A `bctu_snapshot`.
#' @param store Snapshot store directory.
#' @param formats Payload formats: any of `"rds"`, `"csv"`, `"dta"`,
#'   `"sas7bdat"`, `"xpt"`, `"sas"` (a SAS import script).
#' @param tag Optional short extraction tag (see [take_snapshot()]).
#' @param labels Optional named list of extra free-text metadata.
#' @param git Git provenance (git history is the audit trail). `"commit"` (the
#'   default) commits the metadata (manifest only, never the payload), adding an
#'   annotated `snap/<tag>` tag when a `tag` is given; `"record"` only writes the
#'   repo's HEAD SHA into the manifest without committing; `"off"` skips git
#'   entirely. Never fails a snapshot: if git is unavailable the commit is skipped
#'   (with a warning when the store is not in a repository).
#' @param verbose Verbosity.
#' @return `x` with `id` attached, invisibly.
#' @examples
#' \dontrun{
#' ds <- datasource_example()
#' snap <- fetch_snapshot(ds)
#' save_snapshot(snap)
#' }
#' @export
save_snapshot <- function(x, store = snapshot_store(create = TRUE),
                          formats = c("rds", "csv"), tag = NULL, labels = NULL,
                          git = NULL, verbose = 2L) {
  if (!inherits(x, "bctu_snapshot")) cli::cli_abort("{.arg x} must be a {.cls bctu_snapshot}.")
  if (!is.null(tag) && (!is_string(tag) || !nzchar(trimws(tag))))
    cli::cli_abort("{.arg tag} must be a single non-empty string, or NULL.")
  validate_table_names(names(x))
  meta <- attr(x, "bctu_meta")
  meta$tag <- tag %||% meta$tag
  meta$labels <- labels %||% meta$labels
  study <- sanitise_study_name(meta$name)

  now <- utc_now(); id <- snapshot_id(now); dir <- file.path(store, id)
  # collision-safe id: append -NN (never distort the recorded second)
  suffix <- 0L
  while (!dir.create(file.path(dir, "tables"), recursive = TRUE, showWarnings = FALSE)) {
    suffix <- suffix + 1L
    if (suffix > 99L) cli::cli_abort("Too many snapshots in the same second at {.file {store}}.")
    id <- sprintf("%s-%02d", snapshot_id(now), suffix); dir <- file.path(store, id)
  }

  tbl_meta <- list()
  for (nm in names(x)) {
    tdir <- file.path(dir, "tables", nm)
    dir.create(tdir, showWarnings = FALSE)
    stem <- paste(study, nm, id, sep = "_")   # self-identifying: <study>_<table>_<id>
    files <- write_snapshot_payload(x[[nm]], tdir, stem, formats, dir)
    tbl_meta[[nm]] <- list(n_rows = nrow(x[[nm]]), n_cols = ncol(x[[nm]]), files = files)
  }

  manifest <- drop_null(list(
    schema = snapshot_schema, id = id, name = meta$name,
    tag = meta$tag, labels = meta$labels,
    created_utc = iso8601(now), fetched_utc = meta$fetched_utc,
    source = meta$source, checkpoint = meta$checkpoint, tables = tbl_meta
  ))
  tmp <- file.path(dir, paste0(manifest_filename, ".tmp"))
  yaml::write_yaml(manifest, tmp)
  file.rename(tmp, file.path(dir, manifest_filename))

  # Git is the audit trail: record the code HEAD in the manifest and (commit mode,
  # the default) commit the METADATA ONLY (manifest, never payload), tagging only
  # when the extraction carries an explicit tag.
  git_mode <- match.arg(git %||% "commit", c("commit", "record", "off"))
  if (git_mode != "off" && git_available()) {
    root <- git_root(store)
    if (!is.na(root)) {
      manifest$git_head  <- git_head(root)
      manifest$git_dirty <- git_dirty(root)
      tmp2 <- file.path(dir, paste0(manifest_filename, ".tmp"))
      yaml::write_yaml(drop_null(manifest), tmp2)
      file.rename(tmp2, file.path(dir, manifest_filename))
      if (git_mode == "commit")
        commit_snapshot_metadata(root, file.path(dir, manifest_filename),
                                 id, meta$tag, verbose)
    } else if (git_mode == "commit" && verbose >= 1L) {
      cli::cli_warn(c("The snapshot store is not inside a git repository.",
                      "i" = "No git audit commit was made. Put the store under git for an audit trail, or pass {.code git = \"off\"} to silence this."))
    }
  }

  attr(x, "bctu_meta") <- meta
  attr(x, "id") <- id; attr(x, "dir") <- dir; attr(x, "bctu_tag") <- meta$tag
  for (nm in names(x)) attr(x[[nm]], "bctu_tag") <- meta$tag
  if (verbose >= 1L) cli::cli_alert_success("snapshot {.val {id}} saved -> {.file {dir}}")
  invisible(x)
}

#' @keywords internal
validate_table_names <- function(nms) {
  bad <- nms[!nzchar(nms) | grepl("(^[.]{1,2}$)|[/\\\\]|[.][.]", nms) |
               grepl("[[:cntrl:]]", nms)]
  if (length(bad))
    cli::cli_abort(c("Unsafe table name{?s}: {.val {bad}}.",
                     "i" = "Table names become directory names; they may not contain {.code /}, {.code \\\\}, {.code ..} or control characters."))
  invisible(nms)
}

#' @keywords internal
write_snapshot_payload <- function(tbl, tdir, stem, formats, base) {
  files <- list()
  if ("rds" %in% formats) {
    p <- file.path(tdir, paste0(stem, ".rds")); saveRDS(tbl, p)
    files$rds <- file_entry(p, base)
  }
  if ("csv" %in% formats) {
    p <- file.path(tdir, paste0(stem, ".csv"))
    utils::write.csv(restore_codes_frame(tbl), p, row.names = FALSE, na = "")
    files$csv <- file_entry(p, base)
  }
  if ("dta" %in% formats) {
    p <- file.path(tdir, paste0(stem, ".dta"))
    if (haven_write("dta", tbl, p)) files$dta <- file_entry(p, base)
  }
  if ("sas7bdat" %in% formats) {
    p <- file.path(tdir, paste0(stem, ".sas7bdat"))
    if (haven_write("sas7bdat", retag_upper(tbl), p)) files$sas7bdat <- file_entry(p, base)
  }
  if ("xpt" %in% formats) {
    p <- file.path(tdir, paste0(stem, ".xpt"))
    if (haven_write("xpt", retag_upper(tbl), p)) files$xpt <- file_entry(p, base)
  }
  if ("sas" %in% formats) {
    # Reliable path: a SAS script that imports the CSV and applies special missings.
    if (!"csv" %in% formats) {
      pc <- file.path(tdir, paste0(stem, ".csv"))
      utils::write.csv(restore_codes_frame(tbl), pc, row.names = FALSE, na = "")
      files$csv <- file_entry(pc, base)
    }
    p <- file.path(tdir, paste0(stem, "_import.sas"))
    writeLines(sas_import_script(tbl, paste0(stem, ".csv"), make_sas_name(stem)), p)
    files$sas <- file_entry(p, base)
  }
  files
}

#' Best-effort haven writer (haven's SAS writers are unstable; never abort a save)
#' @keywords internal
haven_write <- function(kind, tbl, path) {
  if (!requireNamespace("haven", quietly = TRUE)) {
    cli::cli_warn("Package {.pkg haven} not installed; skipping the {.val {kind}} export.")
    return(FALSE)
  }
  writer <- switch(kind, dta = haven::write_dta, sas7bdat = haven::write_sas,
                   xpt = haven::write_xpt)
  ok <- tryCatch({ suppressWarnings(writer(tbl, path)); TRUE },
                 error = function(e) {
                   cli::cli_warn(c("Could not write the {.val {kind}} export ({conditionMessage(e)}).",
                                   "i" = "The rds/csv copies and any SAS import script are unaffected."))
                   FALSE
                 })
  ok
}

#' @keywords internal
make_sas_name <- function(stem) {
  nm <- gsub("[^A-Za-z0-9_]", "_", stem)
  if (nchar(nm) > 32L) nm <- substr(nm, 1L, 32L)
  nm
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
#' @examples
#' store <- withr::local_tempdir()
#' list_snapshots(store)
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
#' @examples
#' \dontrun{
#' load_snapshot("latest")
#' }
#' @export
load_snapshot <- function(which = "latest", store = snapshot_store(verbose = 0L),
                          table = NULL, verbose = 2L) {
  id <- resolve_snapshot_which(which, store)
  dir <- file.path(store, id)
  man <- yaml::read_yaml(file.path(dir, manifest_filename))
  read_one <- function(nm) {
    f <- man$tables[[nm]]$files
    if (!is.null(f$rds)) return(readRDS(file.path(dir, f$rds$path)))
    if (!is.null(f$csv)) {
      cli::cli_warn("Table {.val {nm}}: no rds payload, reading the csv copy (types and labels may differ).")
      return(utils::read.csv(file.path(dir, f$csv$path), stringsAsFactors = FALSE))
    }
    cli::cli_abort(c("Table {.val {nm}} has no readable payload (no rds or csv) in snapshot {.val {id}}.",
                     "i" = "The snapshot directory may be incomplete or corrupted."))
  }
  if (!is.null(table)) return(read_one(table))
  tables <- stats::setNames(lapply(names(man$tables), read_one), names(man$tables))
  x <- structure(tables, class = c("bctu_snapshot", "list"),
                 bctu_meta = drop_null(list(name = man$name, source = man$source,
                                  tag = man$tag, labels = man$labels,
                                  fetched_utc = man$fetched_utc, checkpoint = man$checkpoint)),
                 id = id, dir = dir, bctu_tag = man$tag)
  if (verbose >= 1L) cli::cli_alert_info("loaded snapshot {.val {id}} ({length(tables)} table{?s})")
  x
}

# --- verify -----------------------------------------------------------------
#' Verify a snapshot's on-disk integrity against its manifest (SHA-256)
#' @param which Snapshot selector.
#' @param store Snapshot store directory.
#' @return A list with `ok` (logical) and a per-file `details` data frame.
#' @examples
#' \dontrun{
#' verify_snapshot("latest")
#' }
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
  if (!length(rows))
    return(list(ok = FALSE, id = id, details = data.frame(),
                message = "Manifest lists no payload files to verify."))
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
#' @param reason Free-text reason (required; recorded in the git deletion commit
#'   and, for a retired snapshot, in `deletion-note.yml`).
#' @param store Snapshot store directory.
#' @param mode `"retire"` (move to `_deleted/`, keeping the manifest) or
#'   `"destroy"` (remove).
#' @param git Git provenance for the deletion: `"commit"` (the default) commits
#'   the metadata change (the manifest's removal, plus the retained
#'   `deletion-note.yml`) as the audit record; `"off"` skips git. Never fails the
#'   deletion.
#' @param verbose Verbosity.
#' @return The deleted id, invisibly.
#' @examples
#' \dontrun{
#' delete_snapshot("latest", reason = "duplicate extraction")
#' }
#' @export
delete_snapshot <- function(which, reason = NULL, store = snapshot_store(verbose = 0L),
                            mode = c("retire", "destroy"),
                            git = c("commit", "off"), verbose = 2L) {
  mode <- match.arg(mode)
  git  <- match.arg(git)
  if (!is_string(reason) || !nzchar(trimws(reason)))
    cli::cli_abort(c("{.arg reason} is required.",
                     "i" = "It is recorded in the git deletion commit (the audit trail).",
                     "x" = "Nothing was deleted."))
  id <- resolve_snapshot_which(which, store); dir <- file.path(store, id)
  old_manifest <- file.path(dir, manifest_filename)
  # Resolve the manifest's path relative to the repo BEFORE removing it, so the
  # deletion commit can stage it (relative_to needs the file to still exist).
  root <- if (git == "commit" && git_available()) git_root(store) else NA_character_
  rels <- if (!is.na(root) && file.exists(old_manifest)) relative_to(old_manifest, root) else character(0)

  Sys.chmod(list.files(dir, recursive = TRUE, full.names = TRUE, include.dirs = TRUE), "0777")
  Sys.chmod(dir, "0777")
  if (mode == "retire") {
    dest <- file.path(store, "_deleted"); dir.create(dest, showWarnings = FALSE)
    retired <- file.path(dest, id)
    file.rename(dir, retired)
    note <- file.path(retired, "deletion-note.yml")
    yaml::write_yaml(
      list(id = id, deleted_utc = iso8601(), mode = mode, reason = reason,
           user = unname(Sys.info()[["user"]])),
      note
    )
    if (!is.na(root))
      rels <- c(rels, relative_to(file.path(retired, manifest_filename), root),
                relative_to(note, root))
  } else {
    unlink(dir, recursive = TRUE, force = TRUE)
  }
  # Git is the audit trail: commit the metadata change so the deletion is recorded
  # even for a destroy (which removes the manifest). Never fails the deletion.
  if (git == "commit" && git_available()) {
    if (!is.na(root) && length(rels))
      commit_snapshot_deletion(root, rels, id, reason, mode, verbose)
    else if (is.na(root) && verbose >= 1L)
      cli::cli_warn(c("The snapshot store is not inside a git repository.",
                      "i" = "No git audit commit was made for this deletion."))
  }
  if (verbose >= 1L) cli::cli_alert_success("snapshot {.val {id}} {mode}d.")
  invisible(id)
}
