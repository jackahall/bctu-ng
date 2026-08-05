# ---------------------------------------------------------------------------
# Project configuration
# ---------------------------------------------------------------------------
# All location is explicit and anchored to a committed project marker at the
# trial root: `bctu-project.yml`. There is NO global option, NO walk-from-cwd
# dotenv, and NO silent "Data/Snapshots under getwd()" fallback. If the marker
# cannot be found, resolution errors loudly. Every resolved path is announced,
# so a snapshot can never be written somewhere unnoticed.

project_marker_name <- "bctu-project.yml"

#' Initialise a bctu project
#'
#' Writes a `bctu-project.yml` marker at `dir` declaring the project name and,
#' relative to the marker, where snapshots live. This file is the single anchor
#' for all later path resolution and should be committed to the trial repo.
#' @param name Project/trial name (e.g. `"OCeAN"`).
#' @param snapshot_store Snapshot directory, relative to the marker. Default
#'   `"Data/Snapshots"`.
#' @param dir Directory to write the marker into; default the current directory.
#' @param overwrite Overwrite an existing marker? Default `FALSE`.
#' @return The absolute path of the written marker, invisibly.
#' @export
bctu_init_project <- function(name,
                              snapshot_store = "Data/Snapshots",
                              dir = getwd(),
                              overwrite = FALSE) {
  if (!is_string(name)) cli::cli_abort("{.arg name} must be a single string.")
  dir <- normalizePath(dir, mustWork = TRUE)
  file <- file.path(dir, project_marker_name)
  if (file.exists(file) && !overwrite)
    cli::cli_abort(c("A bctu project already exists here: {.file {file}}",
                     "i" = "Pass {.code overwrite = TRUE} to replace it."))
  yaml::write_yaml(
    list(bctu_project = name, snapshot_store = snapshot_store),
    file
  )
  cli::cli_alert_success("Initialised bctu project {.val {name}} at {.file {file}}")
  invisible(file)
}

#' Locate the nearest project marker by walking up from `start`
#' @keywords internal
find_project_marker <- function(start = getwd()) {
  dir <- normalizePath(start, mustWork = TRUE)
  repeat {
    cand <- file.path(dir, project_marker_name)
    if (file.exists(cand)) return(cand)
    parent <- dirname(dir)
    if (identical(parent, dir)) return(NA_character_)   # reached filesystem root
    dir <- parent
  }
}

#' Read the bctu project configuration
#'
#' @param start Directory to search upward from; default the current directory.
#' @return A list with `name`, `file` (absolute marker path), `root` (marker's
#'   directory), and the raw declared fields.
#' @export
bctu_project <- function(start = getwd()) {
  file <- find_project_marker(start)
  if (is.na(file))
    cli::cli_abort(c(
      "No {.file {project_marker_name}} found at or above {.file {normalizePath(start)}}.",
      "i" = "Run {.code bctu_init_project(<name>)} at the trial root first.",
      "x" = "bctu refuses to guess a location — nothing was read or written."
    ))
  cfg <- yaml::read_yaml(file)
  if (is.null(cfg$bctu_project))
    cli::cli_abort("{.file {file}} is missing the required {.field bctu_project} field.")
  list(
    name  = cfg$bctu_project,
    file  = file,
    root  = dirname(file),
    snapshot_store = cfg$snapshot_store %||% "Data/Snapshots",
    raw   = cfg
  )
}

#' Resolve the snapshot store directory (absolute), and announce it
#'
#' The store is resolved RELATIVE TO THE PROJECT MARKER, never the working
#' directory. Errors if no project is found (no fallback).
#' @param start Directory to search upward from; default the current directory.
#' @param verbose If `>= 1`, announce the resolved path and its source.
#' @return The absolute snapshot store path (created if it does not exist).
#' @export
snapshot_store <- function(start = getwd(), verbose = 1L) {
  p <- bctu_project(start)
  store <- p$snapshot_store
  store <- if (is_absolute_path(store)) store else file.path(p$root, store)
  store <- normalize_path_lenient(store)
  if (!dir.exists(store)) dir.create(store, recursive = TRUE, showWarnings = FALSE)
  if (verbose >= 1L)
    cli::cli_alert_info("snapshot store: {.file {store}}  (from {.file {p$file}})")
  store
}

#' Print the fully resolved configuration (for operators and auditors)
#' @param start Directory to search upward from; default the current directory.
#' @export
bctu_config <- function(start = getwd()) {
  p <- bctu_project(start)
  cli::cli_h2("bctu project {.val {p$name}}")
  cli::cli_dl(c(
    "project marker" = "{.file {p$file}}",
    "project root"   = "{.file {p$root}}",
    "snapshot store" = "{.file {snapshot_store(start, verbose = 0L)}}"
  ))
  invisible(p)
}

# --- path helpers (no external deps) ---------------------------------------
#' @keywords internal
is_absolute_path <- function(path) {
  grepl("^(/|~|[A-Za-z]:[/\\\\])", path)
}
#' @keywords internal
normalize_path_lenient <- function(path) {
  # normalizePath but tolerate a not-yet-existing leaf
  normalizePath(path, winslash = "/", mustWork = FALSE)
}
