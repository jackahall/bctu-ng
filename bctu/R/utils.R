#' @keywords internal
`%||%` <- function(a, b) if (is.null(a)) b else a

# ---------------------------------------------------------------------------
# Time policy (single source of truth)
# ---------------------------------------------------------------------------
# Every snapshot instant is recorded in UTC. There is exactly one canonical
# on-disk identifier format and exactly one parser. Nothing else in the package
# formats or parses a snapshot time by hand.
#
#   snapshot id  : "YYYY-MM-DDTHHMMSSZ"  (UTC, colon-free -> filesystem-safe on
#                  Windows, lexically sortable, unambiguous). Used for directory
#                  names and cross-references.
#   manifest time: full extended ISO 8601 "YYYY-MM-DDTHH:MM:SSZ" (human-readable,
#                  stored as text in the YAML manifest alongside the id).

utc_now <- function() as.POSIXct(Sys.time(), tz = "UTC")

snapshot_id_regex <- "^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{6}Z(-[0-9]{2})?$"

#' Canonical snapshot identifier from a time
#' @param time A `POSIXct` (or anything coercible); interpreted/rendered in UTC.
#' @return A length-1 character id `"YYYY-MM-DDTHHMMSSZ"`.
#' @export
snapshot_id <- function(time = utc_now()) {
  time <- as.POSIXct(time, tz = "UTC")
  if (length(time) != 1L || is.na(time))
    cli::cli_abort("{.arg time} must be a single non-missing time.")
  format(time, "%Y-%m-%dT%H%M%SZ", tz = "UTC")
}

#' Parse a canonical snapshot id back to a UTC `POSIXct` (strict)
#' @param id A snapshot id string.
#' @return A length-1 UTC `POSIXct`.
#' @export
parse_snapshot_id <- function(id) {
  if (!is.character(id) || length(id) != 1L || !grepl(snapshot_id_regex, id))
    cli::cli_abort(c("Invalid snapshot id: {.val {id}}",
                     "i" = "Expected {.code YYYY-MM-DDTHHMMSSZ} (UTC), optionally with a {.code -NN} suffix."))
  base_id <- sub("-[0-9]{2}$", "", id)
  t <- as.POSIXct(strptime(base_id, "%Y-%m-%dT%H%M%SZ", tz = "UTC"), tz = "UTC")
  if (is.na(t)) cli::cli_abort("Snapshot id {.val {id}} is not a real UTC time.")
  t
}

#' Human-readable extended ISO 8601 timestamp (for manifests)
#' @param time A time; rendered in `tz`.
#' @param tz Timezone; default UTC.
#' @export
iso8601 <- function(time = utc_now(), tz = "UTC") {
  format(as.POSIXct(time, tz = tz), "%Y-%m-%dT%H:%M:%SZ", tz = tz)
}

#' Data-cut calendar date of a snapshot, in an explicit timezone
#'
#' The data-cut date is a local calendar date, so the timezone is explicit and
#' defaults to UK time (never silently UTC, which shifts late-evening snapshots
#' to the next day).
#' @param x A snapshot id, a `POSIXct`, or an object carrying a snapshot id.
#' @param tz Timezone for the calendar date; default `"Europe/London"`.
#' @return A length-1 `Date`.
#' @export
snapshot_date <- function(x, tz = "Europe/London") {
  t <- if (is.character(x)) parse_snapshot_id(x) else as.POSIXct(x, tz = "UTC")
  as.Date(format(t, "%Y-%m-%d", tz = tz))
}

# ---------------------------------------------------------------------------
# Integrity
# ---------------------------------------------------------------------------
#' SHA-256 of a file's bytes
#' @keywords internal
sha256_file <- function(path) {
  as.character(digest::digest(file = path, algo = "sha256"))
}

#' SHA-256 of an R object's serialised value
#' @keywords internal
sha256_object <- function(x) {
  as.character(digest::digest(x, algo = "sha256", serialize = TRUE))
}

# ---------------------------------------------------------------------------
# Small helpers
# ---------------------------------------------------------------------------
#' @keywords internal
is_string <- function(x) is.character(x) && length(x) == 1L && !is.na(x)
