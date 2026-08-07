# ---------------------------------------------------------------------------
# Data sources
# ---------------------------------------------------------------------------
# A datasource is created by composition: one base class plus a `fetch` closure.
# Adding a source (REDCap, SQL, a simulated example) is a preset over
# `new_datasource()` -- no bespoke S3 method, no re-implemented take_snapshot.
# `fetch` returns a NAMED LIST of data frames (one per table); single-table
# sources return a one-element list. Snapshots are therefore multi-table by
# default and REDCap is just the one-table case.

#' Create a datasource
#' @param type Short source type, e.g. `"redcap"`, `"sql"`, `"example"`.
#' @param fetch `function(creds, config, verbose, ...)` returning a named list
#'   of data frames (the tables).
#' @param creds A [credential_spec()] or `NULL` if no credential is needed.
#' @param config Named list of connection/query configuration (no secrets).
#' @param test Optional `function(creds, config)` for a connectivity check.
#' @param label Human label for printing/audit; default `type`.
#' @return A `datasource` object.
#' @export
new_datasource <- function(type, fetch, creds = NULL, config = list(),
                           test = NULL, label = type) {
  if (!is_string(type)) cli::cli_abort("{.arg type} must be a single string.")
  if (!is.function(fetch)) cli::cli_abort("{.arg fetch} must be a function.")
  if (!is.null(creds) && !inherits(creds, "credential_spec"))
    cli::cli_abort("{.arg creds} must be a {.cls credential_spec} or NULL.")
  structure(
    list(type = type, fetch = fetch, creds = creds, config = config,
         test = test, label = label),
    class = c(paste0("datasource_", type), "datasource")
  )
}

#' @export
print.datasource <- function(x, ...) {
  cli::cli_rule("datasource {.val {x$type}} ({x$label})")
  cfg <- x$config[!vapply(x$config, is.function, logical(1))]
  if (length(cfg))
    cli::cli_dl(stats::setNames(lapply(cfg, function(v) format(v)[1]), names(cfg)))
  cli::cli_text("credential: {if (is.null(x$creds)) 'none' else x$creds$id}")
  invisible(x)
}

# ---------------------------------------------------------------------------
# Credentials -- one spec, one resolver, shared by fetch and has_credential.
# ---------------------------------------------------------------------------
#' Declare where a credential lives (never the secret itself)
#' @param id Logical credential name, e.g. `"ocean"`.
#' @param service Keyring service; default `"bctu_api_token"`.
#' @param env Environment variable name; default derived
#'   `BCTU_API_TOKEN_<ID>`.
#' @param expect_nchar Optional exact expected length (e.g. REDCap tokens = 32).
#' @param required Is the credential required? Default `TRUE`.
#' @return A `credential_spec`.
#' @examples
#' credential_spec("ocean")
#' credential_spec("ocean", expect_nchar = 32L)
#' @export
credential_spec <- function(id, service = "bctu_api_token", env = NULL,
                            expect_nchar = NULL, required = TRUE) {
  if (!is_string(id)) cli::cli_abort("{.arg id} must be a single string.")
  env <- env %||% paste0("BCTU_API_TOKEN_", toupper(gsub("[^A-Za-z0-9]+", "_", id)))
  structure(list(id = id, service = service, env = env,
                 expect_nchar = expect_nchar, required = required),
            class = "credential_spec")
}

#' Resolve a credential: keyring, then env var; one documented precedence
#'
#' Precedence is keyring then environment variable. If both are set and differ,
#' warns (a rotated env-var token silently shadowed by a stale keyring value is
#' a data-integrity hazard). Returns the secret string, or `NULL`/errors per
#' `required`.
#' @param spec A [credential_spec()] or `NULL`.
#' @param verbose Verbosity.
#' @return The resolved secret (character), or `NULL`.
#' @export
resolve_credentials <- function(spec, verbose = 1L) {
  if (is.null(spec)) return(NULL)
  kr <- tryCatch(
    if (requireNamespace("keyring", quietly = TRUE))
      keyring::key_get(spec$service, spec$id) else "",
    error = function(e) ""
  )
  ev <- Sys.getenv(spec$env, unset = "")
  if (nzchar(kr) && nzchar(ev) && !identical(kr, ev))
    cli::cli_warn(c("Credential {.val {spec$id}} differs between keyring and {.envvar {spec$env}}.",
                    "i" = "Using the keyring value; check for a stale token."))
  secret <- if (nzchar(kr)) kr else ev
  if (!nzchar(secret)) {
    if (isTRUE(spec$required))
      cli::cli_abort(c("Credential {.val {spec$id}} not found.",
                       "i" = "Set keyring service {.val {spec$service}} (user {.val {spec$id}}) or {.envvar {spec$env}}."))
    return(NULL)
  }
  if (!is.null(spec$expect_nchar) && nchar(secret) != spec$expect_nchar)
    cli::cli_warn("Credential {.val {spec$id}} is {nchar(secret)} chars (expected {spec$expect_nchar}).")
  if (verbose >= 2L)
    cli::cli_alert_info("credential {.val {spec$id}} resolved from {if (nzchar(kr)) 'keyring' else 'env'}.")
  secret
}

#' Is a credential resolvable? (shares the exact resolver, so it never disagrees)
#'
#' A read-only check: it never emits the keyring/env divergence warning that
#' [resolve_credentials()] would (it runs quietly).
#' @param spec A [credential_spec()], or `NULL`.
#' @return `TRUE` if the credential resolves to a non-empty secret.
#' @export
has_credential <- function(spec) {
  if (is.null(spec)) return(FALSE)
  if (!inherits(spec, "credential_spec"))
    cli::cli_abort("{.arg spec} must be a {.cls credential_spec} (see {.fn credential_spec}).")
  !is.null(tryCatch(resolve_credentials(spec, verbose = 0L), error = function(e) NULL))
}

# ---------------------------------------------------------------------------
# fetch_snapshot: source -> named list of tables -> snapshot
# ---------------------------------------------------------------------------
#' Fetch a snapshot from a data source
#'
#' Pulls every table declared by a data source, validates the returned tables,
#' and assembles them into an in-memory snapshot with a redacted record of the
#' source for the audit trail.
#'
#' @param x A `datasource` object (for example from [datasource_redcap()],
#'   [datasource_sql()], or [datasource_example()]).
#' @param verbose Integer verbosity level: `0` silent, `1` brief, `2` detailed.
#' @param ... Passed on to the source's fetch function.
#'
#' @return A `bctu_snapshot` object: a named list of data frames carrying the
#'   snapshot id, data-cut date, and redacted source as attributes.
#' @export
fetch_snapshot <- function(x, ...) UseMethod("fetch_snapshot")

#' @rdname fetch_snapshot
#' @export
fetch_snapshot.datasource <- function(x, verbose = 2L, ...) {
  creds  <- resolve_credentials(x$creds, verbose = verbose)
  tables <- x$fetch(creds = creds, config = x$config, verbose = verbose, ...)
  tables <- validate_tables(tables)
  as_snapshot(tables, source = redact_source(x),
              name = x$config$name %||% x$label)
}

#' @keywords internal
validate_tables <- function(tables) {
  if (is.data.frame(tables)) tables <- list(records = tables)
  if (!is.list(tables) || is.null(names(tables)) || any(!nzchar(names(tables))))
    cli::cli_abort("A source's fetch() must return a NAMED list of data frames (one per table).")
  ok <- vapply(tables, is.data.frame, logical(1))
  if (!all(ok))
    cli::cli_abort("Every element returned by fetch() must be a data frame; bad: {.val {names(tables)[!ok]}}.")
  tables
}

#' @keywords internal
redact_source <- function(x) {
  list(type = x$type, label = x$label,
       config = x$config[!vapply(x$config, is.function, logical(1))],
       credential = if (is.null(x$creds)) NULL else
         list(id = x$creds$id, service = x$creds$service, env = x$creds$env))
}

# ---------------------------------------------------------------------------
# Example source: simulated, deterministic, zero API, zero real data.
# REDCap-flavour = one table; SQL-flavour = multiple tables.
# `drift` adds records so two snapshots differ (drives the update-diff test).
# ---------------------------------------------------------------------------
#' A simulated example datasource (no database, deterministic)
#' @param kind `"redcap"` (single table) or `"sql"` (multiple tables).
#' @param n Base number of participants.
#' @param seed RNG seed for reproducibility.
#' @param drift Extra participants added on top of `n` (to differ between runs).
#' @param name Snapshot name.
#' @export
datasource_example <- function(kind = c("redcap", "sql"), n = 40L, seed = 1L,
                               drift = 0L, name = "example") {
  kind <- match.arg(kind)
  fetch <- function(creds, config, verbose = 2L, ...) {
    old <- if (exists(".Random.seed", .GlobalEnv)) get(".Random.seed", .GlobalEnv) else NULL
    on.exit(if (!is.null(old)) assign(".Random.seed", old, .GlobalEnv), add = TRUE)
    set.seed(config$seed)
    N <- config$n + config$drift
    records <- data.frame(
      record_id = sprintf("E%03d", seq_len(N)),
      event     = "baseline",
      age       = as.integer(round(stats::rnorm(N, 52, 14))),
      sex       = sample(c("F", "M"), N, replace = TRUE),
      weight_kg = round(stats::rnorm(N, 78, 16), 1),
      outcome   = stats::rbinom(N, 1L, 0.3),
      stringsAsFactors = FALSE
    )
    if (verbose >= 1L) cli::cli_alert_success("example fetch ({config$kind}): {N} participants")
    if (identical(config$kind, "sql")) {
      visits <- data.frame(
        record_id = rep(records$record_id, each = 2L),
        visit     = rep(c("v1", "v2"), times = N),
        visit_weight_kg = round(stats::rnorm(2L * N, 78, 16), 1),
        stringsAsFactors = FALSE
      )
      list(subjects = records, visits = visits)
    } else {
      list(records = records)
    }
  }
  new_datasource("example", fetch = fetch, creds = NULL,
                 config = list(kind = kind, n = n, seed = seed, drift = drift, name = name),
                 label = paste0("simulated ", kind))
}
