# ---------------------------------------------------------------------------
# Real datasource presets: REDCap and SQL Server
# ---------------------------------------------------------------------------
# Each preset is a THIN constructor over new_datasource(): it defines a `fetch`
# closure and returns new_datasource(...). No bespoke S3 fetch/take_snapshot
# method is written -- those are inherited from R/datasource.R and R/snapshot.R.
# `fetch` returns a NAMED LIST of data frames (one per table). REDCap is the
# one-table case (`records`); SQL is the multi-table case.
#
# Connectors, connection specs and request builders are lexically captured by
# the fetch closure. The datasource `config` holds only human-readable
# descriptive fields (url, server, database, table list, ...), because `config`
# is written verbatim into the snapshot manifest for the audit trail. Secrets,
# ODBC driver objects and request objects are never placed in `config`.

# ===========================================================================
# REDCap
# ===========================================================================

#' A REDCap datasource (one table: the exported records)
#'
#' Exports records from a REDCap project over its API and returns them as a
#' single-table snapshot. When `report_id` is given, a saved REDCap report is
#' exported instead of the full record set. The project data dictionary and the
#' export field names are fetched alongside the records and attached to the
#' records data frame as attributes (`redcap_dictionary`, `redcap_field_names`)
#' so downstream steps can label or reshape the data.
#'
#' The API token is never passed here. Declare where it lives with `token_id`
#' (a [credential_spec()] keyed on that id, expecting the 32-character REDCap
#' token) and the resolver reads it from the keyring or environment at fetch
#' time.
#'
#' Datetimes are returned exactly as REDCap sends them (text); no timezone is
#' guessed or applied.
#'
#' @param token_id Logical credential name for the REDCap API token (resolved
#'   via [credential_spec()] / [resolve_credentials()]). REDCap tokens are 32
#'   characters.
#' @param url REDCap API endpoint, e.g. `"https://redcap.example.org/api/"`.
#' @param report_id Optional saved-report id. If given, exports that report
#'   instead of all records.
#' @param labelled If `TRUE` (default), apply REDCap value labels (from the data
#'   dictionary) to coded fields using haven-style labelled vectors. Set `FALSE`
#'   to keep raw codes.
#' @param missing_codes Optional [special_missing()] mapping. When given, the
#'   named missing-data codes (e.g. `"UNK"`, `"OTH"`) are converted to native
#'   special missing values at extraction time and each column keeps its natural
#'   type, instead of the whole column coercing to character.
#' @param service Keyring service for the token; default `"bctu_api_token"`. Set
#'   a per-project service to avoid sharing one keyring entry across trials.
#' @param name Optional snapshot/label name; defaults to `"redcap"`.
#' @param ... Extra REDCap API parameters passed on the record/report export
#'   (e.g. `rawOrLabel`, `exportDataAccessGroups`, `filterLogic`).
#' @return A `datasource` object.
#' @seealso [datasource_sql()], [special_missing()], [take_snapshot()]
#' @examples
#' \dontrun{
#' ds <- datasource_redcap(
#'   token_id = "ocean",
#'   url = "https://redcap.example.org/api/"
#' )
#' take_snapshot(ds)
#' }
#' @export
datasource_redcap <- function(token_id, url, report_id = NULL, labelled = TRUE,
                              missing_codes = NULL, service = "bctu_api_token",
                              name = NULL, ...) {
  if (!is_string(token_id)) cli::cli_abort("{.arg token_id} must be a single string.")
  if (!is_string(url)) cli::cli_abort("{.arg url} must be a single string (the REDCap API URL).")
  if (!is.null(missing_codes) && !inherits(missing_codes, "bctu_special_missing"))
    cli::cli_abort("{.arg missing_codes} must be a {.fn special_missing} mapping or NULL.")
  creds <- credential_spec(token_id, service = service, expect_nchar = 32L)
  extra <- list(...)

  fetch <- function(creds, config, verbose = 2L, ...) {
    token <- creds
    if (is.null(token) || !nzchar(token))
      cli::cli_abort(c("The REDCap token {.val {config$token_id}} did not resolve.",
                       "i" = "Set it in the keyring or the matching environment variable."))
    content <- if (is.null(config$report_id)) "record" else "report"

    req  <- redcap_request(config$url, token, content = content,
                           report_id = config$report_id, extra = config$extra)
    body <- redcap_perform(req, sprintf("REDCap %s export", content))

    mapping <- config$missing_codes
    records <- if (is.null(mapping))
      redcap_parse_records(body)
    else
      apply_special_missing(redcap_read_csv(body, as_character = TRUE), mapping, verbose = verbose)

    if (nrow(records) == 0L && verbose >= 1L)
      cli::cli_warn(c("REDCap returned no records for {.val {config$name}}.",
                      "i" = "Check the token, any filterLogic, and that the project has data."))

    dictionary  <- redcap_metadata(config$url, token)
    field_names <- redcap_field_names(config$url, token)
    if (isTRUE(config$labelled))
      records <- redcap_apply_labels(records, dictionary, field_names)

    attr(records, "redcap_dictionary")  <- dictionary
    attr(records, "redcap_field_names") <- field_names
    if (!is.null(mapping)) attr(records, "bctu_special_missing") <- mapping

    if (verbose >= 1L)
      cli::cli_alert_success("REDCap {content}: {nrow(records)} record{?s} x {ncol(records)} field{?s}.")
    list(records = records)
  }

  config <- drop_null(list(url = url, report_id = report_id, labelled = labelled,
                           missing_codes = missing_codes, token_id = token_id,
                           extra = extra, name = name %||% "redcap"))
  new_datasource("redcap", fetch = fetch, creds = creds, config = config,
                 label = name %||% paste0("REDCap ", url))
}

#' Perform a REDCap request with a plain-English error and an error-body guard
#'
#' REDCap can return HTTP 200 with an error payload (e.g. a bad token) rather than
#' a non-2xx status. This performs the request (requiring `httr2`), turns network
#' failures into an actionable message, and rejects an error body so it can never
#' be snapshotted as data.
#' @param req An `httr2` request.
#' @param what Short description for error messages.
#' @return The response body as a string.
#' @export
redcap_perform <- function(req, what = "REDCap request") {
  if (!requireNamespace("httr2", quietly = TRUE))
    cli::cli_abort(c("Package {.pkg httr2} is required for REDCap access.",
                     "i" = "Install it with {.code install.packages(\"httr2\")}."))
  resp <- tryCatch(httr2::req_perform(req),
                   error = function(e)
                     cli::cli_abort(c("{what} failed to reach the server.",
                                      "x" = conditionMessage(e),
                                      "i" = "Check the API URL, your network, and any VPN.")))
  body <- httr2::resp_body_string(resp)
  redcap_guard_body(body, what)
  body
}

#' @keywords internal
redcap_guard_body <- function(body, what) {
  head <- trimws(substr(body, 1L, 64L))
  if (grepl('^\\{\\s*"error"', head) || grepl('^\\{\\s*\\047error\\047', head)) {
    snippet <- substr(trimws(body), 1L, 200L)   # passed as a value so cli never glue-parses its braces
    cli::cli_abort(c("{what} returned an error, not data.",
                     "x" = "{snippet}",
                     "i" = "Nothing was snapshotted. Check the token and permissions."))
  }
  invisible(body)
}

#' Build a REDCap API request (POST, x-www-form-urlencoded)
#'
#' Factored out of the fetch closure so the request can be inspected in tests
#' without hitting a live server.
#' @param url REDCap API URL.
#' @param token Resolved API token (secret).
#' @param content REDCap `content`, e.g. `"record"`, `"report"`, `"metadata"`,
#'   `"exportFieldNames"`.
#' @param format Export format; default `"csv"`.
#' @param report_id Report id (required when `content = "report"`).
#' @param extra Named list of extra API parameters.
#' @return An `httr2` request.
#' @export
redcap_request <- function(url, token, content = "record", format = "csv",
                           report_id = NULL, extra = list()) {
  if (!requireNamespace("httr2", quietly = TRUE))
    cli::cli_abort(c("Package {.pkg httr2} is required for REDCap access.",
                     "i" = "Install it with {.code install.packages(\"httr2\")}."))
  body <- list(token = token, content = content, format = format,
               type = "flat", returnFormat = "json")
  if (identical(content, "report")) {
    if (is.null(report_id))
      cli::cli_abort("A REDCap report export needs a {.arg report_id}.")
    body$report_id <- report_id
  }
  body <- c(body, extra)
  req  <- httr2::request(url)
  req  <- httr2::req_method(req, "POST")
  req  <- httr2::req_user_agent(req, "bctu R package")
  do.call(httr2::req_body_form, c(list(req), body))
}

#' Fetch and parse the REDCap data dictionary (`content = "metadata"`)
#' @param url REDCap API URL.
#' @param token Resolved API token.
#' @return A data frame (the data dictionary).
#' @export
redcap_metadata <- function(url, token) {
  redcap_read_csv(redcap_perform(redcap_request(url, token, content = "metadata"),
                                 "REDCap metadata export"))
}

#' Fetch and parse REDCap export field names (`content = "exportFieldNames"`)
#' @param url REDCap API URL.
#' @param token Resolved API token.
#' @return A data frame mapping fields to exported column names.
#' @export
redcap_field_names <- function(url, token) {
  redcap_read_csv(redcap_perform(redcap_request(url, token, content = "exportFieldNames"),
                                 "REDCap field-names export"))
}

#' Parse a REDCap CSV records payload into a data frame
#' @param csv_text CSV text as returned by the REDCap API.
#' @return A data frame of records.
#' @export
redcap_parse_records <- function(csv_text) {
  redcap_read_csv(csv_text)
}

#' @keywords internal
redcap_read_csv <- function(csv_text, as_character = FALSE) {
  if (!nzchar(csv_text)) return(data.frame())
  if (as_character)
    return(utils::read.csv(text = csv_text, stringsAsFactors = FALSE,
                           colClasses = "character", check.names = FALSE))
  if (requireNamespace("readr", quietly = TRUE))
    as.data.frame(readr::read_csv(I(csv_text), show_col_types = FALSE,
                                  na = character()))
  else
    utils::read.csv(text = csv_text, stringsAsFactors = FALSE,
                    colClasses = "character", check.names = FALSE)
}

#' Apply REDCap value labels to coded fields (haven-style labelled vectors)
#'
#' Labels radio, dropdown and yesno fields from the data dictionary. Checkbox
#' fields (which REDCap splits into multiple `field___code` columns) are left as
#' raw 0/1 indicators. If the `haven` package is not installed, the records are
#' returned unchanged with a warning.
#' @param records Records data frame.
#' @param dictionary REDCap data dictionary data frame.
#' @param field_names Optional REDCap field-name export (`exportFieldNames`)
#'   with `original_field_name` and `export_field_name` columns; required to
#'   label checkbox columns, whose exported names (`field___N`) differ from
#'   their dictionary field name.
#' @return `records` with labels applied where possible.
#' @export
redcap_apply_labels <- function(records, dictionary, field_names = NULL) {
  if (is.null(dictionary) || !nrow(dictionary)) return(records)
  needed <- c("field_name", "field_type", "select_choices_or_calculations")
  if (!all(needed %in% names(dictionary))) return(records)
  if (!requireNamespace("haven", quietly = TRUE)) {
    cli::cli_warn("Package {.pkg haven} not installed; returning raw REDCap codes.")
    return(records)
  }

  # Exported column names come from the field-name export: a checkbox field
  # `x` with choices 1..n arrives as columns `x___1` .. `x___n`, so matching
  # dictionary rows straight onto column names misses every checkbox. Without
  # the export map, fall back to the identity mapping (no checkbox coverage).
  map <- if (!is.null(field_names) &&
             all(c("original_field_name", "export_field_name") %in% names(field_names))) {
    field_names[, c("original_field_name", "export_field_name",
                    intersect("choice_value", names(field_names)))]
  } else {
    data.frame(original_field_name = dictionary$field_name,
               export_field_name = dictionary$field_name,
               stringsAsFactors = FALSE)
  }
  dict_at <- match(map$original_field_name, dictionary$field_name)

  for (j in seq_len(nrow(map))) {
    i <- dict_at[j]
    if (is.na(i)) next
    col  <- map$export_field_name[j]
    type <- dictionary$field_type[i]
    if (!col %in% names(records)) next
    x <- records[[col]]
    if (is.logical(x) || inherits(x, "haven_labelled")) next

    if (type %in% c("yesno", "checkbox")) {
      choices <- data.frame(code = c("0", "1"), label = c("No", "Yes"),
                            stringsAsFactors = FALSE)
    } else if (type %in% c("radio", "dropdown")) {
      choices <- redcap_parse_choices(dictionary$select_choices_or_calculations[i])
    } else {
      next
    }
    if (is.null(choices) || !nrow(choices)) next
    records[[col]] <- redcap_labelled(x, choices)
  }
  records
}

#' @keywords internal
redcap_parse_choices <- function(spec) {
  if (is.null(spec) || is.na(spec) || !nzchar(spec)) return(NULL)
  parts <- trimws(strsplit(spec, "\\|", fixed = FALSE)[[1]])
  parts <- parts[nzchar(parts)]
  code  <- sub("\\s*,.*$", "", parts)
  label <- trimws(sub("^[^,]*,", "", parts))
  data.frame(code = trimws(code), label = label, stringsAsFactors = FALSE)
}

#' @keywords internal
redcap_labelled <- function(x, choices) {
  numeric_codes <- suppressWarnings(!any(is.na(as.numeric(choices$code))))
  if (numeric_codes) {
    values <- stats::setNames(as.numeric(choices$code), choices$label)
    x <- suppressWarnings(as.numeric(x))
  } else {
    values <- stats::setNames(as.character(choices$code), choices$label)
    x <- as.character(x)
  }
  haven::labelled(x, labels = values)
}

# ===========================================================================
# SQL Server (multi-table)
# ===========================================================================

#' Describe an ODBC connection to a SQL Server
#'
#' Captures the full ODBC connection surface so nothing is silently dropped
#' (the old package dropped auth, port and extra parameters). Values left `NULL`
#' are omitted from the connection string. `TrustServerCertificate` and
#' `trusted_connection` are logical toggles rendered as `"yes"`/`"no"`.
#' @param driver ODBC driver name; default `"ODBC Driver 18 for SQL Server"`.
#' @param uid User id for SQL authentication (omit for Windows/trusted auth).
#' @param pwd Password for SQL authentication.
#' @param port TCP port.
#' @param trusted_connection Use Windows/integrated authentication (`TRUE`/`FALSE`).
#' @param encoding Client character encoding.
#' @param trust_server_certificate Trust the server TLS certificate (`TRUE`/`FALSE`).
#' @param extra Named list of any additional ODBC keywords, passed through verbatim.
#' @return An `sql_connection` object.
#' @seealso [datasource_sql()]
#' @export
sql_connection <- function(driver = "ODBC Driver 18 for SQL Server",
                           uid = NULL, pwd = NULL, port = NULL,
                           trusted_connection = NULL, encoding = NULL,
                           trust_server_certificate = NULL, extra = list()) {
  if (!is_string(driver)) cli::cli_abort("{.arg driver} must be a single string.")
  if (!is.list(extra)) cli::cli_abort("{.arg extra} must be a (named) list of ODBC keywords.")
  structure(list(driver = driver, uid = uid, pwd = pwd, port = port,
                 trusted_connection = trusted_connection, encoding = encoding,
                 trust_server_certificate = trust_server_certificate, extra = extra),
            class = "sql_connection")
}

#' A SQL Server datasource (multiple tables/views)
#'
#' Connects to a SQL Server database and loads a set of tables or views into a
#' named list of data frames (a multi-table snapshot). The table set is either
#' given explicitly (`tables`) or discovered by running `views_query` (a query
#' returning object names in its first column, e.g.
#' `"SELECT name FROM sys.views WHERE name LIKE 'vw%'"`); each discovered object
#' is then loaded with `SELECT * FROM <name>` (the name quoted as an identifier).
#'
#' The full ODBC connection surface is honoured via [sql_connection()]. To test
#' or use a non-SQL-Server backend, inject a different DBI `connector` (the
#' driver object) and `connect_args`; by default the connector is
#' `odbc::odbc()` and the connection arguments are built from `server`,
#' `database` and `conn`.
#'
#' Datetimes are returned exactly as the database driver yields them; bctu does
#' not silently coerce timezones (a known GMT/BST trap). Apply an explicit
#' timezone in your analysis if the DB column is timezone-naive.
#'
#' If any requested object loads as something other than a data frame (a view
#' whose SQL errored can arrive as a character error message), the fetch aborts
#' loudly naming the offending object rather than snapshotting the error.
#'
#' @param server SQL Server host (or instance).
#' @param database Database name.
#' @param tables Optional character vector of table/view names to load.
#' @param views_query Optional discovery query returning object names in its
#'   first column. Used when `tables` is `NULL`.
#' @param conn An [sql_connection()] describing the ODBC connection.
#' @param include Optional regular expression; when discovering via
#'   `views_query`, keep only names matching it (a view-subset toggle).
#' @param connector Optional DBI driver object to inject (default `odbc::odbc()`
#'   at fetch time). Used for testing against other backends (e.g.
#'   `RSQLite::SQLite()`).
#' @param connect_args Optional named list of `DBI::dbConnect()` arguments to
#'   use verbatim with `connector`, bypassing the ODBC argument builder.
#' @param name Optional snapshot/label name; defaults to `database`.
#' @param ... Reserved for future use.
#' @return A `datasource` object.
#' @seealso [sql_connection()], [datasource_redcap()], [take_snapshot()]
#' @examples
#' \dontrun{
#' ds <- datasource_sql(
#'   server = "sql.example.org",
#'   database = "OCEAN",
#'   tables = c("subjects", "visits")
#' )
#' take_snapshot(ds)
#' }
#' @export
datasource_sql <- function(server, database, tables = NULL, views_query = NULL,
                           conn = sql_connection(), include = NULL,
                           connector = NULL, connect_args = NULL,
                           name = NULL, ...) {
  if (missing(server) || !is_string(server)) cli::cli_abort("{.arg server} must be a single string.")
  if (missing(database) || !is_string(database)) cli::cli_abort("{.arg database} must be a single string.")
  if (grepl("[;=\n\r]|[[:cntrl:]]", server) || grepl("[;=\n\r]|[[:cntrl:]]", database))
    cli::cli_abort(c("{.arg server} and {.arg database} may not contain {.code ;}, {.code =}, newlines or control characters.",
                     "i" = "Those characters would alter the ODBC connection string; pass extra connection options via {.fn sql_connection}."))
  if (is.null(tables) && is.null(views_query))
    cli::cli_abort(c("{.fn datasource_sql} needs either {.arg tables} or {.arg views_query}.",
                     "i" = "Give an explicit table/view vector, or a discovery query like {.code SELECT name FROM sys.views WHERE name LIKE 'vw%'}."))
  if (!is.null(tables) && !is.character(tables))
    cli::cli_abort("{.arg tables} must be a character vector of table/view names.")
  if (!inherits(conn, "sql_connection"))
    cli::cli_abort("{.arg conn} must be an {.fn sql_connection}.")

  fetch <- function(creds, config, verbose = 2L, ...) {
    driver <- connector %||% odbc::odbc()
    args   <- connect_args %||% sql_odbc_arguments(config$server, config$database, conn)
    con    <- tryCatch(
      do.call(DBI::dbConnect, c(list(driver), args)),
      error = function(e)
        cli::cli_abort(c(
          "Could not connect to the SQL database {.val {config$database}} on {.val {config$server}}.",
          "i" = "Check the server, database, ODBC driver and credentials.")))
    on.exit(DBI::dbDisconnect(con), add = TRUE)

    to_load <- config$tables
    if (is.null(to_load))
      to_load <- sql_discover_objects(con, config$views_query, config$include, verbose)
    if (!length(to_load))
      cli::cli_abort("No tables/views to load (empty {.arg tables} / discovery result).")

    tables <- stats::setNames(lapply(to_load, function(nm) sql_read_object(con, nm)), to_load)
    sql_guard_dataframes(tables)
    if (verbose >= 1L)
      cli::cli_alert_success("SQL: loaded {length(tables)} table{?s}: {.val {names(tables)}}.")
    tables
  }

  config <- drop_null(list(server = server, database = database, tables = tables,
                           views_query = views_query, include = include,
                           name = name %||% database))
  new_datasource("sql", fetch = fetch, creds = NULL, config = config,
                 label = name %||% paste0(database, " @ ", server))
}

#' Build the ODBC `DBI::dbConnect()` argument list from an sql_connection
#' @param server SQL Server host.
#' @param database Database name.
#' @param conn An [sql_connection()].
#' @return A named list of connection arguments.
#' @export
sql_odbc_arguments <- function(server, database, conn) {
  yes_no <- function(v) if (isTRUE(v)) "yes" else "no"
  args <- list(Driver = conn$driver, Server = server, Database = database)
  if (!is.null(conn$uid))                      args$UID                     <- conn$uid
  if (!is.null(conn$pwd))                      args$PWD                     <- conn$pwd
  if (!is.null(conn$port))                     args$Port                    <- conn$port
  if (!is.null(conn$trusted_connection))       args$Trusted_Connection      <- yes_no(conn$trusted_connection)
  if (!is.null(conn$encoding))                 args$encoding                <- conn$encoding
  if (!is.null(conn$trust_server_certificate)) args$TrustServerCertificate  <- yes_no(conn$trust_server_certificate)
  c(args, conn$extra)
}

#' Run a discovery query and return the object names in its first column
#' @param con A DBI connection.
#' @param views_query Discovery SQL.
#' @param include Optional regex to subset the names.
#' @param verbose Verbosity.
#' @return A character vector of object names.
#' @export
sql_discover_objects <- function(con, views_query, include = NULL, verbose = 2L) {
  if (!is_string(views_query))
    cli::cli_abort("{.arg views_query} must be a single SQL string returning object names.")
  res <- DBI::dbGetQuery(con, views_query)
  if (!is.data.frame(res) || ncol(res) < 1L)
    cli::cli_abort("The discovery query must return at least one column of object names.")
  found <- as.character(res[[1L]])
  if (!is.null(include)) found <- found[grepl(include, found)]
  if (verbose >= 2L)
    cli::cli_alert_info("discovery query matched {length(found)} object{?s}.")
  found
}

#' Load one object with a quoted `SELECT * FROM <name>`
#'
#' The name is quoted as an SQL identifier (never interpolated raw). On a query
#' error the object name is included in the message.
#' @param con A DBI connection.
#' @param name Object (table/view) name.
#' @return A data frame.
#' @export
sql_read_object <- function(con, name) {
  query <- paste0("SELECT * FROM ", DBI::dbQuoteIdentifier(con, name))
  tryCatch(
    DBI::dbGetQuery(con, query),
    error = function(e)
      cli::cli_abort(c("Failed to load {.val {name}} from the database.",
                       "x" = conditionMessage(e)))
  )
}

#' Guard: every loaded object must be a data frame (CRCTU expect.dataframes)
#'
#' A view whose SQL errored can come back as a character error string rather
#' than a table. This aborts loudly, naming the offending objects, so an error
#' message is never silently snapshotted as data.
#' @param tables A named list of loaded objects.
#' @return `tables`, invisibly, if all are data frames.
#' @export
sql_guard_dataframes <- function(tables) {
  ok <- vapply(tables, is.data.frame, logical(1))
  if (!all(ok))
    cli::cli_abort(c("SQL returned results that are not tables.",
                     "x" = "Not data frames: {.val {names(tables)[!ok]}}.",
                     "i" = "A view whose query failed can load as an error string; refusing to snapshot it."))
  invisible(tables)
}

# ===========================================================================
# Small shared helper
# ===========================================================================

#' @keywords internal
drop_null <- function(x) x[!vapply(x, is.null, logical(1))]
