# ---------------------------------------------------------------------------
# Validation: three layers, clearly separated (MHRA / EMA / FDA / GCP)
# ---------------------------------------------------------------------------
# The rebuild plan (§8) defines THREE distinct validation layers. This file
# builds only layer (a); it points at the pharmaR tools for (b) and (c) and does
# NOT recreate them.
#
#   (a) Installation / environment IQ-OQ  -- WE build this, here.
#       `check_setup()` runs a battery of named checks that qualify that R, the
#       system toolchain and the bctu dependencies are INSTALLED correctly (IQ)
#       and OPERATE to spec (OQ) on a given machine and R release. It is modelled
#       on the R Foundation regulatory document "R: Regulatory Compliance and
#       Validation Issues" (R-FDA.pdf, https://www.r-project.org/doc/R-FDA.pdf)
#       and the marcschwartz/R-IQ-OQ approach that document underpins.
#       `write_setup_report()` writes the result as an archivable, human- and
#       machine-readable YAML qualification record.
#
#   (b) Package validation (bctu itself) -- use pharmaR `valtools`, NOT recreated.
#       Formal validation of bctu (requirements -> test cases -> traceability
#       matrix -> a signed validation report, plus known-truth numerical checks)
#       is produced with `valtools` (valtools::vdoc / the validation report) at
#       RELEASE time. bctu supplies the content (requirements, the testthat
#       suite, golden results); valtools supplies the framework. We do not
#       reimplement it.
#
#   (c) Dependency risk -- use pharmaR `riskmetric`, NOT recreated.
#       Risk-scoring of bctu's dependencies follows the R Validation Hub
#       approach with `riskmetric` (and the `riskassessment` app).
#       `package_risk_report()` below is a thin, optional wrapper over
#       `riskmetric`; it never re-implements the scoring.

# ---------------------------------------------------------------------------
# One check = one row. Each named check function returns rows built by this
# constructor. `passed` drives the overall verdict; `status` is the display
# label; `fatal` marks a check that MUST pass for the machine to qualify.
# ---------------------------------------------------------------------------
#' @keywords internal
setup_check_row <- function(check, status, detail, fatal, passed) {
  data.frame(check = check, status = status, detail = detail,
             fatal = fatal, passed = passed, stringsAsFactors = FALSE)
}

# --- small, explicitly-named toolchain probes ------------------------------
#' First line of a program's version output, or NA if it is not on the PATH
#' @keywords internal
program_version_line <- function(program, version_args = "--version") {
  bin <- Sys.which(program)
  if (!nzchar(bin)) return(NA_character_)
  out <- tryCatch(
    suppressWarnings(system2(bin, version_args, stdout = TRUE, stderr = TRUE)),
    error = function(e) character(0)
  )
  if (!length(out)) return(NA_character_)
  trimws(out[1])
}

#' Pandoc version as a plain "X.Y.Z" string, or NA if pandoc is absent
#' @keywords internal
pandoc_version_string <- function() {
  if (requireNamespace("rmarkdown", quietly = TRUE)) {
    v <- tryCatch(as.character(rmarkdown::pandoc_version()),
                  error = function(e) NA_character_)
    if (!is.na(v)) return(v)
  }
  line <- program_version_line("pandoc")
  if (is.na(line)) return(NA_character_)
  sub("^pandoc(\\.exe)?[[:space:]]+", "", line)
}

#' Installed version of a package as a string, or NA if it is not installed
#' @keywords internal
installed_version_or_na <- function(package) {
  if (requireNamespace(package, quietly = TRUE))
    as.character(utils::packageVersion(package))
  else
    NA_character_
}

# --- the individual checks -------------------------------------------------
#' @keywords internal
check_r_version <- function() {
  ok <- getRversion() >= "4.1.0"
  setup_check_row(
    "R version",
    if (ok) "PASS" else "FAIL",
    if (ok)
      paste0(R.version.string, " (>= 4.1.0).")
    else
      paste0(R.version.string,
             " is older than 4.1.0. bctu uses the native pipe |>; install R 4.1.0 or newer."),
    fatal = TRUE, passed = ok)
}

#' @keywords internal
check_hard_dependencies <- function() {
  deps <- c("cli", "digest", "yaml")
  rows <- lapply(deps, function(pkg) {
    ver <- installed_version_or_na(pkg)
    ok <- !is.na(ver)
    setup_check_row(
      paste0("Required package: ", pkg),
      if (ok) "PASS" else "FAIL",
      if (ok)
        paste0(pkg, " ", ver, " installed.")
      else
        paste0(pkg, " is not installed. Install it with install.packages(\"", pkg, "\")."),
      fatal = TRUE, passed = ok)
  })
  do.call(rbind, rows)
}

#' @keywords internal
check_rendering_toolchain <- function(formats) {
  rows <- list()
  # pandoc is needed for every requested output format.
  pv <- pandoc_version_string()
  pandoc_ok <- !is.na(pv)
  rows[[length(rows) + 1L]] <- setup_check_row(
    "Report engine: pandoc",
    if (pandoc_ok) "PASS" else "FAIL",
    if (pandoc_ok)
      paste0("pandoc ", pv, " found on the PATH.")
    else
      "pandoc was not found on the PATH. Install pandoc (https://pandoc.org/installing.html) to render reports.",
    fatal = TRUE, passed = pandoc_ok)

  # A LaTeX engine (xelatex) is needed ONLY when PDF output is requested.
  if ("pdf" %in% formats) {
    xv <- program_version_line("xelatex")
    tinytex_ok <- requireNamespace("tinytex", quietly = TRUE) &&
      isTRUE(tryCatch(tinytex::is_tinytex(), error = function(e) FALSE))
    latex_ok <- !is.na(xv) || tinytex_ok
    detail <- if (latex_ok) {
      if (!is.na(xv))
        paste0("xelatex found on the PATH (", xv, ").")
      else
        "A working TinyTeX installation was found (tinytex::is_tinytex())."
    } else {
      "No xelatex on the PATH and no TinyTeX. Install TinyTeX with tinytex::install_tinytex(), or install a LaTeX distribution, to render PDF."
    }
    rows[[length(rows) + 1L]] <- setup_check_row(
      "PDF engine: xelatex / LaTeX",
      if (latex_ok) "PASS" else "FAIL",
      detail, fatal = TRUE, passed = latex_ok)
  }
  do.call(rbind, rows)
}

#' @keywords internal
check_suggested_packages <- function() {
  # Integration packages needed for specific tasks; informational, never fatal.
  purposes <- c(
    httr2    = "REDCap API access",
    keyring  = "storing REDCap / API tokens",
    DBI      = "SQL data sources",
    odbc     = "SQL data sources (ODBC)",
    openxlsx = "writing xlsx findings",
    readr    = "reading delimited files",
    haven    = "reading SAS / Stata / SPSS files",
    withr    = "scoped options in reporting"
  )
  rows <- lapply(names(purposes), function(pkg) {
    ver <- installed_version_or_na(pkg)
    present <- !is.na(ver)
    setup_check_row(
      paste0("Optional package: ", pkg),
      if (present) "PRESENT" else "ABSENT",
      if (present)
        paste0(pkg, " ", ver, " installed (for ", purposes[[pkg]], ").")
      else
        paste0(pkg, " not installed; needed only for ", purposes[[pkg]],
               ". Install with install.packages(\"", pkg, "\") if you need it."),
      fatal = FALSE, passed = present)
  })
  do.call(rbind, rows)
}

#' @keywords internal
check_snapshot_store_writable <- function(store) {
  resolved <- store
  if (is.null(resolved))
    resolved <- tryCatch(snapshot_store(verbose = 0L), error = function(e) NULL)
  if (is.null(resolved))
    return(setup_check_row(
      "Snapshot store writable", "SKIP",
      paste0("No ", project_marker_name, " project found, so there is no snapshot store to test. ",
             "Run bctu_init_project(<name>) at the trial root, then re-run check_setup()."),
      fatal = FALSE, passed = TRUE))

  ok <- tryCatch({
    if (!dir.exists(resolved))
      dir.create(resolved, recursive = TRUE, showWarnings = FALSE)
    test_file <- file.path(resolved, paste0(basename(tempfile("bctu-write-test-")), ".tmp"))
    writeLines("bctu write test", test_file)
    wrote <- file.exists(test_file)
    unlink(test_file, force = TRUE)
    wrote && !file.exists(test_file)
  }, error = function(e) FALSE)

  setup_check_row(
    "Snapshot store writable",
    if (ok) "PASS" else "FAIL",
    if (ok)
      paste0("Wrote and removed a test file in ", resolved, ".")
    else
      paste0("Could not write to ", resolved,
             ". Check the folder exists and that you have write permission."),
    fatal = TRUE, passed = ok)
}

#' @keywords internal
check_git_available <- function() {
  gv <- program_version_line("git")
  ok <- !is.na(gv)
  setup_check_row(
    "git available (provenance)",
    if (ok) "PASS" else "WARN",
    if (ok)
      paste0(gv, " found on the PATH.")
    else
      "git was not found on the PATH. Snapshot provenance recording needs git; install it from https://git-scm.com/.",
    fatal = FALSE, passed = ok)
}

#' @keywords internal
check_required_credentials <- function(require_credentials) {
  if (is.null(require_credentials)) return(NULL)
  if (inherits(require_credentials, "credential_spec"))
    require_credentials <- list(require_credentials)
  if (!is.list(require_credentials))
    cli::cli_abort(c(
      "{.arg require_credentials} must be a {.cls credential_spec} or a list of them.",
      "i" = "Build one with {.code credential_spec(<id>)}."))
  rows <- lapply(require_credentials, function(spec) {
    if (!inherits(spec, "credential_spec"))
      cli::cli_abort("Each element of {.arg require_credentials} must be a {.cls credential_spec}.")
    # PRESENT / MISSING ONLY. The secret value is never resolved into, printed,
    # or returned here.
    present <- isTRUE(tryCatch(has_credential(spec), error = function(e) FALSE))
    setup_check_row(
      paste0("Credential present: ", spec$id),
      if (present) "PRESENT" else "MISSING",
      if (present)
        paste0("Credential '", spec$id, "' resolves (value not shown).")
      else
        paste0("Credential '", spec$id, "' not found. Set keyring service '",
               spec$service, "' (user '", spec$id, "') or environment variable ",
               spec$env, "."),
      fatal = TRUE, passed = present)
  })
  do.call(rbind, rows)
}

# --- environment capture ---------------------------------------------------
#' @keywords internal
capture_setup_environment <- function(formats) {
  hard_deps <- c("cli", "digest", "yaml")
  dep_versions <- stats::setNames(
    lapply(hard_deps, installed_version_or_na), hard_deps)
  session_lines <- if (requireNamespace("sessioninfo", quietly = TRUE))
    utils::capture.output(print(sessioninfo::session_info()))
  else
    utils::capture.output(print(utils::sessionInfo()))
  list(
    r_version    = R.version.string,
    platform     = R.version$platform,
    os           = utils::osVersion %||% Sys.info()[["sysname"]],
    running      = R.version$version.string,
    formats      = formats,
    dependencies = dep_versions,
    pandoc       = pandoc_version_string() %||% NA_character_,
    xelatex      = program_version_line("xelatex"),
    git          = program_version_line("git"),
    session_info = session_lines
  )
}

# --- printing (IQ/OQ-style table) ------------------------------------------
#' @keywords internal
print_qualification_table <- function(checks) {
  width_check  <- max(nchar(c("Check", checks$check)))
  width_status <- max(nchar(c("Result", checks$status)))
  header <- sprintf("  %-*s  %-*s  %s",
                    width_check, "Check", width_status, "Result", "Detail")
  rule <- paste0("  ", strrep("-", width_check), "  ",
                 strrep("-", width_status), "  ", strrep("-", 6L))
  cli::cli_verbatim(header)
  cli::cli_verbatim(rule)
  for (i in seq_len(nrow(checks))) {
    cli::cli_verbatim(sprintf("  %-*s  %-*s  %s",
                              width_check, checks$check[i],
                              width_status, checks$status[i],
                              checks$detail[i]))
  }
}

# ---------------------------------------------------------------------------
# check_setup(): the one obvious call to qualify a machine (layer a).
# ---------------------------------------------------------------------------
#' Check this machine is correctly set up to use bctu (installation IQ/OQ)
#'
#' Runs a battery of named installation-qualification / operational-qualification
#' checks, prints an IQ/OQ-style table (Check / Result / Detail) with an overall
#' PASS or FAIL, and returns (invisibly) a structured result you can archive with
#' [write_setup_report()]. This is validation layer (a): it qualifies that R, the
#' system toolchain and the bctu dependencies are installed correctly and operate
#' to spec on this machine. It is modelled on the R Foundation regulatory document
#' R-FDA.pdf and the marcschwartz/R-IQ-OQ approach.
#'
#' Formal validation of the bctu package itself is done separately with the
#' pharmaR `valtools` package at release time (requirements, test cases,
#' traceability matrix, validation report), and dependency risk is scored with
#' pharmaR `riskmetric` via [package_risk_report()]. bctu does not recreate
#' those frameworks; `check_setup()` is only the installation IQ/OQ.
#'
#' @param store Optional snapshot store directory to test writing to. If `NULL`
#'   (the default), the store is resolved from the current bctu project; if no
#'   project marker is found the store check is reported as an informational skip
#'   with the fix, not a failure.
#' @param formats Report output formats you intend to produce; the rendering
#'   toolchain is checked only for these. `"pdf"` additionally requires a LaTeX
#'   engine; a `"docx"`-only user is never failed for missing LaTeX. Default
#'   `c("docx", "pdf")`.
#' @param require_credentials Optional [credential_spec()] (or list of them) to
#'   confirm are available on this machine. Reported as PRESENT or MISSING only;
#'   the secret value is never resolved into output, printed, or returned.
#' @param verbose `2` prints the full table and the overall verdict (default),
#'   `1` prints only the overall verdict, `0` prints nothing.
#' @return Invisibly, a `bctu_setup_qualification` list with `ok` (overall pass),
#'   `checks` (a data frame of every check), `environment` (the captured
#'   environment), `formats`, and `time`.
#' @export
check_setup <- function(store = NULL,
                        formats = c("docx", "pdf"),
                        require_credentials = NULL,
                        verbose = 2L) {
  formats <- match.arg(formats, c("docx", "pdf"), several.ok = TRUE)

  checks <- rbind(
    check_r_version(),
    check_hard_dependencies(),
    check_rendering_toolchain(formats),
    check_suggested_packages(),
    check_snapshot_store_writable(store),
    check_git_available(),
    check_required_credentials(require_credentials)
  )
  rownames(checks) <- NULL

  ok <- !any(checks$fatal & !checks$passed)

  result <- structure(
    list(
      ok          = ok,
      checks      = checks,
      environment = capture_setup_environment(formats),
      formats     = formats,
      time        = iso8601()
    ),
    class = "bctu_setup_qualification"
  )

  if (verbose >= 2L) {
    cli::cli_h1("bctu setup qualification (IQ/OQ)")
    print_qualification_table(checks)
    cli::cli_text("")
  }
  if (verbose >= 1L) {
    n_fail <- sum(checks$fatal & !checks$passed)
    if (ok)
      cli::cli_alert_success("Overall: PASS. This machine is set up to use bctu.")
    else
      cli::cli_alert_danger(
        "Overall: FAIL. {n_fail} required check{?s} did not pass; fix the item{?s} marked FAIL/MISSING above.")
  }

  invisible(result)
}

#' @export
print.bctu_setup_qualification <- function(x, ...) {
  cli::cli_h1("bctu setup qualification (IQ/OQ)")
  print_qualification_table(x$checks)
  cli::cli_text("")
  if (isTRUE(x$ok))
    cli::cli_alert_success("Overall: PASS.")
  else
    cli::cli_alert_danger("Overall: FAIL.")
  invisible(x)
}

# ---------------------------------------------------------------------------
# write_setup_report(): the archivable IQ/OQ artefact (YAML).
# ---------------------------------------------------------------------------
#' Write a setup-qualification record as a YAML IQ/OQ artefact
#'
#' Writes the result of [check_setup()] to a human-readable and machine-readable
#' YAML file (schema `bctu-setup-qualification/1`): a timestamp, the overall
#' verdict, every check row, and a full environment capture (R version, platform,
#' dependency versions, pandoc / xelatex / git versions, and session-level
#' detail). This is the archivable installation-qualification artefact. No
#' credential value is ever written.
#'
#' @param result A `bctu_setup_qualification` from [check_setup()]. Defaults to
#'   running [check_setup()] silently.
#' @param path Output file path. Default `"bctu-setup-qualification.yml"`.
#' @return The absolute path of the written file, invisibly.
#' @export
write_setup_report <- function(result = check_setup(verbose = 0L),
                               path = "bctu-setup-qualification.yml") {
  if (!inherits(result, "bctu_setup_qualification"))
    cli::cli_abort(c(
      "{.arg result} must be a {.cls bctu_setup_qualification}.",
      "i" = "Produce one with {.code check_setup()}."))
  if (!is_string(path))
    cli::cli_abort("{.arg path} must be a single file path.")

  check_rows <- lapply(seq_len(nrow(result$checks)), function(i) {
    row <- result$checks[i, , drop = FALSE]
    list(check = row$check, status = row$status, detail = row$detail,
         fatal = isTRUE(row$fatal), passed = isTRUE(row$passed))
  })

  record <- list(
    schema          = "bctu-setup-qualification/1",
    generated       = result$time,
    overall_ok      = isTRUE(result$ok),
    formats_checked = result$formats,
    checks          = check_rows,
    environment     = result$environment
  )

  yaml::write_yaml(record, path)
  full <- normalizePath(path, winslash = "/", mustWork = TRUE)
  cli::cli_alert_success("setup qualification -> {.file {full}}")
  invisible(full)
}

# ---------------------------------------------------------------------------
# package_risk_report(): thin wrapper over pharmaR riskmetric (layer c).
# ---------------------------------------------------------------------------
#' @keywords internal
package_dependency_names <- function(path) {
  desc <- file.path(path, "DESCRIPTION")
  if (!file.exists(desc))
    cli::cli_abort(c("No DESCRIPTION file at {.file {path}}.",
                     "i" = "Point {.arg path} at an R package root."))
  fields <- read.dcf(desc, fields = c("Imports", "Depends"))
  raw <- unlist(strsplit(paste(fields[!is.na(fields)], collapse = ","), ","))
  names_only <- trimws(sub("\\(.*", "", raw))
  names_only <- names_only[nzchar(names_only) & names_only != "R"]
  unique(names_only)
}

#' Score bctu's dependency risk with pharmaR riskmetric (dependency-risk layer)
#'
#' A thin wrapper over the pharmaR / R Validation Hub `riskmetric` package. If
#' `riskmetric` is installed, it assesses and scores each of the package's
#' dependencies and returns a tidy risk summary. bctu does not recreate
#' `riskmetric`; if it is not installed this errors with how to install it.
#'
#' Dependency risk is validation layer (c). Formal validation of bctu itself is
#' layer (b), done with `valtools` at release time. The installation IQ/OQ is
#' layer (a), [check_setup()].
#'
#' @param path Package root whose dependencies are scored. Default `"."`.
#' @param ... Passed through to `riskmetric::pkg_assess()`.
#' @return A tidy data frame of dependency risk scores.
#' @export
package_risk_report <- function(path = ".", ...) {
  if (!requireNamespace("riskmetric", quietly = TRUE))
    cli::cli_abort(c(
      "The {.pkg riskmetric} package is not installed.",
      "i" = "Install it with {.code install.packages(\"riskmetric\")}.",
      "i" = "{.pkg riskmetric} is the pharmaR / R Validation Hub tool for dependency risk; bctu does not recreate it (see {.url https://pharmar.org}).",
      "x" = "No risk assessment was run."))

  deps <- package_dependency_names(path)
  if (!length(deps))
    cli::cli_abort("No package dependencies found in {.file {file.path(path, 'DESCRIPTION')}}.")

  scored <- tryCatch({
    refs <- riskmetric::pkg_ref(deps)
    assessed <- riskmetric::pkg_assess(tibble::as_tibble(refs), ...)
    riskmetric::pkg_score(assessed)
  }, error = function(e) {
    cli::cli_abort(c(
      "riskmetric could not assess the dependencies.",
      "x" = conditionMessage(e),
      "i" = "Check that the dependencies are installed and readable by riskmetric."))
  })

  cli::cli_alert_success("Scored {nrow(scored)} dependenc{?y/ies} with riskmetric.")
  as.data.frame(scored)
}
