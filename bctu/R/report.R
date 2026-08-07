# ---------------------------------------------------------------------------
# Reports: explicit inputs in -> document + provenance out
# ---------------------------------------------------------------------------
# A report is built from EXPLICIT section objects passed in directly. Nothing is
# read from the global environment: the "jank" being removed is the old habit of
# `source()`-ing chapters into globals and having an .Rmd read them back, which
# is order-dependent and impossible to unit-test. Here `sections` is an ordered
# named list of content objects (a heading, a paragraph, a report table, or a
# figure), and the document is assembled from exactly those objects.
#
# Rendering targets a format-specific pandoc-markdown document: text, headings
# and figures are the same in every format; tables differ (a grid table for the
# DOCX path, a raw-LaTeX table for the PDF path) but come from the SAME
# `report_table` object. Every rendered bundle is stamped with a human-readable
# provenance manifest (`report-manifest.yml`).

# --- section content objects -----------------------------------------------
#' A heading section
#' @param text Heading text.
#' @param level Heading level (1 = top). Default 1.
#' @return A `bctu_report_heading` section object.
#' @export
report_heading <- function(text, level = 1L) {
  if (!is_string(text)) cli::cli_abort("{.arg text} must be a single string.")
  if (!is.numeric(level) || length(level) != 1L || level < 1L)
    cli::cli_abort("{.arg level} must be a single positive number.")
  structure(list(type = "heading", text = text, level = as.integer(level)),
            class = c("bctu_report_heading", "bctu_report_section"))
}

#' A paragraph (markdown/plain text) section
#' @param text Paragraph text (markdown is allowed).
#' @return A `bctu_report_paragraph` section object.
#' @export
report_paragraph <- function(text) {
  if (!is_string(text)) cli::cli_abort("{.arg text} must be a single string.")
  structure(list(type = "paragraph", text = text),
            class = c("bctu_report_paragraph", "bctu_report_section"))
}

#' A figure section (from an image file or a ggplot object)
#'
#' Supply either a `path` to an existing image, or a `plot` object (for example
#' a ggplot), which is saved to a PNG when the report is rendered.
#' @param path Path to an existing image file, or `NULL`.
#' @param plot A plot object (for example a ggplot), or `NULL`.
#' @param caption Optional caption.
#' @param width Display width as a fraction of the text width (0-1). Default 0.8.
#' @param dpi Resolution used when saving a `plot`. Default 150.
#' @return A `bctu_report_figure` section object.
#' @export
report_figure <- function(path = NULL, plot = NULL, caption = NULL,
                          width = 0.8, dpi = 150) {
  if (is.null(path) && is.null(plot))
    cli::cli_abort("Give {.arg path} to an image or a {.arg plot} object.")
  if (!is.null(path) && !is.null(plot))
    cli::cli_abort("Give only one of {.arg path} or {.arg plot}.")
  structure(list(type = "figure", path = path, plot = plot, caption = caption,
                 width = width, dpi = dpi),
            class = c("bctu_report_figure", "bctu_report_section"))
}

# --- the report spec -------------------------------------------------------
#' Assemble a report from explicit sections
#'
#' The report is defined entirely by the objects you pass in; nothing is read
#' from the global environment, so a report is reproducible and unit-testable.
#' @param title The report title (a single string).
#' @param sections An ordered, named list of section objects: [report_heading()],
#'   [report_paragraph()], a [report_table()], or [report_figure()].
#' @param meta Optional named list of extra metadata to record (for example the
#'   author or trial name); stored with the report and written to the manifest.
#' @return A `bctu_report` object.
#' @export
bctu_report <- function(title, sections, meta = list()) {
  if (!is_string(title)) cli::cli_abort("{.arg title} must be a single string.")
  if (!is.list(sections) || !length(sections))
    cli::cli_abort("{.arg sections} must be a non-empty list of section objects.")
  ok <- vapply(sections, function(s)
    inherits(s, "bctu_report_section") || inherits(s, "bctu_report_table"),
    logical(1))
  if (!all(ok))
    cli::cli_abort(c("Every entry in {.arg sections} must be a section or a report table.",
                     "x" = "Not valid: position{?s} {.val {which(!ok)}}.",
                     "i" = "Use {.fn report_heading}, {.fn report_paragraph}, {.fn report_table} or {.fn report_figure}."))
  if (!is.list(meta)) cli::cli_abort("{.arg meta} must be a list.")
  structure(list(title = title, sections = sections, meta = meta),
            class = c("bctu_report", "list"))
}

#' @export
print.bctu_report <- function(x, ...) {
  cli::cli_rule("bctu report {.val {x$title}}")
  types <- vapply(x$sections, function(s)
    if (inherits(s, "bctu_report_table")) "table" else s$type, character(1))
  cli::cli_text("{length(x$sections)} section{?s}: {types}")
  invisible(x)
}

# --- rendering -------------------------------------------------------------
#' Render a report to files, with a provenance manifest
#'
#' Renders the report to each requested format in `output_dir`, copies the whole
#' bundle to any `extra_destinations`, and writes a human-readable provenance
#' manifest (`report-manifest.yml`) recording the snapshot, data-cut date, tool
#' versions, template identity, and the SHA-256 of every output file.
#'
#' @param report A `bctu_report` from [bctu_report()].
#' @param output_dir Directory to render into (created if needed).
#' @param formats Output formats: any of `"docx"`, `"pdf"`. Default both.
#' @param snapshot Optional `bctu_snapshot` (or a snapshot id string) the report
#'   is built from; recorded in the manifest for provenance.
#' @param template Optional path to a Word reference document (`.docx`) used for
#'   DOCX styling; its identity and SHA-256 are recorded.
#' @param extra_destinations Optional character vector of directories to also
#'   copy the whole rendered bundle into.
#' @param verbose Verbosity.
#' @return Invisibly, a list describing the render (output paths and manifest).
#' @export
render_report <- function(report, output_dir,
                          formats = c("docx", "pdf"),
                          snapshot = NULL,
                          template = NULL,
                          extra_destinations = NULL,
                          verbose = 2L) {
  if (!inherits(report, "bctu_report"))
    cli::cli_abort("{.arg report} must be a {.cls bctu_report}.")
  formats <- match.arg(formats, c("docx", "pdf"), several.ok = TRUE)
  pandoc <- Sys.which("pandoc")
  if (!nzchar(pandoc))
    cli::cli_abort(c("pandoc was not found on the PATH.",
                     "i" = "Install pandoc to render reports."))
  if (!is.null(template) && !file.exists(template))
    cli::cli_abort("Reference document not found: {.file {template}}.")

  if (!dir.exists(output_dir))
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  output_dir <- normalizePath(output_dir, winslash = "/")

  now  <- utc_now()
  ver  <- tryCatch(as.character(utils::packageVersion("bctu")),
                   error = function(e) "dev")
  base_name <- paste0(make_filename_slug(report$title), "-",
                      snapshot_id(now), "-v", ver)

  work <- file.path(tempdir(), paste0("bctu-report-", snapshot_id(now)))
  dir.create(work, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(work, recursive = TRUE, force = TRUE), add = TRUE)

  outputs <- list()
  start <- Sys.time()
  for (fmt in formats) {
    md_path <- file.path(work, paste0(base_name, "-", fmt, ".md"))
    writeLines(build_report_markdown(report, fmt, work), md_path)
    out_path <- file.path(output_dir, paste0(base_name, ".", fmt))
    run_pandoc(pandoc, md_path, out_path, fmt, template)
    if (!file.exists(out_path) || file.info(out_path)$size == 0)
      cli::cli_abort("pandoc produced no {fmt} output at {.file {out_path}}.")
    outputs[[fmt]] <- out_path
    if (verbose >= 1L)
      cli::cli_alert_success("rendered {.strong {fmt}} -> {.file {out_path}}")
  }
  render_seconds <- round(as.numeric(difftime(Sys.time(), start, units = "secs")), 2)

  manifest_path <- file.path(output_dir, "report-manifest.yml")
  manifest <- build_report_manifest(report, outputs, snapshot, template,
                                    formats, ver, now, render_seconds)
  yaml::write_yaml(manifest, manifest_path)
  if (verbose >= 1L)
    cli::cli_alert_success("manifest -> {.file {manifest_path}}")

  bundle <- c(unlist(outputs, use.names = FALSE), manifest_path)
  for (dest in extra_destinations %||% character(0)) {
    if (!dir.exists(dest)) dir.create(dest, recursive = TRUE, showWarnings = FALSE)
    file.copy(bundle, dest, overwrite = TRUE)
    if (verbose >= 1L) cli::cli_alert_info("copied bundle -> {.file {dest}}")
  }

  invisible(list(base_name = base_name, outputs = outputs,
                 manifest = manifest_path, extra_destinations = extra_destinations))
}

#' Run pandoc for one output format
#' @keywords internal
run_pandoc <- function(pandoc, md_path, out_path, fmt, template) {
  args <- c(shQuote(md_path), "--from", "markdown",
            "-o", shQuote(out_path), "--standalone")
  if (fmt == "pdf")
    args <- c(args, "--pdf-engine=xelatex")
  if (fmt == "docx" && !is.null(template))
    args <- c(args, paste0("--reference-doc=", shQuote(template)))
  status <- system2(pandoc, args, stdout = TRUE, stderr = TRUE)
  code <- attr(status, "status")
  if (!is.null(code) && code != 0)
    cli::cli_abort(c("pandoc failed rendering {fmt}.",
                     "x" = paste(status, collapse = "\n")))
  invisible(out_path)
}

# --- document assembly (explicit sections -> pandoc markdown) --------------
#' Assemble the full pandoc-markdown document for one format
#' @keywords internal
build_report_markdown <- function(report, format, assets_dir) {
  yaml_header <- c("---",
                   paste0("title: ", yaml_quote(report$title)),
                   "---", "")
  blocks <- vapply(seq_along(report$sections), function(i)
    render_section(report$sections[[i]], format, assets_dir, i),
    character(1))
  paste(c(yaml_header, blocks), collapse = "\n\n")
}

#' Render one section to markdown for the given format
#' @keywords internal
render_section <- function(section, format, assets_dir, index) {
  if (inherits(section, "bctu_report_table"))
    return(render_table_section(section, format))
  switch(section$type,
    heading   = paste0(strrep("#", section$level), " ", section$text),
    paragraph = section$text,
    figure    = render_figure_section(section, assets_dir, index),
    cli::cli_abort("Unknown section type: {.val {section$type}}."))
}

#' Render a table section: grid table for DOCX, raw LaTeX for PDF
#' @keywords internal
render_table_section <- function(tbl, format) {
  if (format == "pdf")
    return(paste0("```{=latex}\n", render_table_latex(tbl), "\n```"))
  render_table_markdown(tbl)
}

#' Render a figure section (saving a plot object to PNG if needed)
#' @keywords internal
render_figure_section <- function(fig, assets_dir, index) {
  path <- fig$path
  if (is.null(path)) {
    path <- file.path(assets_dir, paste0("figure-", index, ".png"))
    if (!requireNamespace("ggplot2", quietly = TRUE))
      cli::cli_abort("Saving a plot needs the {.pkg ggplot2} package.")
    ggplot2::ggsave(path, plot = fig$plot, width = 6, height = 4, dpi = fig$dpi)
  } else {
    dest <- file.path(assets_dir, basename(path))
    file.copy(path, dest, overwrite = TRUE)
    path <- dest
  }
  cap <- fig$caption %||% ""
  pct <- paste0(round(fig$width * 100), "%")
  paste0("![", cap, "](", basename(path), "){width=", pct, "}")
}

# --- provenance manifest ---------------------------------------------------
#' Build the provenance manifest for a rendered report
#' @keywords internal
build_report_manifest <- function(report, outputs, snapshot, template,
                                   formats, bctu_version, now, render_seconds) {
  snap_block <- list(id = NULL, sha256 = NULL, data_cut_date = NULL)
  if (inherits(snapshot, "bctu_snapshot")) {
    id <- attr(snapshot, "id")
    snap_block <- drop_null(list(
      id = id %||% "unsaved",
      tag = attr(snapshot, "bctu_tag") %||% attr(snapshot, "bctu_meta")$tag,
      sha256 = tryCatch(snapshot_fingerprint(snapshot),
                        error = function(e) NA_character_),
      data_cut_date = if (!is.null(id)) as.character(snapshot_date(id)) else NA_character_))
  } else if (is_string(snapshot)) {
    snap_block <- list(
      id = snapshot, sha256 = NA_character_,
      data_cut_date = tryCatch(as.character(snapshot_date(snapshot)),
                               error = function(e) NA_character_))
  }

  template_block <- if (is.null(template))
    list(identity = "none (pandoc default)", sha256 = NA_character_)
  else
    list(identity = basename(template), path = normalizePath(template, winslash = "/"),
         sha256 = sha256_file(template))

  outputs_block <- lapply(names(outputs), function(fmt) {
    p <- outputs[[fmt]]
    list(format = fmt, file = basename(p),
         size_bytes = as.integer(file.info(p)$size),
         sha256 = sha256_file(p))
  })
  names(outputs_block) <- names(outputs)

  section_types <- vapply(report$sections, function(s)
    if (inherits(s, "bctu_report_table")) "table" else s$type, character(1))

  list(
    schema        = "bctu-report/1",
    report_title  = report$title,
    created_utc   = iso8601(now),
    render_seconds = render_seconds,
    versions = list(
      bctu   = bctu_version,
      r      = R.version.string,
      pandoc = tool_version("pandoc", "--version"),
      xelatex = if ("pdf" %in% formats) tool_version("xelatex", "--version") else "not used (no PDF)"
    ),
    template   = template_block,
    snapshot   = snap_block,
    meta       = report$meta,
    sections   = as.list(section_types),
    outputs    = outputs_block,
    rendered_by = list(user = unname(Sys.info()[["user"]]) %||% "unknown",
                       host = unname(Sys.info()[["nodename"]]) %||% "unknown")
  )
}

#' First line of a command-line tool's version output
#' @keywords internal
tool_version <- function(cmd, flag) {
  exe <- Sys.which(cmd)
  if (!nzchar(exe)) return("not found")
  out <- tryCatch(system2(exe, flag, stdout = TRUE, stderr = TRUE),
                  error = function(e) character(0))
  if (!length(out)) "unknown" else trimws(out[[1]])
}

# --- small helpers ---------------------------------------------------------
#' A filesystem-safe slug from a title (lowercase, dashes, no punctuation)
#' @keywords internal
make_filename_slug <- function(title) {
  s <- tolower(title)
  s <- gsub("[^a-z0-9]+", "-", s)
  s <- gsub("(^-+|-+$)", "", s)
  if (!nzchar(s)) "report" else s
}

#' Quote a string for a YAML metadata value
#' @keywords internal
yaml_quote <- function(s) paste0("\"", gsub("\"", "\\\\\"", s), "\"")
