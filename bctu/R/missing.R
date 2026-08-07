# ---------------------------------------------------------------------------
# Missing-data codes (REDCap codes -> tagged NA -> SAS/Stata special missings)
# ---------------------------------------------------------------------------
# REDCap exports missing-data codes (e.g. "UNK", "OTH") as text, which coerces
# an otherwise-numeric column to character and loses its type. `special_missing()`
# declares a mapping from each code to a single-letter tag; `apply_special_missing()`
# restores the natural column type and stores the codes as haven tagged NAs on the
# numeric column. The snapshot export layer then writes them as native special
# missing values: Stata `.a` (lowercase tag) and SAS `.A` (uppercase tag), and
# always also emits a SAS import script as a reliable fallback.
#
# Canonical internal tag = a single lowercase letter a-z (haven normalises tags to
# lowercase on read; Stata uses lowercase; SAS uses the uppercase form, applied at
# export time). This is verified behaviour for haven 2.5.5.

#' Declare missing-data codes and the special-missing tags they map to
#'
#' Each argument is a formula `CODE ~ "tag"` mapping a REDCap (or other) missing
#' code to a single lowercase letter `a`-`z`. The left side may be written bare
#' (`UNK ~ "a"`) or quoted (`"UNK" ~ "a"`). The tag becomes a native special
#' missing value in exports: Stata `.a` and SAS `.A`.
#'
#' @param ... One or more `CODE ~ "tag"` formulas.
#' @return A `bctu_special_missing` mapping (a data frame of `code` and `tag`).
#' @examples
#' special_missing(UNK ~ "a", OTH ~ "b", NASK ~ "c")
#' @export
special_missing <- function(...) {
  dots <- list(...)
  if (!length(dots))
    cli::cli_abort("Declare at least one mapping, e.g. {.code special_missing(UNK ~ \"a\")}.")
  codes <- character(length(dots)); tags <- character(length(dots))
  for (i in seq_along(dots)) {
    f <- dots[[i]]
    if (!inherits(f, "formula") || length(f) != 3L)
      cli::cli_abort(c("Each mapping must be a formula {.code CODE ~ \"tag\"}.",
                       "x" = "Argument {i} is not."))
    lhs <- f[[2L]]; rhs <- f[[3L]]
    code <- if (is.symbol(lhs)) as.character(lhs) else if (is.character(lhs)) lhs else NA_character_
    tag  <- if (is.character(rhs)) rhs else if (is.symbol(rhs)) as.character(rhs) else NA_character_
    if (is.na(code) || !nzchar(code))
      cli::cli_abort("The left side of mapping {i} must be a code like {.code UNK}.")
    if (is.na(tag) || !grepl("^[a-z]$", tag))
      cli::cli_abort(c("The tag for code {.val {code}} must be a single lowercase letter a-z.",
                       "i" = "SAS/Stata special missing values are {.code .A}-{.code .Z} / {.code .a}-{.code .z}."))
    codes[i] <- code; tags[i] <- tag
  }
  if (anyDuplicated(codes)) cli::cli_abort("Duplicate code{?s}: {.val {codes[duplicated(codes)]}}.")
  if (anyDuplicated(tags))  cli::cli_abort("Duplicate tag{?s}: {.val {tags[duplicated(tags)]}}; each code needs a distinct tag.")
  structure(data.frame(code = codes, tag = tags, stringsAsFactors = FALSE),
            class = c("bctu_special_missing", "data.frame"))
}

#' @export
print.bctu_special_missing <- function(x, ...) {
  cli::cli_rule("missing-data codes")
  for (i in seq_len(nrow(x)))
    cli::cli_text("{.val {x$code[i]}} -> Stata {.code .{x$tag[i]}} / SAS {.code .{toupper(x$tag[i])}}")
  invisible(x)
}

#' Apply a missing-data code mapping to a character data frame
#'
#' For each column, cells whose (trimmed) value is a declared code are removed and
#' the remaining values are type-converted exactly as a CSV reader would, so the
#' column recovers its natural type. Numeric columns then carry the codes as haven
#' tagged NAs; non-numeric columns keep the code text (character) or drop it to a
#' plain NA (date fields), with a warning, because special missing values are a
#' numeric-only concept.
#'
#' @param records A data frame, read with every column as character.
#' @param mapping A [special_missing()] mapping, or `NULL` (returns `records`
#'   type-converted).
#' @param verbose Verbosity.
#' @return `records` with natural column types and tagged-NA special missings,
#'   carrying the mapping as `attr(., "bctu_special_missing")`.
#' @export
apply_special_missing <- function(records, mapping = NULL, verbose = 1L) {
  records <- as.data.frame(records, stringsAsFactors = FALSE)
  if (is.null(mapping)) {
    for (col in names(records)) records[[col]] <- convert_like_csv(as.character(records[[col]]))
    return(records)
  }
  if (!inherits(mapping, "bctu_special_missing"))
    cli::cli_abort("{.arg mapping} must be a {.fn special_missing} object or NULL.")
  if (!requireNamespace("haven", quietly = TRUE))
    cli::cli_abort(c("Package {.pkg haven} is required for missing-data codes.",
                     "i" = "Install it, or omit {.arg missing_codes}."))
  text_fields <- character(); date_fields <- character()
  for (col in names(records)) {
    raw <- as.character(records[[col]])
    hit <- match(trimws(raw), mapping$code)          # index into mapping, NA if not a code
    if (all(is.na(hit))) { records[[col]] <- convert_like_csv(raw); next }
    cleaned <- raw; cleaned[!is.na(hit)] <- NA
    conv <- convert_like_csv(cleaned)
    if (is.numeric(conv)) {
      conv <- as.double(conv)
      for (k in which(!is.na(hit))) conv[k] <- haven::tagged_na(mapping$tag[hit[k]])
      attr(conv, "bctu_codes") <- mapping[sort(unique(hit[!is.na(hit)])), , drop = FALSE]
      records[[col]] <- conv
    } else if (inherits(conv, c("Date", "POSIXct"))) {
      records[[col]] <- conv                          # coded cells already NA; date kept
      date_fields <- c(date_fields, col)
    } else {
      records[[col]] <- raw                           # character: keep the code text
      text_fields <- c(text_fields, col)
    }
  }
  if (verbose >= 1L && length(text_fields))
    cli::cli_warn(c("Missing-data codes kept as text in character field{?s}: {.val {unique(text_fields)}}.",
                    "i" = "Special missing values apply to numeric fields only."))
  if (verbose >= 1L && length(date_fields))
    cli::cli_warn(c("Missing-data codes dropped to plain NA in date field{?s}: {.val {unique(date_fields)}}.",
                    "i" = "A date column cannot carry a special missing value."))
  attr(records, "bctu_special_missing") <- mapping
  records
}

#' @keywords internal
convert_like_csv <- function(x) {
  if (requireNamespace("readr", quietly = TRUE))
    suppressWarnings(readr::parse_guess(x))
  else
    utils::type.convert(x, as.is = TRUE)
}

# --- export helpers ---------------------------------------------------------
#' @keywords internal
has_tagged_na <- function(v) {
  is.double(v) && requireNamespace("haven", quietly = TRUE) && any(haven::is_tagged_na(v))
}

#' Restore original codes into tagged-NA cells for a readable CSV copy
#' @keywords internal
restore_codes_frame <- function(tbl) {
  map <- attr(tbl, "bctu_special_missing")
  if (is.null(map) || !requireNamespace("haven", quietly = TRUE)) return(tbl)
  for (col in names(tbl)) {
    v <- tbl[[col]]
    if (!has_tagged_na(v)) next
    tg  <- haven::na_tag(v)
    out <- format(v, trim = TRUE, scientific = FALSE)
    out[is.na(v) & is.na(tg)] <- NA
    for (i in which(!is.na(tg))) {
      code <- map$code[map$tag == tg[i]]
      out[i] <- if (length(code)) code[1L] else NA
    }
    tbl[[col]] <- out
  }
  tbl
}

#' Re-tag tagged NAs to their uppercase form (SAS special missings)
#' @keywords internal
retag_upper <- function(tbl) {
  if (!requireNamespace("haven", quietly = TRUE)) return(tbl)
  for (col in names(tbl)) {
    v <- tbl[[col]]
    if (!has_tagged_na(v)) next
    tg <- haven::na_tag(v)
    for (i in which(!is.na(tg))) v[i] <- haven::tagged_na(toupper(tg[i]))
    tbl[[col]] <- v
  }
  tbl
}

#' Which columns carry special-missing codes?
#' @keywords internal
coded_columns <- function(tbl) names(tbl)[vapply(tbl, has_tagged_na, logical(1))]

#' Generate a SAS import script that reads the CSV and applies special missings
#'
#' The reliable path: haven's SAS writer is unstable, so the snapshot always ships
#' a `.sas` script that PROC IMPORTs the readable CSV (which holds the original
#' codes) and recodes each coded column to native SAS special missing values.
#' @keywords internal
sas_import_script <- function(tbl, csv_name, dataset) {
  map   <- attr(tbl, "bctu_special_missing")
  coded <- coded_columns(tbl)
  L <- c(
    "/* Auto-generated by bctu. Imports the snapshot CSV and applies SAS special */",
    "/* missing values. Run in SAS from the folder containing the CSV.           */",
    "",
    sprintf("proc import datafile=\"%s\" out=work.%s dbms=csv replace;", csv_name, dataset),
    "  getnames=yes; guessingrows=max;",
    "run;"
  )
  if (length(coded) && !is.null(map)) {
    L <- c(L, "", sprintf("data work.%s;", dataset), sprintf("  set work.%s;", dataset))
    for (col in coded) {
      nv <- paste0("_bctu_", col)
      L <- c(L, sprintf("  length %s 8;", nv))
      for (j in seq_len(nrow(map)))
        L <- c(L, sprintf("  %sif strip(%s) = \"%s\" then %s = .%s;",
                          if (j == 1L) "" else "else ", col, map$code[j], nv, toupper(map$tag[j])))
      L <- c(L, sprintf("  else %s = input(%s, ?? best32.);", nv, col),
                sprintf("  drop %s; rename %s = %s;", col, nv, col))
    }
    L <- c(L, "run;")
  }
  paste0(paste(L, collapse = "\n"), "\n")
}
