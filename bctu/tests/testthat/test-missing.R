test_that("special_missing parses formulas and validates tags", {
  m <- special_missing(UNK ~ "a", "OTH" ~ "b")
  expect_s3_class(m, "bctu_special_missing")
  expect_equal(m$code, c("UNK", "OTH"))
  expect_equal(m$tag, c("a", "b"))

  expect_error(special_missing(UNK ~ "A"), "single lowercase")
  expect_error(special_missing(UNK ~ "ab"), "single lowercase")
  expect_error(special_missing(UNK ~ "a", OTH ~ "a"), "Duplicate tag")
  expect_error(special_missing(UNK ~ "a", UNK ~ "b"), "Duplicate code")
  expect_error(special_missing("not a formula"), "must be a formula")
})

test_that("apply_special_missing types a numeric column and tags the codes", {
  skip_if_not_installed("haven")
  df <- data.frame(age = c("52", "UNK", "61", "OTH"),
                   note = c("x", "UNK", "y", "z"),
                   stringsAsFactors = FALSE)
  m <- special_missing(UNK ~ "a", OTH ~ "b")
  out <- suppressWarnings(apply_special_missing(df, m, verbose = 0L))

  expect_true(is.double(out$age))
  expect_equal(out$age[c(1, 3)], c(52, 61))
  expect_equal(haven::na_tag(out$age), c(NA, "a", NA, "b"))
  # character column keeps the code text (special missing is numeric-only)
  expect_type(out$note, "character")
  expect_equal(out$note[2], "UNK")
})

test_that("apply_special_missing warns on a non-numeric field carrying a code", {
  m <- special_missing(UNK ~ "a")
  df <- data.frame(sex = c("F", "UNK", "M"), stringsAsFactors = FALSE)
  expect_warning(apply_special_missing(df, m), "character field")
})

test_that("restore_codes_frame and retag_upper round-trip", {
  skip_if_not_installed("haven")
  m <- special_missing(UNK ~ "a", OTH ~ "b")
  df <- suppressWarnings(apply_special_missing(
    data.frame(age = c("52", "UNK", "OTH"), stringsAsFactors = FALSE), m, verbose = 0L))

  restored <- bctu:::restore_codes_frame(df)
  expect_equal(restored$age, c("52", "UNK", "OTH"))

  up <- bctu:::retag_upper(df)
  expect_equal(haven::na_tag(up$age), c(NA, "A", "B"))
})

test_that("snapshot export writes dta with Stata special missings, csv with codes, and a SAS script", {
  skip_if_not_installed("haven")
  store <- withr::local_tempdir()
  m <- special_missing(UNK ~ "a")
  recs <- suppressWarnings(apply_special_missing(
    data.frame(record_id = c("E1", "E2", "E3"),
               age = c("50", "UNK", "62"), stringsAsFactors = FALSE), m, verbose = 0L))
  snap <- structure(list(records = recs),
                    class = c("bctu_snapshot", "list"),
                    bctu_meta = list(name = "DEMO"))

  save_snapshot(snap, store = store, formats = c("rds", "csv", "dta", "sas"), verbose = 0L)
  id  <- list_snapshots(store)
  tdir <- file.path(store, id, "tables", "records")
  csv  <- list.files(tdir, pattern = "\\.csv$", full.names = TRUE)
  dta  <- list.files(tdir, pattern = "\\.dta$", full.names = TRUE)
  sas  <- list.files(tdir, pattern = "_import\\.sas$", full.names = TRUE)
  expect_length(dta, 1L); expect_length(sas, 1L)

  # csv restores the original code
  csv_back <- utils::read.csv(csv, colClasses = "character")
  expect_equal(csv_back$age[2], "UNK")
  # dta carries the lowercase Stata special missing
  dta_back <- haven::read_dta(dta)
  expect_equal(haven::na_tag(dta_back$age), c(NA, "a", NA))
  # the SAS script recodes the coded column
  script <- paste(readLines(sas), collapse = "\n")
  expect_match(script, "proc import")
  expect_match(script, 'strip\\(age\\) = "UNK" then _bctu1 = .A')
})
