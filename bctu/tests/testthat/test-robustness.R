# Regression tests for the adversarial-campaign findings (2026-08-07).

# --- checks: workbook sheet dedup must terminate (was an infinite loop) -------
test_that("unique_sheet_name terminates and stays <= 31 chars for 31-char clashes", {
  base <- strrep("a", 31L)                       # already at the Excel limit
  used <- base
  for (i in 1:5) {
    nm <- bctu:::unique_sheet_name(base, used)
    expect_false(nm %in% used)
    expect_lte(nchar(nm), 31L)
    used <- c(used, nm)
  }
  expect_equal(length(unique(used)), 6L)
  expect_true(nzchar(bctu:::unique_sheet_name("", character(0))))   # empty -> fallback
})

test_that("write_findings_workbook does not hang on two check names sharing 31 chars", {
  skip_if_not_installed("openxlsx")
  long1 <- paste0(strrep("check_", 5L), "one")   # > 31 chars, share first 31
  long2 <- paste0(strrep("check_", 5L), "two")
  sheets <- setNames(list(data.frame(x = 1), data.frame(x = 2)), c(long1, long2))
  p <- tempfile(fileext = ".xlsx")
  expect_no_error(write_findings_workbook(sheets, p))
  expect_true(file.exists(p))
})

# --- checks: compare_dvp must not clobber a check's own status column ---------
test_that("compare_dvp errors if a check returns a reserved 'status' column", {
  dvp <- function(data) list(q = data.frame(id = 1, status = "open"))
  d <- data.frame(id = 1)
  expect_error(compare_dvp(dvp, d, d), "reserved column")
})

# --- missing: full numeric precision in the restored CSV ----------------------
test_that("restore_codes_frame keeps full double precision (not 7 sig digits)", {
  skip_if_not_installed("haven")
  mp <- special_missing(UNK ~ "a")
  df <- apply_special_missing(
    data.frame(v = c("0.123456789012345", "UNK", "1234567.891")), mp, verbose = 0L)
  out <- bctu:::restore_codes_frame(df)
  expect_equal(out$v[1], "0.123456789012345")
  expect_equal(out$v[2], "UNK")
  expect_false(grepl("e", out$v[3], fixed = TRUE))
})

# --- missing: code content is validated (keeps generated SAS well-formed) -----
test_that("special_missing rejects codes with quotes / newlines / control chars", {
  expect_error(special_missing(`a"b` ~ "a"), "quote|control|newline")
  expect_error(special_missing("a\nb" ~ "a"), "quote|control|newline")
})

test_that("generated SAS uses short temp names even for very long column names", {
  skip_if_not_installed("haven")
  mp <- special_missing(UNK ~ "a")
  long <- paste0("a_very_long_redcap_field_name_", strrep("x", 20L))
  df <- apply_special_missing(setNames(data.frame(c("1", "UNK")), long), mp, verbose = 0L)
  script <- bctu:::sas_import_script(df, "f.csv", "d")
  expect_match(script, "_bctu1", fixed = TRUE)
  expect_false(grepl("_bctu_a_very_long", script, fixed = TRUE))
})

# --- table: duplicate column names, span, align ------------------------------
test_that("report_table rejects duplicate displayed column names (no silent mis-map)", {
  df <- data.frame(x = 1:2, y = 3:4); names(df) <- c("x", "x")
  expect_error(report_table(df), "duplicate")
})

test_that("group_headers with span < 1 is rejected (was garbled pandoc output)", {
  df <- data.frame(a = 1, b = 2)
  expect_error(report_table(df, group_headers = c(G = 0, H = 2)), "at least 1")
})

test_that("invalid align values are rejected, not silently dropped", {
  df <- data.frame(a = 1:2, b = 1:2, c = 1:2)
  expect_error(report_table(df, align = c("right", "bogus", "left")), "left.*right.*center|Invalid")
})

# --- snapshot: only-failing-format must not leave a broken snapshot -----------
test_that("a snapshot whose only requested format fails errors and leaves nothing", {
  skip_if_not_installed("haven")
  store <- withr::local_tempdir()
  ds <- new_datasource("rw", fetch = function(creds, config, verbose = 0L, ...)
    list(t = data.frame(int = 1:3)))          # 'int' is a Stata reserved word -> dta fails
  # the failed-dta warning is expected graceful degradation; the abort is the point
  expect_error(suppressWarnings(
    take_snapshot(ds, store = store, formats = "dta", git = "off", verbose = 0L)),
    "No payload")
  expect_length(list_snapshots(store), 0L)
})

# --- snapshot: destroy always leaves a committed audit tombstone --------------
test_that("destroy leaves a committed deletion tombstone even if the save was never committed", {
  skip_if(!nzchar(Sys.which("git")), "git not available")
  repo <- withr::local_tempdir()
  run <- function(...) system2("git", c("-C", repo, ...), stdout = FALSE, stderr = FALSE)
  run("init", "-q"); run("config", "user.email", "t@b"); run("config", "user.name", "t")
  writeLines("x", file.path(repo, "README")); run("add", "README"); run("commit", "-qm", "init")
  store <- file.path(repo, "Data", "Snapshots"); dir.create(store, recursive = TRUE)
  s <- take_snapshot(datasource_example("redcap", n = 4L), store = store,
                     git = "off", verbose = 0L)   # NOT committed on save
  delete_snapshot(attr(s, "id"), reason = "destroyed", store = store,
                  mode = "destroy", verbose = 0L)
  msg <- system2("git", c("-C", repo, "log", "-1", "--pretty=%s"), stdout = TRUE)
  expect_true(grepl("delete snapshot", msg, fixed = TRUE))
  expect_true(file.exists(file.path(store, "_deleted", attr(s, "id"), "deletion-note.yml")))
  expect_false(dir.exists(file.path(store, attr(s, "id"))))   # payload gone
})

# --- snapshot: a store that is a file gives an accurate error -----------------
test_that("saving into a store path that is a file reports the real problem", {
  bad <- tempfile(); writeLines("i am a file", bad)
  ds <- new_datasource("f", fetch = function(creds, config, verbose = 0L, ...)
    list(t = data.frame(x = 1)))
  expect_error(take_snapshot(ds, store = bad, git = "off", verbose = 0L),
               "Could not create|directory|writable")
})

# --- utils: parse_snapshot_id is strict --------------------------------------
test_that("parse_snapshot_id rejects out-of-range times instead of rolling over", {
  expect_error(parse_snapshot_id("2020-01-01T256000Z"), "not a real UTC time|Invalid")
  expect_error(parse_snapshot_id("2020-01-01T235960Z"), "not a real UTC time")
  expect_s3_class(parse_snapshot_id("2020-01-01T235959Z"), "POSIXct")
})

# --- datasource: duplicate table names are rejected --------------------------
test_that("a source returning duplicate table names is rejected", {
  ds <- new_datasource("dup", fetch = function(creds, config, verbose = 0L, ...)
    setNames(list(data.frame(x = 1), data.frame(y = 2)), c("t", "t")))
  store <- withr::local_tempdir()
  expect_error(take_snapshot(ds, store = store, git = "off", verbose = 0L), "duplicate table")
})

# --- sql: server/database reject connection-string metacharacters ------------
test_that("datasource_sql rejects ; and = in server/database (connection-string safety)", {
  conn <- sql_connection()
  expect_error(datasource_sql("host;EVIL=1", "db", tables = "t", conn = conn), "may not contain")
  expect_error(datasource_sql("host", "db;DROP", tables = "t", conn = conn), "may not contain")
})
