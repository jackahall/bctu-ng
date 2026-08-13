test_that("run_dvp rejects anything that is not a named list", {
  expect_error(run_dvp("not a function", list()), "must be a function")
  expect_error(run_dvp(function(data) data.frame(x = 1), NULL), "named list")
  expect_error(run_dvp(function(data) list(1, 2), NULL), "named list")
  expect_error(run_dvp(function(data) list(a = 1, a = 2), NULL), "named list")
})

test_that("run_dvp keeps a named list and normalises NULL elements to empty frames", {
  dvp <- function(data) list(
    always_empty = NULL,
    one_row      = data.frame(record_id = "E001", note = "x"))
  out <- run_dvp(dvp, NULL)
  expect_named(out, c("always_empty", "one_row"))
  expect_s3_class(out$always_empty, "data.frame")
  expect_equal(nrow(out$always_empty), 0L)
  expect_equal(nrow(out$one_row), 1L)
})

test_that("run_dvp rejects a check element that is not a data frame", {
  expect_error(run_dvp(function(data) list(bad = 1:3), NULL), "data frame")
})

test_that("compare_dvp labels findings new / unchanged / resolved whole-row", {
  before <- data.frame(record_id = c("E001", "E002", "E004"),
                       reason = c("r1", "r2", "r4"))
  after  <- data.frame(record_id = c("E001", "E002", "E003"),
                       reason = c("r1", "rX", "r3"))
  dvp <- function(data) list(demo = data)

  cmp <- compare_dvp(dvp, before, after)$demo
  status_of <- function(rid) cmp$status[cmp$record_id == rid]
  expect_equal(status_of("E001"), "unchanged")           # identical row
  expect_equal(sort(status_of("E002")), c("new", "resolved"))  # reason changed -> row differs
  expect_equal(status_of("E003"), "new")
  expect_equal(status_of("E004"), "resolved")
})

test_that("compare_dvp handles a check present in only one run", {
  dvp <- function(data) {
    if (identical(data, "after")) list(only_after = data.frame(id = 1))
    else list(only_before = data.frame(id = 9))
  }
  cmp <- compare_dvp(dvp, before = "before", after = "after")
  expect_setequal(names(cmp), c("only_after", "only_before"))
  expect_equal(cmp$only_after$status, "new")
  expect_equal(cmp$only_before$status, "resolved")
})

test_that("save_dvr writes a full set and an auditable manifest, trial name from the snapshot", {
  store   <- withr::local_tempdir()
  out_dir <- withr::local_tempdir()

  lo <- 65; hi <- 90
  dvp <- function(data) {
    r <- data$records
    bad <- is.na(r$weight_kg) | r$weight_kg < lo | r$weight_kg > hi
    list(weight_range = data.frame(record_id = r$record_id[bad],
                                   value = r$weight_kg[bad],
                                   reason = "weight out of range"))
  }

  snap <- take_snapshot(datasource_example("redcap", n = 40L, seed = 1L, name = "DEMO"),
                        store = store, verbose = 0L)
  res <- save_dvr(dvp, snap, paths = out_dir, operator = "tester",
                  write_xlsx = FALSE, verbose = 0L)

  expect_length(res$dirs, 1L)
  expect_true(dir.exists(res$dirs[[1]]))
  expect_true(file.exists(file.path(res$dirs[[1]], "full", "weight_range.csv")))
  expect_false(res$compared)
  expect_equal(res$trial, "DEMO")

  man <- yaml::read_yaml(file.path(res$dirs[[1]], "manifest.yml"))
  expect_equal(man$schema, "bctu-dvr/1")
  expect_equal(man$trial, "DEMO")
  expect_equal(man$after_snapshot$id, attr(snap, "id"))
  expect_null(man$before_snapshot)
})

test_that("save_dvr with a before snapshot records the comparison and an update set", {
  store   <- withr::local_tempdir()
  out_dir <- withr::local_tempdir()

  dvp <- function(data) {
    hit <- data$records$record_id[data$records$outcome == 1]
    list(outcome_positive = data.frame(record_id = hit,
                                       reason = rep("outcome positive", length(hit))))
  }

  snapA <- take_snapshot(datasource_example("redcap", n = 40L, seed = 1L),
                         store = store, verbose = 0L)
  Sys.sleep(1.1)
  snapB <- take_snapshot(datasource_example("redcap", n = 40L, seed = 3L),
                         store = store, verbose = 0L)

  res <- save_dvr(dvp, after = snapB, before = snapA, paths = out_dir,
                  operator = "tester", write_xlsx = FALSE, verbose = 0L)
  expect_true(res$compared)
  expect_true("status" %in% names(res$sheets$outcome_positive))
  expect_true(dir.exists(file.path(res$dirs[[1]], "update")))

  man <- yaml::read_yaml(file.path(res$dirs[[1]], "manifest.yml"))
  expect_true(man$compared)
  expect_equal(man$before_snapshot$id, attr(snapA, "id"))
  expect_equal(man$after_snapshot$id, attr(snapB, "id"))
})

test_that("save_dvr writes to every path in paths", {
  store <- withr::local_tempdir()
  out1  <- withr::local_tempdir()
  out2  <- withr::local_tempdir()

  dvp <- function(data) list(all_records =
    data.frame(record_id = data$records$record_id))

  snap <- take_snapshot(datasource_example("redcap", n = 10L, seed = 1L),
                        store = store, verbose = 0L)
  res <- save_dvr(dvp, snap, paths = c(out1, out2), write_xlsx = FALSE, verbose = 0L)

  expect_length(res$dirs, 2L)
  expect_true(all(vapply(res$dirs, dir.exists, logical(1))))
  expect_true(file.exists(file.path(res$dirs[[1]], "full", "all_records.csv")))
  expect_true(file.exists(file.path(res$dirs[[2]], "full", "all_records.csv")))
})

test_that("save_dvr splits per site plus overall when site_col is given", {
  skip_if_not_installed("openxlsx")
  store   <- withr::local_tempdir()
  out_dir <- withr::local_tempdir()

  snap <- take_snapshot(datasource_example("redcap", n = 20L, seed = 1L),
                        store = store, verbose = 0L)
  snap$records$site <- rep(c("Site_A", "Site_B"), length.out = nrow(snap$records))

  dvp <- function(data) list(all_records =
    data.frame(record_id = data$records$record_id))

  res <- save_dvr(dvp, snap, paths = out_dir, id_col = "record_id",
                  site_col = "site", write_xlsx = TRUE, verbose = 0L)

  full <- file.path(res$dirs[[1]], "full")
  expect_true(length(list.files(full, pattern = "\\.xlsx$")) >= 1L)   # overall
  expect_true(dir.exists(file.path(full, "sites", "Site_A")))
  expect_true(dir.exists(file.path(full, "sites", "Site_B")))
})

test_that("a resolved finding whose record was removed from after is still sited from before", {
  out_dir <- withr::local_tempdir()

  before <- list(records = data.frame(
    record_id = c("E001", "E002"), site = c("Site_A", "Site_B"),
    stringsAsFactors = FALSE))
  after <- list(records = data.frame(
    record_id = "E002", site = "Site_B", stringsAsFactors = FALSE))  # E001 withdrawn

  dvp <- function(data) list(demo = data.frame(
    record_id = data$records$record_id, note = "issue", stringsAsFactors = FALSE))

  res <- save_dvr(dvp, after = after, before = before, paths = out_dir,
                  id_col = "record_id", site_col = "site",
                  write_xlsx = FALSE, verbose = 0L)

  full <- file.path(res$dirs[[1]], "full")
  site_a_csv <- file.path(full, "sites", "Site_A", "demo.csv")
  expect_true(file.exists(site_a_csv))
  rows <- utils::read.csv(site_a_csv, stringsAsFactors = FALSE)
  expect_true("E001" %in% rows$record_id)
  expect_false(dir.exists(file.path(full, "sites", "NO_SITE")))
})

test_that("write_findings_readable de-duplicates filenames that sanitise to the same stem", {
  dir <- withr::local_tempdir()
  sheets <- list(
    `a/b` = data.frame(x = 1L),
    `a b` = data.frame(x = 2L))

  write_findings_readable(sheets, dir)

  csvs <- sort(list.files(dir, pattern = "\\.csv$"))
  expect_length(csvs, 2L)
  first  <- utils::read.csv(file.path(dir, csvs[1]))
  second <- utils::read.csv(file.path(dir, csvs[2]))
  expect_false(identical(first$x, second$x))
})

test_that("finding_row_keys distinguishes a genuine NA from the literal string \"NA\"", {
  df <- data.frame(value = c(NA_character_, "NA"), stringsAsFactors = FALSE)
  keys <- finding_row_keys(df)
  expect_false(keys[1] == keys[2])
})

test_that("write_report_set warns per check with findings that cannot be mapped to a site", {
  dir <- withr::local_tempdir()
  snapshot <- list(records = data.frame(
    record_id = "E001", site = "Site_A", stringsAsFactors = FALSE))
  sheets <- list(demo = data.frame(
    record_id = "E999", note = "orphan", stringsAsFactors = FALSE))  # not in snapshot

  expect_warning(
    write_report_set(sheets, snapshot, dir, "base", id_col = "record_id",
                     site_col = "site", write_xlsx = FALSE),
    "could not be mapped to a site")
})

test_that("check_info writes the index, prepends the query column, and reaches the manifest", {
  store   <- withr::local_tempdir()
  out_dir <- withr::local_tempdir()

  dvp <- function(data) {
    r <- data$records
    bad <- is.na(r$weight_kg) | r$weight_kg < 65 | r$weight_kg > 90
    list(weight_range = data.frame(record_id = r$record_id[bad]),
         never_fires  = data.frame())
  }
  info <- data.frame(
    check    = c("weight_range", "never_fires"),
    query    = c("Please confirm the recorded weight.",
                 "Please confirm the visit date."),
    section  = c("Baseline", "Baseline"),
    critical = c(TRUE, FALSE))

  snap <- take_snapshot(datasource_example("redcap", n = 40L, seed = 1L),
                        store = store, verbose = 0L)
  res <- save_dvr(dvp, snap, paths = out_dir, check_info = info,
                  write_xlsx = FALSE, verbose = 0L)

  full <- file.path(res$dirs[[1]], "full")
  idx <- utils::read.csv(file.path(full, "checks_index.csv"))
  expect_equal(idx$check, info$check)   # all checks listed, all-clear included
  expect_true(file.exists(file.path(full, "checks_index.txt")))

  found <- utils::read.csv(file.path(full, "weight_range.csv"))
  expect_gt(nrow(found), 0L)
  expect_equal(names(found)[1], "query")
  expect_equal(unique(found$query), "Please confirm the recorded weight.")

  man <- yaml::read_yaml(file.path(res$dirs[[1]], "manifest.yml"))
  wr <- Filter(function(chk) chk$name == "weight_range", man$checks)[[1]]
  expect_equal(wr$query, "Please confirm the recorded weight.")
  expect_equal(wr$section, "Baseline")
  expect_true(wr$critical)
})

test_that("check_info attr fallback works and the explicit argument wins", {
  store   <- withr::local_tempdir()
  out_dir <- withr::local_tempdir()

  info_attr <- data.frame(check = "all_records", query = "Attribute text.")
  dvp <- function(data) {
    findings <- list(all_records = data.frame(record_id = data$records$record_id))
    attr(findings, "check_info") <- info_attr
    findings
  }
  snap <- take_snapshot(datasource_example("redcap", n = 10L, seed = 1L),
                        store = store, verbose = 0L)

  res <- save_dvr(dvp, snap, paths = out_dir, write_xlsx = FALSE, verbose = 0L)
  found <- utils::read.csv(file.path(res$dirs[[1]], "full", "all_records.csv"))
  expect_equal(unique(found$query), "Attribute text.")

  out2 <- withr::local_tempdir()
  info_arg <- data.frame(check = "all_records", query = "Argument text.")
  res2 <- save_dvr(dvp, snap, paths = out2, check_info = info_arg,
                   write_xlsx = FALSE, verbose = 0L)
  found2 <- utils::read.csv(file.path(res2$dirs[[1]], "full", "all_records.csv"))
  expect_equal(unique(found2$query), "Argument text.")
})

test_that("the query column is added after comparison, so it never disturbs status", {
  store   <- withr::local_tempdir()
  out_dir <- withr::local_tempdir()

  dvp <- function(data) list(all_records =
    data.frame(record_id = data$records$record_id))
  info <- data.frame(check = "all_records", query = "Confirm the record.")

  snapA <- take_snapshot(datasource_example("redcap", n = 10L, seed = 1L),
                         store = store, verbose = 0L)
  snapB <- take_snapshot(datasource_example("redcap", n = 10L, seed = 1L),
                         store = store, verbose = 0L)

  res <- save_dvr(dvp, after = snapB, before = snapA, paths = out_dir,
                  check_info = info, write_xlsx = FALSE, verbose = 0L)
  found <- utils::read.csv(file.path(res$dirs[[1]], "full", "all_records.csv"))
  expect_equal(names(found)[1], "query")
  expect_true(all(found$status == "unchanged"))
})

test_that("a check returning its own query column errors while check_info is in use", {
  store <- withr::local_tempdir()
  dvp <- function(data) list(clash =
    data.frame(record_id = data$records$record_id, query = "mine"))
  info <- data.frame(check = "clash", query = "Engine text.")
  snap <- take_snapshot(datasource_example("redcap", n = 5L, seed = 1L),
                        store = store, verbose = 0L)
  expect_error(
    save_dvr(dvp, snap, paths = withr::local_tempdir(), check_info = info,
             write_xlsx = FALSE, verbose = 0L),
    "reserved column")
})

test_that("check_info is validated and partial annotation warns", {
  store <- withr::local_tempdir()
  dvp <- function(data) list(all_records =
    data.frame(record_id = data$records$record_id))
  snap <- take_snapshot(datasource_example("redcap", n = 5L, seed = 1L),
                        store = store, verbose = 0L)
  run <- function(info) save_dvr(dvp, snap, paths = withr::local_tempdir(),
                                 check_info = info, write_xlsx = FALSE, verbose = 0L)

  expect_error(run(list(check = "a")), "must be a data frame")
  expect_error(run(data.frame(check = c("a", "a"), query = c("x", "y"))), "Duplicate")
  expect_error(run(data.frame(check = "all_records", query = " ")), "non-empty text")
  expect_error(run(data.frame(check = "all_records", query = "x", critical = "yes")),
               "logical")
  expect_warning(run(data.frame(check = c("all_records", "ghost"),
                                query = c("x", "y"))), "matching no check")
})

test_that("checks_index leads every workbook, per-site included", {
  skip_if_not_installed("openxlsx")
  store   <- withr::local_tempdir()
  out_dir <- withr::local_tempdir()

  snap <- take_snapshot(datasource_example("redcap", n = 20L, seed = 1L),
                        store = store, verbose = 0L)
  snap$records$site <- rep(c("Site_A", "Site_B"), length.out = nrow(snap$records))

  dvp <- function(data) list(all_records =
    data.frame(record_id = data$records$record_id))
  info <- data.frame(check = "all_records", query = "Confirm the record.")

  res <- save_dvr(dvp, snap, paths = out_dir, id_col = "record_id",
                  site_col = "site", check_info = info, write_xlsx = TRUE,
                  verbose = 0L)

  full <- file.path(res$dirs[[1]], "full")
  master <- list.files(full, pattern = "\\.xlsx$", full.names = TRUE)[1]
  expect_equal(openxlsx::getSheetNames(master)[1], "checks_index")

  site_wb <- list.files(file.path(full, "sites", "Site_A"),
                        pattern = "\\.xlsx$", full.names = TRUE)[1]
  expect_equal(openxlsx::getSheetNames(site_wb)[1], "checks_index")
  expect_true(file.exists(file.path(full, "sites", "Site_A", "checks_index.csv")))
})
