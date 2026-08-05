test_that("as_findings rejects columns outside the fixed schema", {
  bad <- data.frame(record_id = "E001", field = "x", reason = "y", extra_col = 1)
  expect_error(as_findings(bad), "column")
})

test_that("as_findings requires the mandatory columns", {
  expect_error(as_findings(data.frame(record_id = "E001", field = "x")))
})

test_that("as_findings normalises NULL and zero-row input to a clean schema", {
  expect_equal(nrow(as_findings(NULL)), 0L)
  expect_s3_class(as_findings(NULL), "bctu_findings")
})

test_that("classify_findings labels new / changed / unchanged / resolved", {
  baseline <- new_findings(
    record_id = c("E001", "E002", "E004"),
    field     = "weight_kg",
    reason    = c("r1", "r2", "r4"),
    value     = c("10", "20", "40"),
    check_id  = "wt")
  current <- new_findings(
    record_id = c("E001", "E002", "E003"),
    field     = "weight_kg",
    reason    = c("r1", "r2", "r3"),
    value     = c("10", "25", "30"),
    check_id  = "wt")

  cls <- classify_findings(current, baseline)
  klass <- function(rid) cls$change_class[cls$record_id == rid]
  expect_equal(klass("E001"), "unchanged")
  expect_equal(klass("E002"), "changed")
  expect_equal(klass("E003"), "new")
  expect_equal(klass("E004"), "resolved")

  counts <- table(cls$change_class)
  expect_equal(as.integer(counts[c("new", "changed", "unchanged", "resolved")]),
               c(1L, 1L, 1L, 1L))
})

test_that("save_dvr baseline is the snapshot the last issued DVR was built from", {
  store   <- withr::local_tempdir()
  out_dir <- withr::local_tempdir()

  lo <- 65; hi <- 90
  weight_check <- bctu_check(
    id = "weight_range",
    description = "weight_kg missing or out of range",
    run = function(tables) {
      r <- tables$records
      bad <- is.na(r$weight_kg) | r$weight_kg < lo | r$weight_kg > hi
      if (!any(bad)) return(no_findings())
      new_findings(record_id = r$record_id[bad], field = "weight_kg",
                   value = r$weight_kg[bad], reason = "weight out of range")
    })

  snapA <- take_snapshot(datasource_example("redcap", n = 40L, seed = 1L),
                         store = store, verbose = 0L)
  idA <- attr(snapA, "id")
  r1 <- save_dvr(snapA, weight_check, out_dir, operator = "tester",
                 write_xlsx = FALSE, verbose = 0L)
  expect_true(is.na(r1$baseline_snapshot_id))

  Sys.sleep(1.1)
  snapB <- take_snapshot(datasource_example("redcap", n = 40L, seed = 3L),
                         store = store, verbose = 0L)
  r2 <- save_dvr(snapB, weight_check, out_dir, operator = "tester",
                 write_xlsx = FALSE, verbose = 0L)

  man <- yaml::read_yaml(file.path(r2$dir, "manifest.yml"))
  expect_equal(man$baseline_snapshot$id, idA)

  led <- read_dvr_ledger(out_dir, "dvr")
  expect_length(led, 2L)
  expect_equal(led[[2]]$baseline_snapshot_id, idA)
})
