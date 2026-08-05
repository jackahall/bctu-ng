test_that("take_snapshot -> load_snapshot -> verify_snapshot round-trips", {
  store <- withr::local_tempdir()
  snap  <- take_snapshot(datasource_example("redcap", n = 12L, seed = 1L),
                         store = store, verbose = 0L)
  id <- attr(snap, "id")
  expect_true(is.character(id) && length(id) == 1L && nzchar(id))
  expect_equal(list_snapshots(store), id)

  loaded <- load_snapshot("latest", store = store, verbose = 0L)
  expect_s3_class(loaded, "bctu_snapshot")
  expect_equal(nrow(loaded$records), 12L)

  v <- verify_snapshot("latest", store = store)
  expect_true(v$ok)
})

test_that("the manifest is readable YAML carrying a sha256 per table", {
  store <- withr::local_tempdir()
  snap  <- take_snapshot(datasource_example("redcap", n = 6L, seed = 1L),
                         store = store, verbose = 0L)
  id  <- attr(snap, "id")
  man <- yaml::read_yaml(file.path(store, id, "manifest.yml"))

  expect_equal(man$schema, "bctu-snapshot/1")
  expect_true("records" %in% names(man$tables))
  sha <- man$tables$records$files$rds$sha256
  expect_true(is.character(sha) && length(sha) == 1L && nchar(sha) == 64L)
})

test_that("delete_snapshot writes a delete record to the audit ledger", {
  store <- withr::local_tempdir()
  snap  <- take_snapshot(datasource_example("redcap", n = 6L, seed = 1L),
                         store = store, verbose = 0L)
  id <- attr(snap, "id")

  delete_snapshot(id, reason = "unit-test cleanup", store = store, verbose = 0L)
  expect_length(list_snapshots(store), 0L)

  led <- read_ledger(store)
  events <- vapply(led, function(r) r$event, character(1))
  expect_true("extract" %in% events)
  expect_equal(led[[length(led)]]$event, "delete")
  expect_equal(led[[length(led)]]$reason, "unit-test cleanup")
})

test_that("delete_snapshot requires a reason", {
  store <- withr::local_tempdir()
  snap  <- take_snapshot(datasource_example("redcap", n = 6L, seed = 1L),
                         store = store, verbose = 0L)
  expect_error(delete_snapshot(attr(snap, "id"), reason = "", store = store, verbose = 0L))
})
