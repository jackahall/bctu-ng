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

test_that("payload files are self-identifying (study + table + id)", {
  store <- withr::local_tempdir()
  snap  <- take_snapshot(datasource_example("redcap", n = 6L, seed = 1L),
                         store = store, verbose = 0L)
  id  <- attr(snap, "id")
  tdir  <- file.path(store, id, "tables", "records")
  files <- list.files(tdir)
  expect_setequal(files,
                  c(paste0("example_records_", id, ".rds"),
                    paste0("example_records_", id, ".csv")))
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

test_that("with no project, snapshots save in the working directory and load back", {
  wd <- withr::local_tempdir()
  withr::local_dir(wd)

  expect_message(
    snap <- take_snapshot(datasource_example("redcap", n = 5L, seed = 1L)),
    "working directory"
  )
  id <- attr(snap, "id")

  expect_true(dir.exists(file.path(wd, id, "tables", "records")))
  expect_equal(list_snapshots(), id)

  loaded <- load_snapshot(verbose = 0L)
  expect_s3_class(loaded, "bctu_snapshot")
  expect_equal(nrow(loaded$records), 5L)
})

test_that("delete_snapshot retires by default, keeping the manifest and a deletion note", {
  store <- withr::local_tempdir()
  snap  <- take_snapshot(datasource_example("redcap", n = 6L, seed = 1L),
                         store = store, verbose = 0L)
  id <- attr(snap, "id")

  delete_snapshot(id, reason = "unit-test cleanup", store = store, verbose = 0L)
  expect_length(list_snapshots(store), 0L)

  retired <- file.path(store, "_deleted", id)
  expect_true(file.exists(file.path(retired, "manifest.yml")))
  expect_true(file.exists(file.path(retired, "deletion-note.yml")))
  note <- yaml::read_yaml(file.path(retired, "deletion-note.yml"))
  expect_equal(note$reason, "unit-test cleanup")
  expect_equal(note$id, id)
})

test_that("delete_snapshot can destroy outright", {
  store <- withr::local_tempdir()
  snap  <- take_snapshot(datasource_example("redcap", n = 6L, seed = 1L),
                         store = store, verbose = 0L)
  id <- attr(snap, "id")

  delete_snapshot(id, reason = "unit-test cleanup", store = store,
                  mode = "destroy", verbose = 0L)
  expect_length(list_snapshots(store), 0L)
  expect_false(dir.exists(file.path(store, "_deleted", id)))
})

test_that("delete_snapshot requires a reason", {
  store <- withr::local_tempdir()
  snap  <- take_snapshot(datasource_example("redcap", n = 6L, seed = 1L),
                         store = store, verbose = 0L)
  expect_error(delete_snapshot(attr(snap, "id"), reason = "", store = store, verbose = 0L))
})
