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

test_that("with no project and no explicit store, take_snapshot errors (never guesses a location)", {
  wd <- withr::local_tempdir()
  withr::local_dir(wd)
  expect_error(
    take_snapshot(datasource_example("redcap", n = 5L, seed = 1L)),
    "bctu-project|No "
  )
})

test_that("every take and every delete commits the metadata to git (audit trail)", {
  skip_if(!nzchar(Sys.which("git")), "git not available")
  repo <- withr::local_tempdir()
  run <- function(...) system2("git", c("-C", repo, ...), stdout = FALSE, stderr = FALSE)
  run("init", "-q"); run("config", "user.email", "t@example.org"); run("config", "user.name", "t")
  writeLines("x", file.path(repo, "README")); run("add", "README"); run("commit", "-qm", "init")
  store <- file.path(repo, "Data", "Snapshots"); dir.create(store, recursive = TRUE)
  commits <- function() length(system2("git", c("-C", repo, "log", "--oneline"), stdout = TRUE))
  base <- commits()

  s1 <- take_snapshot(datasource_example("redcap", n = 5L, seed = 1L), store = store, verbose = 0L)
  expect_equal(commits(), base + 1L)                     # take committed metadata
  msg <- system2("git", c("-C", repo, "log", "-1", "--pretty=%s"), stdout = TRUE)
  expect_true(grepl(attr(s1, "id"), msg, fixed = TRUE))
  # the commit carries the manifest, never a payload file
  files <- system2("git", c("-C", repo, "show", "--name-only", "--pretty=format:", "HEAD"), stdout = TRUE)
  files <- files[nzchar(files)]
  expect_true(any(grepl("manifest\\.yml$", files)))
  expect_false(any(grepl("\\.(rds|csv)$", files)))

  delete_snapshot(attr(s1, "id"), reason = "test cleanup", store = store,
                  mode = "destroy", verbose = 0L)
  expect_equal(commits(), base + 2L)                     # delete committed too
  dmsg <- system2("git", c("-C", repo, "log", "-1", "--pretty=%s"), stdout = TRUE)
  expect_true(grepl("delete snapshot", dmsg, fixed = TRUE))
  expect_true(grepl("test cleanup", dmsg, fixed = TRUE))
})

test_that("delete_snapshot requires a reason (plain-English error, nothing deleted)", {
  store <- withr::local_tempdir()
  s <- take_snapshot(datasource_example("redcap", n = 5L, seed = 1L),
                     store = store, git = "off", verbose = 0L)
  expect_error(delete_snapshot(attr(s, "id"), store = store, verbose = 0L), "is required")
  expect_true(attr(s, "id") %in% list_snapshots(store))
})

test_that("unsafe table names are rejected", {
  store <- withr::local_tempdir()
  snap <- structure(list(`../evil` = data.frame(a = 1)),
                    class = c("bctu_snapshot", "list"),
                    bctu_meta = list(name = "X"))
  expect_error(save_snapshot(snap, store = store, verbose = 0L), "Unsafe table name")
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
