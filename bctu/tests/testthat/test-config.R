test_that("bctu_init_project writes a marker that bctu_project reads back", {
  proj <- withr::local_tempdir()
  bctu_init_project("EXAMPLE", dir = proj)
  expect_true(file.exists(file.path(proj, "bctu-project.yml")))

  p <- bctu_project(proj)
  expect_equal(p$name, "EXAMPLE")
  expect_equal(normalizePath(p$root), normalizePath(proj))
})

test_that("snapshot_store resolves relative to the marker and creates the dir", {
  proj <- withr::local_tempdir()
  bctu_init_project("EXAMPLE", dir = proj)

  store <- snapshot_store(proj, verbose = 0L)
  expect_equal(normalizePath(store),
               normalizePath(file.path(proj, "Data", "Snapshots")))
  expect_true(dir.exists(store))
})

test_that("a marker found in a parent directory resolves from a child", {
  proj <- withr::local_tempdir()
  bctu_init_project("EXAMPLE", dir = proj)
  child <- file.path(proj, "sub", "deeper")
  dir.create(child, recursive = TRUE)

  expect_equal(bctu_project(child)$name, "EXAMPLE")
})

test_that("no marker anywhere above errors loudly (no silent guess)", {
  empty <- withr::local_tempdir()
  expect_error(bctu_project(empty), "bctu-project|No ")
})
