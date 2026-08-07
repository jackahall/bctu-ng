test_that("a tag propagates to the snapshot, its tables, the manifest and the ledger", {
  store <- withr::local_tempdir()
  snap <- take_snapshot(datasource_example("redcap", n = 8L, seed = 1L),
                        store = store, tag = "DMC-2026-08", git = "off", verbose = 0L)

  expect_equal(attr(snap, "bctu_tag"), "DMC-2026-08")
  expect_equal(attr(snap$records, "bctu_tag"), "DMC-2026-08")

  id  <- attr(snap, "id")
  man <- yaml::read_yaml(file.path(store, id, "manifest.yml"))
  expect_equal(man$tag, "DMC-2026-08")

  led <- read_ledger(store)
  expect_equal(led[[1]]$tag, "DMC-2026-08")

  loaded <- load_snapshot("latest", store = store, verbose = 0L)
  expect_equal(attr(loaded, "bctu_tag"), "DMC-2026-08")
})

test_that("git=commit records HEAD, commits metadata only, and annotates a tag", {
  skip_if(!nzchar(Sys.which("git")), "git not available")
  repo <- withr::local_tempdir()
  # a real repo with one commit so HEAD exists
  run <- function(...) system2("git", c("-C", repo, ...), stdout = FALSE, stderr = FALSE)
  run("init", "-q")
  run("config", "user.email", "t@example.org"); run("config", "user.name", "t")
  writeLines("x", file.path(repo, "README"))
  run("add", "README"); run("commit", "-qm", "init")

  store <- file.path(repo, "Data", "Snapshots"); dir.create(store, recursive = TRUE)
  snap <- take_snapshot(datasource_example("redcap", n = 5L, seed = 1L),
                        store = store, tag = "DMC-2026-08", verbose = 0L)
  id <- attr(snap, "id")

  man <- yaml::read_yaml(file.path(store, id, "manifest.yml"))
  expect_true(is.character(man$git_head) && nchar(man$git_head) >= 7L)

  tags <- system2("git", c("-C", repo, "tag", "--list"), stdout = TRUE)
  expect_true("snap/DMC-2026-08" %in% tags)

  # the commit staged metadata (manifest + ledger), never a payload file
  files <- system2("git", c("-C", repo, "show", "--name-only", "--pretty=format:", "HEAD"),
                   stdout = TRUE)
  files <- files[nzchar(files)]
  expect_true(any(grepl("manifest\\.yml$", files)))
  expect_false(any(grepl("\\.rds$", files)))
})
