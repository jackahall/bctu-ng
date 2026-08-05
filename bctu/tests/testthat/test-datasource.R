test_that("datasource_example(redcap) fetches a single records table", {
  src  <- datasource_example("redcap", n = 20L, seed = 1L)
  expect_s3_class(src, "datasource")

  snap <- fetch_snapshot(src, verbose = 0L)
  expect_s3_class(snap, "bctu_snapshot")
  expect_equal(names(snap), "records")
  expect_equal(nrow(snap$records), 20L)
  expect_true(all(c("record_id", "age", "sex", "weight_kg") %in% names(snap$records)))
})

test_that("datasource_example(sql) fetches multiple tables", {
  snap <- fetch_snapshot(datasource_example("sql", n = 8L, seed = 2L), verbose = 0L)
  expect_setequal(names(snap), c("subjects", "visits"))
  expect_equal(nrow(snap$subjects), 8L)
  expect_equal(nrow(snap$visits), 16L)
})

test_that("credential_spec derives the env var and resolves from it", {
  spec <- credential_spec(id = "demo")
  expect_s3_class(spec, "credential_spec")
  expect_equal(spec$env, "BCTU_API_TOKEN_DEMO")

  withr::local_envvar(BCTU_API_TOKEN_DEMO = "not-a-real-token-value")
  expect_equal(suppressWarnings(resolve_credentials(spec, verbose = 0L)),
               "not-a-real-token-value")
  expect_true(suppressWarnings(has_credential(spec)))
})

test_that("a required credential that is absent errors", {
  spec <- credential_spec(id = "absent_demo")
  withr::local_envvar(BCTU_API_TOKEN_ABSENT_DEMO = "")
  expect_error(suppressWarnings(resolve_credentials(spec, verbose = 0L)))
  expect_false(suppressWarnings(has_credential(spec)))
})
