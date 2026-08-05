test_that("check_setup passes the core checks in a temp project", {
  store <- withr::local_tempdir()
  result <- check_setup(store = store, formats = "docx", verbose = 0L)

  expect_s3_class(result, "bctu_setup_qualification")

  core <- result$checks[result$checks$check %in%
    c("R version", "Required package: cli", "Required package: digest",
      "Required package: yaml", "Snapshot store writable"), ]
  expect_true(all(core$passed))
  expect_true(result$ok)
})

test_that("write_setup_report writes the schema and leaks no credential value", {
  proj <- withr::local_tempdir()
  sentinel <- "SUPERSECRET-TOKEN-abcdef0123456789-DO-NOT-LEAK"
  withr::local_envvar(BCTU_API_TOKEN_DEMO = sentinel)

  result <- suppressWarnings(check_setup(store = proj, formats = "docx",
                        require_credentials = credential_spec(id = "demo"),
                        verbose = 0L))

  cred_row <- result$checks[result$checks$check == "Credential present: demo", ]
  expect_equal(cred_row$status, "PRESENT")

  report_path <- file.path(proj, "bctu-setup-qualification.yml")
  write_setup_report(result, path = report_path)

  lines <- readLines(report_path)
  expect_true(any(grepl("bctu-setup-qualification/1", lines, fixed = TRUE)))
  expect_false(any(grepl(sentinel, lines, fixed = TRUE)))

  printed <- paste(capture.output(print(result)), collapse = "\n")
  expect_false(grepl(sentinel, printed, fixed = TRUE))
})
