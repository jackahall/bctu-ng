test_that("snapshot_id round-trips through parse_snapshot_id", {
  t  <- as.POSIXct("2025-03-04 09:15:42", tz = "UTC")
  id <- snapshot_id(t)
  expect_match(id, "^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{6}Z$")
  expect_equal(parse_snapshot_id(id), t)
})

test_that("snapshot_id rejects non-scalar or missing times", {
  expect_error(snapshot_id(as.POSIXct(c("2025-01-01", "2025-01-02"), tz = "UTC")))
  expect_error(snapshot_id(as.POSIXct(NA, tz = "UTC")))
})

test_that("parse_snapshot_id rejects a malformed id", {
  expect_error(parse_snapshot_id("2025-03-04 09:15:42"))
  expect_error(parse_snapshot_id("not-an-id"))
})

test_that("snapshot_date respects the timezone", {
  id <- "2025-06-01T233000Z"
  expect_equal(snapshot_date(id, tz = "UTC"), as.Date("2025-06-01"))
  expect_equal(snapshot_date(id, tz = "Europe/London"), as.Date("2025-06-02"))
})
