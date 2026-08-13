test_that("redcap_request builds a POST request and requires a report_id for reports", {
  req <- redcap_request("https://redcap.example.org/api/", "TOK", content = "record")
  expect_s3_class(req, "httr2_request")
  expect_equal(req$method, "POST")

  expect_error(redcap_request("u", "t", content = "report"), "report_id")
  rep_req <- redcap_request("u", "t", content = "report", report_id = "42")
  expect_s3_class(rep_req, "httr2_request")
})

test_that("redcap_read_csv parses, honours as_character, and returns an empty frame for an empty body", {
  csv <- "record_id,age\nE1,50\nE2,61\n"
  df <- bctu:::redcap_read_csv(csv)
  expect_equal(nrow(df), 2L)
  expect_equal(df$record_id, c("E1", "E2"))

  ch <- bctu:::redcap_read_csv(csv, as_character = TRUE)
  expect_type(ch$age, "character")

  expect_equal(nrow(bctu:::redcap_read_csv("")), 0L)
})

test_that("redcap_guard_body rejects an in-body error payload so it is never snapshotted", {
  expect_error(bctu:::redcap_guard_body('{"error":"You do not have permission"}', "REDCap export"),
               "returned an error")
  expect_silent(bctu:::redcap_guard_body("record_id,age\nE1,50\n", "REDCap export"))
})

test_that("redcap_parse_choices splits code/label pairs, keeping commas in labels", {
  ch <- bctu:::redcap_parse_choices("1, Yes | 2, No, not really | 3, Unknown")
  expect_equal(ch$code, c("1", "2", "3"))
  expect_equal(ch$label, c("Yes", "No, not really", "Unknown"))
  expect_null(bctu:::redcap_parse_choices(NA))
})

test_that("redcap_labelled applies numeric-coded value labels", {
  skip_if_not_installed("haven")
  ch <- data.frame(code = c("1", "0"), label = c("Yes", "No"), stringsAsFactors = FALSE)
  v  <- bctu:::redcap_labelled(c("1", "0", "1"), ch)
  expect_s3_class(v, "haven_labelled")
  expect_equal(as.vector(v), c(1, 0, 1))
})

test_that("redcap_apply_labels labels radio/yesno fields and leaves others alone", {
  skip_if_not_installed("haven")
  records <- data.frame(record_id = c("E1", "E2"),
                        sex = c("1", "2"), consent = c("1", "0"),
                        stringsAsFactors = FALSE)
  dict <- data.frame(
    field_name = c("sex", "consent"),
    field_type = c("radio", "yesno"),
    select_choices_or_calculations = c("1, Male | 2, Female", ""),
    stringsAsFactors = FALSE)
  out <- redcap_apply_labels(records, dict)
  expect_s3_class(out$sex, "haven_labelled")
  expect_s3_class(out$consent, "haven_labelled")
  expect_type(out$record_id, "character")
})

test_that("sql_odbc_arguments renders the full connection surface and omits NULLs", {
  conn <- sql_connection(uid = "u", pwd = "p", port = 1433L,
                         trust_server_certificate = TRUE)
  args <- sql_odbc_arguments("srv", "db", conn)
  expect_equal(args$Server, "srv")
  expect_equal(args$Database, "db")
  expect_equal(args$UID, "u")
  expect_equal(args$PWD, "p")
  expect_equal(args$Port, 1433L)
  expect_equal(args$TrustServerCertificate, "yes")
  expect_null(args$encoding)
})

test_that("datasource_sql requires either tables or a discovery query", {
  expect_error(datasource_sql(server = "s", database = "d"),
               "tables|views_query")
})

test_that("datasource_sql loads multiple tables through an injected DBI backend", {
  skip_if_not_installed("RSQLite")
  db <- withr::local_tempfile(fileext = ".sqlite")
  con <- DBI::dbConnect(RSQLite::SQLite(), db)
  DBI::dbWriteTable(con, "subjects", data.frame(id = 1:3, arm = c("A", "B", "A")))
  DBI::dbWriteTable(con, "visits", data.frame(id = c(1L, 1L, 2L), day = c(0L, 7L, 0L)))
  DBI::dbDisconnect(con)

  src <- datasource_sql(server = "srv", database = "db",
                        tables = c("subjects", "visits"),
                        connector = RSQLite::SQLite(),
                        connect_args = list(dbname = db))
  tabs <- fetch_snapshot(src, verbose = 0L)
  expect_named(tabs, c("subjects", "visits"))
  expect_equal(nrow(tabs$subjects), 3L)
  expect_equal(nrow(tabs$visits), 3L)
})

test_that("datasource_redcap rejects a non-special_missing mapping", {
  expect_error(
    datasource_redcap("ocean", "https://r/api/", missing_codes = list(UNK = "a")),
    "special_missing")
})

test_that("labels cover checkbox columns via the field-name export map, plus yesno and radio", {
  skip_if_not_installed("haven")
  records <- data.frame(
    record_id = c("1", "2"),
    x___1 = c(1, 0), x___2 = c(0, 1),
    y = c(1, 0),
    r = c(2, 3))
  dictionary <- data.frame(
    field_name = c("record_id", "x", "y", "r"),
    field_type = c("text", "checkbox", "yesno", "radio"),
    select_choices_or_calculations =
      c("", "1, Apple | 2, Pear", "", "2, Mid | 3, High"),
    stringsAsFactors = FALSE)
  field_names <- data.frame(
    original_field_name = c("record_id", "x", "x", "y", "r"),
    choice_value = c("", "1", "2", "", ""),
    export_field_name = c("record_id", "x___1", "x___2", "y", "r"),
    stringsAsFactors = FALSE)

  out <- redcap_apply_labels(records, dictionary, field_names)
  expect_s3_class(out$x___1, "haven_labelled")
  expect_s3_class(out$x___2, "haven_labelled")
  expect_s3_class(out$y, "haven_labelled")
  expect_s3_class(out$r, "haven_labelled")
  expect_equal(as.character(haven::as_factor(out$x___1)), c("Yes", "No"))
  expect_equal(as.character(haven::as_factor(out$y)), c("Yes", "No"))
  expect_equal(as.character(haven::as_factor(out$r)), c("Mid", "High"))
  expect_false(inherits(out$record_id, "haven_labelled"))

  no_map <- redcap_apply_labels(records, dictionary)
  expect_false(inherits(no_map$x___1, "haven_labelled"))
  expect_s3_class(no_map$y, "haven_labelled")
})
