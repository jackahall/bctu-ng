make_example_table <- function() {
  tab_data <- data.frame(
    characteristic = c("Age (years)", "Weight (kg)", "Female"),
    arm_a_n        = c(30, 30, 30),
    arm_a_stat     = c(54.2, 78.510, 16),
    arm_b_n        = c(31, 31, 31),
    arm_b_stat     = c(55.1, 80.3, 18),
    stringsAsFactors = FALSE)
  rt <- report_table(
    tab_data,
    columns = c(characteristic = "Characteristic",
                arm_a_n = "n", arm_a_stat = "Statistic",
                arm_b_n = "n", arm_b_stat = "Statistic"),
    caption = "Baseline characteristics by arm",
    group_headers = c(" " = 1, "Arm A" = 2, "Arm B" = 2),
    banner_rows = list(list(label = "Continuous", after = 0),
                       list(label = "Binary",     after = 2)))
  list(rt = rt, data = tab_data)
}

test_that("report_table_data returns the underlying numbers unchanged", {
  ex <- make_example_table()
  qc <- report_table_data(ex$rt)
  expect_equal(qc$arm_a_stat, ex$data$arm_a_stat)
  expect_equal(qc$arm_b_stat, ex$data$arm_b_stat)
})

test_that("render_table_markdown produces a non-empty grid table with the data", {
  ex <- make_example_table()
  md <- render_table_markdown(ex$rt)
  expect_true(nzchar(md))
  expect_match(md, "78.51", fixed = TRUE)
  expect_match(md, "Arm A", fixed = TRUE)
  expect_match(md, "Continuous", fixed = TRUE)
})

test_that("render_table_latex produces non-empty LaTeX with spanning header and banner", {
  ex  <- make_example_table()
  tex <- render_table_latex(ex$rt)
  expect_true(nzchar(tex))
  expect_match(tex, "78.51", fixed = TRUE)
  expect_match(tex, "\\multicolumn{2}{c}{Arm A}", fixed = TRUE)
  expect_match(tex, "textbf{Continuous}", fixed = TRUE)
})

test_that("render_report writes a docx bundle with a provenance manifest", {
  skip_on_cran()
  skip_if(!nzchar(Sys.which("pandoc")), "pandoc not on PATH")

  store <- withr::local_tempdir()
  snap  <- take_snapshot(datasource_example("redcap", n = 12L, seed = 1L),
                         store = store, verbose = 0L)
  ex <- make_example_table()
  report <- bctu_report(
    title = "EXAMPLE Monitoring Report",
    sections = list(
      intro  = report_heading("Introduction", level = 1),
      para   = report_paragraph("Assembled from explicit section objects."),
      table1 = ex$rt),
    meta = list(author = "Test Harness", trial = "EXAMPLE"))

  out_dir <- withr::local_tempdir()
  res <- render_report(report, output_dir = out_dir, formats = "docx",
                       snapshot = snap, verbose = 0L)

  expect_true(file.exists(res$outputs$docx))
  expect_gt(file.info(res$outputs$docx)$size, 0)
  expect_true(file.exists(res$manifest))

  man <- yaml::read_yaml(res$manifest)
  expect_false(is.null(man$outputs$docx$sha256))
  expect_equal(man$snapshot$id, attr(snap, "id"))
})

test_that("render_table_markdown escapes pipe and newline so the grid table stays structurally valid", {
  df <- data.frame(id = 1:2,
                   note = c("has|pipe", "line1\nline2"),
                   stringsAsFactors = FALSE)
  rt <- report_table(df)
  md <- render_table_markdown(rt)

  # the literal pipe is escaped, not left to look like a column boundary
  expect_match(md, "has\\|pipe", fixed = TRUE)
  # the embedded newline is collapsed so the row stays on one grid-table line
  expect_false(grepl("line1\nline2", md, fixed = TRUE))
  expect_match(md, "line1 line2", fixed = TRUE)

  # every content line has the same number of (unescaped) "|" as a border
  # line has "+", i.e. no cell content introduced a spurious column
  lines <- strsplit(md, "\n")[[1]]
  content_lines <- lines[startsWith(lines, "|")]
  border_lines  <- lines[startsWith(lines, "+")]
  n_pipes   <- lengths(regmatches(content_lines, gregexpr("(?<!\\\\)\\|", content_lines, perl = TRUE)))
  n_borders <- lengths(regmatches(border_lines, gregexpr("\\+", border_lines)))
  expect_true(all(n_pipes == n_borders[1]))
})

test_that("latex_escape and render_table_latex escape a literal backslash exactly once", {
  expect_equal(bctu:::latex_escape("a\\b"), "a\\textbackslash{}b")

  df <- data.frame(path = "C:\\Users\\Data", stringsAsFactors = FALSE)
  rt  <- report_table(df)
  tex <- render_table_latex(rt)
  expect_match(tex, "C:\\textbackslash{}Users\\textbackslash{}Data", fixed = TRUE)
  # the braces textbackslash{} introduces must not be re-escaped
  expect_false(grepl("textbackslash\\{", tex, fixed = TRUE))
})

test_that("render_table_markdown renders a structurally valid table for zero rows", {
  df <- data.frame(id = integer(0), note = character(0), stringsAsFactors = FALSE)
  rt <- report_table(df)
  md <- render_table_markdown(rt)

  lines <- strsplit(md, "\n")[[1]]
  expect_true(length(lines) >= 4)
  expect_true(startsWith(lines[1], "+"))
  expect_true(startsWith(lines[length(lines)], "+"))
})
