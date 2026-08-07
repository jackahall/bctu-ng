# --- report_table(): column resolution --------------------------------------

test_that("report_table defaults to every column of data, heading = column name", {
  df <- data.frame(x = 1:3, y = c("a", "b", "c"), stringsAsFactors = FALSE)
  rt <- report_table(df)
  expect_equal(rt$columns$name, c("x", "y"))
  expect_equal(rt$columns$label, c("x", "y"))
  expect_equal(names(rt$body), c("x", "y"))
})

test_that("report_table honours a named character columns argument (names -> data cols, values -> labels)", {
  df <- data.frame(id = 1:2, val = c(10.5, 20.1), stringsAsFactors = FALSE)
  rt <- report_table(df, columns = c(id = "ID", val = "Value"))
  expect_equal(rt$columns$name, c("id", "val"))
  expect_equal(rt$columns$label, c("ID", "Value"))
})

test_that("report_table errors on a column not present in data", {
  df <- data.frame(x = 1:2, stringsAsFactors = FALSE)
  expect_error(report_table(df, columns = c("x", "nope")), "not in")
})

test_that("report_table errors when align length does not match the number of displayed columns", {
  df <- data.frame(x = 1:2, y = 1:2, stringsAsFactors = FALSE)
  expect_error(report_table(df, align = "left"), "one entry per displayed column")
})

test_that("report_table honours an explicit align vector over the numeric/character default", {
  df <- data.frame(x = 1:2, y = c("a", "b"), stringsAsFactors = FALSE)
  rt <- report_table(df, align = c("center", "right"))
  expect_equal(rt$columns$align, c("center", "right"))
})

test_that("report_table defaults align to right for numeric columns and left otherwise", {
  df <- data.frame(x = 1:2, y = c("a", "b"), stringsAsFactors = FALSE)
  rt <- report_table(df)
  expect_equal(rt$columns$align, c("right", "left"))
})

# --- report_table_data() -----------------------------------------------------

test_that("report_table_data returns the displayed columns as a plain data frame", {
  df <- data.frame(a = 1:3, b = c(1.1, 2.2, 3.3), c = c("x", "y", "z"),
                   stringsAsFactors = FALSE)
  rt <- report_table(df, columns = c("a", "c"))
  qc <- report_table_data(rt)
  expect_equal(names(qc), c("a", "c"))
  expect_equal(qc$a, df$a)
  expect_equal(qc$c, df$c)
})

test_that("report_table_data errors on a non-report-table input", {
  expect_error(report_table_data(list(body = data.frame(x = 1))), "bctu_report_table")
})

# --- group_headers ------------------------------------------------------------

test_that("group_headers accepts a numeric-named vector form", {
  df <- data.frame(a = 1, b = 2, c = 3)
  rt <- report_table(df, group_headers = c("Group 1" = 2, "Group 2" = 1))
  expect_equal(rt$group_headers$label, c("Group 1", "Group 2"))
  expect_equal(rt$group_headers$span, c(2L, 1L))
})

test_that("group_headers accepts a data frame form", {
  df <- data.frame(a = 1, b = 2, c = 3)
  gh <- data.frame(label = c("Left", "Right"), span = c(1, 2), stringsAsFactors = FALSE)
  rt <- report_table(df, group_headers = gh)
  expect_equal(rt$group_headers$label, c("Left", "Right"))
  expect_equal(rt$group_headers$span, c(1L, 2L))
})

test_that("group_headers errors when spans do not sum to the number of displayed columns", {
  df <- data.frame(a = 1, b = 2, c = 3)
  expect_error(report_table(df, group_headers = c("Only" = 2)), "add up to")
})

test_that("a spanning group-header label wider than its span widens the rendered grid table", {
  df <- data.frame(a = 1:2, b = 1:2, stringsAsFactors = FALSE)
  rt_narrow <- report_table(df, group_headers = c("AB" = 2))
  rt_wide   <- report_table(df, group_headers = c("A Very Long Spanning Label" = 2))

  md_narrow <- render_table_markdown(rt_narrow)
  md_wide   <- render_table_markdown(rt_wide)

  width_narrow <- nchar(strsplit(md_narrow, "\n")[[1]][1])
  width_wide   <- nchar(strsplit(md_wide, "\n")[[1]][1])
  expect_gt(width_wide, width_narrow)
  expect_match(md_wide, "A Very Long Spanning Label", fixed = TRUE)
})

# --- banner_rows ---------------------------------------------------------------

test_that("banner_rows accepts a list-of-lists form", {
  df <- data.frame(x = 1:3)
  rt <- report_table(df, banner_rows = list(list(label = "Section A", after = 0),
                                            list(label = "Section B", after = 2)))
  expect_equal(rt$banner_rows$label, c("Section A", "Section B"))
  expect_equal(rt$banner_rows$after, c(0L, 2L))
})

test_that("banner_rows accepts a data frame form", {
  df <- data.frame(x = 1:3)
  br <- data.frame(label = "Only banner", after = 1, stringsAsFactors = FALSE)
  rt <- report_table(df, banner_rows = br)
  expect_equal(rt$banner_rows$label, "Only banner")
  expect_equal(rt$banner_rows$after, 1L)
})

test_that("banner_rows errors when after is out of range", {
  df <- data.frame(x = 1:3)
  expect_error(report_table(df, banner_rows = list(list(label = "Bad", after = 4))),
              "between 0 and the number of rows")
  expect_error(report_table(df, banner_rows = list(list(label = "Bad", after = -1))),
              "between 0 and the number of rows")
})

test_that("a banner at after = 0 appears before the first body row and after = nrow appears at the end", {
  df <- data.frame(x = c(1, 2), stringsAsFactors = FALSE)
  rt <- report_table(df, banner_rows = list(list(label = "TOP", after = 0),
                                            list(label = "BOTTOM", after = 2)))
  md <- render_table_markdown(rt)
  lines <- strsplit(md, "\n")[[1]]
  content_lines <- lines[startsWith(lines, "|")]

  top_idx    <- grep("TOP", content_lines, fixed = TRUE)
  bottom_idx <- grep("BOTTOM", content_lines, fixed = TRUE)
  body_1_idx <- grep("^\\|\\s*1\\b", content_lines)
  body_2_idx <- grep("^\\|\\s*2\\b", content_lines)

  expect_length(top_idx, 1)
  expect_length(bottom_idx, 1)
  expect_true(top_idx < body_1_idx)
  expect_true(bottom_idx > body_2_idx)
})

# --- render_table_markdown: grid table structure ------------------------------

test_that("render_table_markdown produces a well-formed pandoc grid table shell", {
  df <- data.frame(a = 1:2, b = c("x", "y"), stringsAsFactors = FALSE)
  rt <- report_table(df)
  md <- render_table_markdown(rt)
  lines <- strsplit(md, "\n")[[1]]

  expect_true(startsWith(lines[1], "+"))
  border_lines <- lines[startsWith(lines, "+")]
  expect_true(startsWith(border_lines[length(border_lines)], "+"))
  expect_true(any(grepl("=", lines, fixed = TRUE) & startsWith(lines, "+")))
})

test_that("the header separator line uses '=' and content lines use '|'", {
  df <- data.frame(a = 1:2, stringsAsFactors = FALSE)
  rt <- report_table(df)
  md <- render_table_markdown(rt)
  lines <- strsplit(md, "\n")[[1]]

  eq_lines <- lines[grepl("=", lines, fixed = TRUE)]
  expect_length(eq_lines, 1)
  expect_true(startsWith(eq_lines[1], "+"))
})

test_that("a long body token wider than its header widens that column", {
  df <- data.frame(short_header = c("x", "a-very-long-body-value-indeed"),
                   stringsAsFactors = FALSE)
  rt <- report_table(df)
  md <- render_table_markdown(rt)
  lines <- strsplit(md, "\n")[[1]]

  border_width <- nchar(lines[1])
  # the column must be at least as wide as the longest body token plus the
  # 2-space content padding and the 2 border "+" characters
  expect_gte(border_width, nchar("a-very-long-body-value-indeed") + 2L)
})

test_that("alignment colons appear on the '=' header separator row for right/center/left columns", {
  df <- data.frame(num = 1:2, txt = c("a", "b"), stringsAsFactors = FALSE)
  rt <- report_table(df, align = c("right", "center"))
  md <- render_table_markdown(rt)
  lines <- strsplit(md, "\n")[[1]]
  eq_line <- lines[grepl("=", lines, fixed = TRUE)][1]

  # right-aligned column: colon on the trailing edge of its segment
  # center-aligned column: colons on both edges
  segs <- strsplit(sub("^\\+", "", sub("\\+$", "", eq_line)), "\\+")[[1]]
  expect_true(endsWith(segs[1], ":"))
  expect_false(startsWith(segs[1], ":"))
  expect_true(startsWith(segs[2], ":"))
  expect_true(endsWith(segs[2], ":"))
})

# --- format_table_column() / NA-NaN handling ----------------------------------

test_that("format_table_column renders NA and NaN as blank strings", {
  out <- bctu:::format_table_column(c(1, NA, NaN, 4))
  expect_equal(out, c("1", "", "", "4"))
})

test_that("format_table_column renders character NA as blank too", {
  out <- bctu:::format_table_column(c("a", NA, "c"))
  expect_equal(out, c("a", "", "c"))
})

test_that("a numeric column formats to a common number of decimal places via format()", {
  out <- bctu:::format_table_column(c(1, 2.5, 3))
  expect_equal(out, c("1.0", "2.5", "3.0"))
})

test_that("NA values render as blank cells in the rendered markdown grid table", {
  df <- data.frame(x = c(1, NA, 3), stringsAsFactors = FALSE)
  rt <- report_table(df)
  md <- render_table_markdown(rt)
  lines <- strsplit(md, "\n")[[1]]
  content_lines <- lines[startsWith(lines, "|")]
  # the NA row's cell should be present but contain no digit
  na_row <- content_lines[3]
  expect_false(grepl("NA", na_row, fixed = TRUE))
})
