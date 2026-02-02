
test_that("parse_stat_cols identifies statistical columns", {
  raw_cols <- c("cond.P.Value", "cond.adj.P.Val", "cond.logFC", "cond.AveExpr")
  stat_patterns <- list(
    P.Value = ".P.Value",
    adj.P.Val = ".adj.P.Val",
    logFC = ".logFC",
    AveExpr = ".AveExpr"
  )

  result <- parse_stat_cols(raw_cols, "cond", stat_patterns)

  expect_equal(result$P.Value, "cond.P.Value")
  expect_equal(result$adj.P.Val, "cond.adj.P.Val")
  expect_equal(result$logFC, "cond.logFC")
  expect_equal(result$AveExpr, "cond.AveExpr")
})

test_that("parse_stat_cols throws error on missing columns", {
  raw_cols <- c("col1", "col2")
  stat_patterns <- list(
    P.Value = ".P.Value",
    adj.P.Val = ".adj.P.Val",
    logFC = ".logFC",
    AveExpr = ".AveExpr"
  )

  expect_error(
    parse_stat_cols(raw_cols, "cond", stat_patterns),
    "No match for desired columns"
  )
})

test_that("parse_stat_cols handles pattern with prefix", {
  raw_cols <- c("cond.P.Value", "cond.adj.P.Val", "cond.logFC", "cond.AveExpr")
  stat_patterns <- list(
    P.Value = ".P.Value",
    adj.P.Val = ".adj.P.Val",
    logFC = ".logFC",
    AveExpr = ".AveExpr"
  )

  result <- parse_stat_cols(raw_cols, "cond", stat_patterns)

  expect_equal(result$P.Value, "cond.P.Value")
  expect_equal(result$logFC, "cond.logFC")
})

test_that("factor_prep_color_col converts character to factor", {
  test_df <- data.frame(
    id = 1:5,
    color = c("A", "B", "A", "C", "B")
  )

  result <- factor_prep_color_col(test_df, "color", 2, 5)

  expect_s3_class(result$color, "factor")
})

test_that("factor_prep_color_col bins numeric values", {
  test_df <- data.frame(
    id = 1:10,
    values = 1:10
  )

  result <- factor_prep_color_col(test_df, "values", 2, 5)

  expect_s3_class(result$values, "factor")
  expect_lte(nlevels(result$values), 6)
})

test_that("factor_prep_color_col groups rare categories as 'other'", {
  test_df <- data.frame(
    id = 1:100,
    category = c(rep("A", 50), rep("B", 30), rep("C", 15), rep("D", 5))
  )

  result <- factor_prep_color_col(test_df, "category", 2, 100)

  expect_lte(nlevels(result$category), 3)
})

test_that("get_curr_selected_cols_pattern identifies dataset correctly", {
  filenames <- c("dataset1.tsv", "dataset2.tsv")

  expect_equal(
    get_curr_selected_cols_pattern("dataset1.tsv", filenames),
    "selected_cols_1"
  )
  expect_equal(
    get_curr_selected_cols_pattern("dataset2.tsv", filenames),
    "selected_cols_2s"
  )
})

test_that("get_curr_selected_cols_pattern errors on unknown file", {
  filenames <- c("dataset1.tsv", "dataset2.tsv")

  expect_error(
    get_curr_selected_cols_pattern("unknown.tsv", filenames),
    "Unknown situation"
  )
})

test_that("di_new maps dataset index correctly", {
  mock_rv <- list(
    filename_1 = function() "file1.txt",
    filename_2 = function() "file2.txt"
  )

  expect_equal(di_new(mock_rv, "file1.txt"), 1)
  expect_equal(di_new(mock_rv, "file2.txt"), 2)
  expect_null(di_new(mock_rv, "file3.txt"))
})

test_that("di_new handles NULL filename_2", {
  mock_rv <- list(
    filename_1 = function() "file1.txt",
    filename_2 = function() NULL
  )

  expect_equal(di_new(mock_rv, "file1.txt"), 1)
  expect_null(di_new(mock_rv, "file2.txt"))
})

test_that("assign_fig_settings configures plotly output", {
  mock_rv <- list(
    figure_save_format = function() "svg",
    figure_save_width = function() 1000,
    figure_save_height = function() 800
  )

  base_plot <- plotly::plot_ly()
  result <- assign_fig_settings(base_plot, mock_rv)

  expect_s3_class(result, "plotly")
})

test_that("settings_download_handler creates proper download handler", {
  mock_input <- list(
    param1 = "value1",
    param2 = "value2"
  )

  handler <- settings_download_handler("test_module", mock_input)

  expect_type(handler, "closure")
  expect_s3_class(handler, "function")
})
