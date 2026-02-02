

test_that("column_selection_action updates selected columns", {
  cases <- list(
    list(
      name = "adds columns",
      selected = c("col3", "col4"),
      current = c("col1", "col2"),
      is_deselect = FALSE,
      expected = c("col1", "col2", "col3", "col4")
    ),
    list(
      name = "removes columns",
      selected = c("col2", "col4"),
      current = c("col1", "col2", "col3", "col4"),
      is_deselect = TRUE,
      expected = c("col1", "col3")
    ),
    list(
      name = "handles empty vectors",
      selected = character(0),
      current = character(0),
      is_deselect = FALSE,
      expected = character(0)
    ),
    list(
      name = "preserves duplicates when adding",
      selected = c("col2", "col3"),
      current = c("col1", "col2"),
      is_deselect = FALSE,
      expected = c("col1", "col2", "col2", "col3")
    ),
    list(
      name = "ignores unknown removals",
      selected = c("col3", "col4"),
      current = c("col1", "col2"),
      is_deselect = TRUE,
      expected = c("col1", "col2")
    ),
    list(
      name = "handles special characters",
      selected = c("adj.P.Val", "Sample-1"),
      current = c("Sample ID", "P.Value", "log2(FC)"),
      is_deselect = FALSE,
      expected = c("Sample ID", "P.Value", "log2(FC)", "adj.P.Val", "Sample-1")
    )
  )

  for (case in cases) {
    result <- column_selection_action(
      case$selected,
      case$current,
      is_deselect = case$is_deselect
    )
    expect_equal(result, case$expected, info = case$name)
  }

  expect_equal(
    column_selection_action(c("col2"), c("col1")),
    c("col1", "col2"),
    info = "default is_deselect"
  )
})



create_mock_rv <- function(filedata_1 = NULL, filedata_2 = NULL,
                           mapping_obj = NULL) {
  rv <- list()
  rv$filedata_1 <- function() filedata_1
  rv$filedata_2 <- function() filedata_2
  rv$mapping_obj <- function(x = NULL) {
    if (!missing(x)) {
      mapping_obj <<- x
    }
    mapping_obj
  }
  rv
}

create_mock_output <- function() {
  output <- list()
  output$load_status <- NULL
  renderText <- function(expr) {
    force(expr)
  }
  environment(renderText)$output <- output
  assign("renderText", renderText, envir = parent.frame())
  output
}

test_that("do_dataset_mapping handles missing both datasets", {
  rv <- create_mock_rv(filedata_1 = NULL, filedata_2 = NULL)
  output <- list(load_status = NULL)

  text_output <- NULL
  renderText <- function(expr) {
    text_output <<- force(expr)
    text_output
  }

  result_rv <- do_dataset_mapping(
    rv,
    feature_col_1 = "Feature",
    feature_col_2 = "ID",
    output = output,
    sample_cols1 = NULL,
    sample_cols2 = NULL,
    matched_samples = FALSE
  )

  expect_true(!is.null(result_rv))
})

test_that("do_dataset_mapping handles single dataset (dataset1 only)", {
  df1 <- data.frame(
    Feature = c("A", "B", "C"),
    s1 = c(1, 2, 3),
    s2 = c(4, 5, 6),
    stringsAsFactors = FALSE
  )

  rv <- create_mock_rv(filedata_1 = df1, filedata_2 = NULL)
  output <- list(load_status = NULL)

  result_rv <- do_dataset_mapping(
    rv,
    feature_col_1 = "Feature",
    feature_col_2 = NULL,
    output = output,
    sample_cols1 = c("s1", "s2"),
    sample_cols2 = NULL,
    matched_samples = FALSE
  )

  expect_true(!is.null(result_rv$mapping_obj()))
  expect_false(result_rv$mapping_obj()$dual_datasets)
})

test_that("do_dataset_mapping handles single dataset (dataset2 only)", {
  df2 <- data.frame(
    ID = c("X", "Y", "Z"),
    sampleA = c(7, 8, 9),
    sampleB = c(10, 11, 12),
    stringsAsFactors = FALSE
  )

  rv <- create_mock_rv(filedata_1 = NULL, filedata_2 = df2)
  output <- list(load_status = NULL)

  result_rv <- do_dataset_mapping(
    rv,
    feature_col_1 = NULL,
    feature_col_2 = "ID",
    output = output,
    sample_cols1 = NULL,
    sample_cols2 = c("sampleA", "sampleB"),
    matched_samples = FALSE
  )

  expect_true(!is.null(result_rv$mapping_obj()))
  expect_false(result_rv$mapping_obj()$dual_datasets)
})

test_that("do_dataset_mapping handles dual datasets", {
  df1 <- data.frame(
    Feature = c("A", "B", "C"),
    s1 = c(1, 2, 3),
    s2 = c(4, 5, 6),
    stringsAsFactors = FALSE
  )

  df2 <- data.frame(
    ID = c("A", "B", "D"),
    sampleA = c(7, 8, 9),
    sampleB = c(10, 11, 12),
    stringsAsFactors = FALSE
  )

  rv <- create_mock_rv(filedata_1 = df1, filedata_2 = df2)
  output <- list(load_status = NULL)

  result_rv <- do_dataset_mapping(
    rv,
    feature_col_1 = "Feature",
    feature_col_2 = "ID",
    output = output,
    sample_cols1 = c("s1", "s2"),
    sample_cols2 = c("sampleA", "sampleB"),
    matched_samples = FALSE
  )

  expect_true(!is.null(result_rv$mapping_obj()))
  expect_true(result_rv$mapping_obj()$dual_datasets)
  expect_equal(result_rv$mapping_obj()$target_col1, "Feature")
  expect_equal(result_rv$mapping_obj()$target_col2, "ID")
})

test_that("do_dataset_mapping handles matched_samples parameter", {
  skip("Requires Shiny session context for showNotification")

  df1 <- data.frame(
    Feature = c("A", "B"),
    s1 = c(1, 2),
    s2 = c(3, 4),
    stringsAsFactors = FALSE
  )

  df2 <- data.frame(
    ID = c("A", "B"),
    s1 = c(5, 6),
    s2 = c(7, 8),
    stringsAsFactors = FALSE
  )

  rv <- create_mock_rv(filedata_1 = df1, filedata_2 = df2)
  output <- list(load_status = NULL)

  result_rv <- do_dataset_mapping(
    rv,
    feature_col_1 = "Feature",
    feature_col_2 = "ID",
    output = output,
    sample_cols1 = c("s1", "s2"),
    sample_cols2 = c("s1", "s2"),
    matched_samples = TRUE
  )

  expect_true(!is.null(result_rv$mapping_obj()))
  expect_true(result_rv$mapping_obj()$has_correlations())
})

test_that("do_dataset_mapping handles skip_correlation parameter", {
  skip("Requires Shiny session context for showNotification with matched_samples")

  df1 <- data.frame(
    Feature = c("A", "B"),
    s1 = c(1, 2),
    s2 = c(3, 4),
    stringsAsFactors = FALSE
  )

  df2 <- data.frame(
    ID = c("A", "B"),
    s1 = c(5, 6),
    s2 = c(7, 8),
    stringsAsFactors = FALSE
  )

  rv <- create_mock_rv(filedata_1 = df1, filedata_2 = df2)
  output <- list(load_status = NULL)

  result_rv <- do_dataset_mapping(
    rv,
    feature_col_1 = "Feature",
    feature_col_2 = "ID",
    output = output,
    sample_cols1 = c("s1", "s2"),
    sample_cols2 = c("s1", "s2"),
    matched_samples = TRUE,
    skip_correlation = TRUE
  )

  expect_true(!is.null(result_rv$mapping_obj()))
  expect_false(result_rv$mapping_obj()$has_correlations())
})

test_that("do_dataset_mapping handles duplicates_method='discard'", {
  df1 <- data.frame(
    Feature = c("A", "A", "B"),
    s1 = c(1, 1.1, 2),
    s2 = c(3, 3.1, 4),
    stringsAsFactors = FALSE
  )

  df2 <- data.frame(
    ID = c("A", "B"),
    sampleA = c(5, 6),
    sampleB = c(7, 8),
    stringsAsFactors = FALSE
  )

  rv <- create_mock_rv(filedata_1 = df1, filedata_2 = df2)
  output <- list(load_status = NULL)

  result_rv <- do_dataset_mapping(
    rv,
    feature_col_1 = "Feature",
    feature_col_2 = "ID",
    output = output,
    sample_cols1 = c("s1", "s2"),
    sample_cols2 = c("sampleA", "sampleB"),
    matched_samples = FALSE,
    duplicates_method = "discard"
  )

  expect_true(!is.null(result_rv$mapping_obj()))
  expect_equal(nrow(result_rv$mapping_obj()$dataset1), 2)
})

test_that("do_dataset_mapping preserves sample column information", {
  df1 <- data.frame(
    Feature = c("A", "B", "C"),
    s1 = c(1, 2, 3),
    s2 = c(4, 5, 6),
    s3 = c(7, 8, 9),
    stringsAsFactors = FALSE
  )

  rv <- create_mock_rv(filedata_1 = df1, filedata_2 = NULL)
  output <- list(load_status = NULL)

  sample_cols <- c("s1", "s2")

  result_rv <- do_dataset_mapping(
    rv,
    feature_col_1 = "Feature",
    feature_col_2 = NULL,
    output = output,
    sample_cols1 = sample_cols,
    sample_cols2 = NULL,
    matched_samples = FALSE
  )

  expect_equal(result_rv$mapping_obj()$samples1, sample_cols)
})

test_that("do_dataset_mapping handles empty sample columns", {
  df1 <- data.frame(
    Feature = c("A", "B"),
    s1 = c(1, 2),
    stringsAsFactors = FALSE
  )

  rv <- create_mock_rv(filedata_1 = df1, filedata_2 = NULL)
  output <- list(load_status = NULL)

  result_rv <- do_dataset_mapping(
    rv,
    feature_col_1 = "Feature",
    feature_col_2 = NULL,
    output = output,
    sample_cols1 = NULL,
    sample_cols2 = NULL,
    matched_samples = FALSE
  )

  expect_true(!is.null(result_rv$mapping_obj()))
})

test_that("do_dataset_mapping returns modified rv object", {
  df1 <- data.frame(
    Feature = c("A", "B"),
    s1 = c(1, 2),
    stringsAsFactors = FALSE
  )

  rv <- create_mock_rv(filedata_1 = df1, filedata_2 = NULL)
  output <- list(load_status = NULL)

  result_rv <- do_dataset_mapping(
    rv,
    feature_col_1 = "Feature",
    feature_col_2 = NULL,
    output = output,
    sample_cols1 = c("s1"),
    sample_cols2 = NULL,
    matched_samples = FALSE
  )

  expect_identical(result_rv, rv)
})



test_that("do_dataset_mapping creates proper combined dataset", {
  df1 <- data.frame(
    Feature = c("Gene1", "Gene2", "Gene3"),
    Sample1 = c(10, 20, 30),
    Sample2 = c(15, 25, 35),
    stringsAsFactors = FALSE
  )

  df2 <- data.frame(
    ID = c("Gene1", "Gene2", "Gene4"),
    SampleA = c(12, 22, 42),
    SampleB = c(17, 27, 47),
    stringsAsFactors = FALSE
  )

  rv <- create_mock_rv(filedata_1 = df1, filedata_2 = df2)
  output <- list(load_status = NULL)

  result_rv <- do_dataset_mapping(
    rv,
    feature_col_1 = "Feature",
    feature_col_2 = "ID",
    output = output,
    sample_cols1 = c("Sample1", "Sample2"),
    sample_cols2 = c("SampleA", "SampleB"),
    matched_samples = FALSE
  )

  comb_df <- result_rv$mapping_obj()$get_combined_dataset(
    include_one_dataset_entries = FALSE
  )

  expect_equal(nrow(comb_df), 2)
  expect_true("comb_id" %in% colnames(comb_df))
})

test_that("do_dataset_mapping handles completely non-overlapping datasets", {
  df1 <- data.frame(
    Feature = c("A", "B", "C"),
    s1 = c(1, 2, 3),
    stringsAsFactors = FALSE
  )

  df2 <- data.frame(
    ID = c("X", "Y", "Z"),
    sampleA = c(7, 8, 9),
    stringsAsFactors = FALSE
  )

  rv <- create_mock_rv(filedata_1 = df1, filedata_2 = df2)
  output <- list(load_status = NULL)

  result_rv <- do_dataset_mapping(
    rv,
    feature_col_1 = "Feature",
    feature_col_2 = "ID",
    output = output,
    sample_cols1 = c("s1"),
    sample_cols2 = c("sampleA"),
    matched_samples = FALSE
  )

  expect_true(!is.null(result_rv$mapping_obj()))
  expect_false(result_rv$mapping_obj()$has_combined())
})

test_that("do_dataset_mapping validates feature columns exist", {
  df1 <- data.frame(
    Feature = c("A", "B"),
    s1 = c(1, 2),
    stringsAsFactors = FALSE
  )

  rv <- create_mock_rv(filedata_1 = df1, filedata_2 = NULL)
  output <- list(load_status = NULL)

  expect_error(
    do_dataset_mapping(
      rv,
      feature_col_1 = "NonExistentColumn",
      feature_col_2 = NULL,
      output = output,
      sample_cols1 = c("s1"),
      sample_cols2 = NULL,
      matched_samples = FALSE
    ),
    "NonExistentColumn"
  )
})
