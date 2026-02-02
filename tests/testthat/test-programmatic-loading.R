
if (!exists("runApp")) {
  source(file.path("..", "..", "R", "launch_app.R"), local = TRUE)
}

test_that("validate_preloaded_data validates data frames", {
  data1 <- data.frame(Feature = c("A", "B"), s1 = c(1, 2), s2 = c(3, 4))

  expect_silent(
    validate_preloaded_data(
      data1 = data1,
      data2 = NULL,
      design1 = NULL,
      design2 = NULL,
      feature_col1 = "Feature",
      feature_col2 = NULL,
      sample_col1 = "sample",
      sample_col2 = "sample",
      two_datasets = FALSE
    )
  )

  expect_error(
    validate_preloaded_data(
      data1 = "not a dataframe",
      data2 = NULL,
      design1 = NULL,
      design2 = NULL,
      feature_col1 = NULL,
      feature_col2 = NULL,
      sample_col1 = "sample",
      sample_col2 = "sample",
      two_datasets = FALSE
    ),
    "data1 must be a data.frame"
  )

  expect_error(
    validate_preloaded_data(
      data1 = data.frame(),
      data2 = NULL,
      design1 = NULL,
      design2 = NULL,
      feature_col1 = NULL,
      feature_col2 = NULL,
      sample_col1 = "sample",
      sample_col2 = "sample",
      two_datasets = FALSE
    ),
    "data1 has no rows"
  )

  expect_error(
    validate_preloaded_data(
      data1 = data1,
      data2 = NULL,
      design1 = NULL,
      design2 = NULL,
      feature_col1 = "NonExistent",
      feature_col2 = NULL,
      sample_col1 = "sample",
      sample_col2 = "sample",
      two_datasets = FALSE
    ),
    "feature_col1 'NonExistent' not found"
  )
})

test_that("validate_preloaded_data validates design matrices", {
  data1 <- data.frame(Feature = c("A", "B"), s1 = c(1, 2), s2 = c(3, 4))
  design1 <- data.frame(sample = c("s1", "s2"), condition = c("A", "B"))

  expect_silent(
    validate_preloaded_data(
      data1 = data1,
      data2 = NULL,
      design1 = design1,
      design2 = NULL,
      feature_col1 = "Feature",
      feature_col2 = NULL,
      sample_col1 = "sample",
      sample_col2 = "sample",
      two_datasets = FALSE
    )
  )

  expect_error(
    validate_preloaded_data(
      data1 = data1,
      data2 = NULL,
      design1 = design1,
      design2 = NULL,
      feature_col1 = "Feature",
      feature_col2 = NULL,
      sample_col1 = "NonExistent",
      sample_col2 = "sample",
      two_datasets = FALSE
    ),
    "sample_col1 'NonExistent' not found"
  )

  bad_design <- data.frame(sample = c("s1", "s2", "s3"), condition = c("A", "B", "C"))
  expect_warning(
    validate_preloaded_data(
      data1 = data1,
      data2 = NULL,
      design1 = bad_design,
      design2 = NULL,
      feature_col1 = "Feature",
      feature_col2 = NULL,
      sample_col1 = "sample",
      sample_col2 = "sample",
      two_datasets = FALSE
    ),
    "Design1 samples not in data1: s3"
  )
})

test_that("validate_preloaded_data validates two datasets", {
  data1 <- data.frame(Feature = c("A", "B"), s1 = c(1, 2), s2 = c(3, 4))
  data2 <- data.frame(ID = c("A", "C"), s1 = c(5, 6), s2 = c(7, 8))

  expect_silent(
    validate_preloaded_data(
      data1 = data1,
      data2 = data2,
      design1 = NULL,
      design2 = NULL,
      feature_col1 = "Feature",
      feature_col2 = "ID",
      sample_col1 = "sample",
      sample_col2 = "sample",
      two_datasets = TRUE
    )
  )

  expect_error(
    validate_preloaded_data(
      data1 = data1,
      data2 = NULL,
      design1 = NULL,
      design2 = NULL,
      feature_col1 = "Feature",
      feature_col2 = NULL,
      sample_col1 = "sample",
      sample_col2 = "sample",
      two_datasets = TRUE
    ),
    "two_datasets=TRUE but data2 is NULL"
  )
})

test_that("detect_feature_column returns first column", {
  data1 <- data.frame(Feature = c("A", "B"), s1 = c(1, 2), s2 = c(3, 4))

  expect_equal(detect_feature_column(data1), "Feature")

  data2 <- data.frame(ID = c("A", "B"), Value = c(1, 2))
  expect_equal(detect_feature_column(data2), "ID")
})

test_that("detect_sample_columns excludes feature and stat columns", {
  data1 <- data.frame(
    Feature = c("A", "B"),
    s1 = c(1, 2),
    s2 = c(3, 4),
    s3 = c(5, 6),
    P.Value = c(0.01, 0.05),
    logFC = c(1.5, -2.0)
  )

  sample_cols <- detect_sample_columns(data1, "Feature")

  expect_true("s1" %in% sample_cols)
  expect_true("s2" %in% sample_cols)
  expect_true("s3" %in% sample_cols)

  expect_false("Feature" %in% sample_cols)
  expect_false("P.Value" %in% sample_cols)
  expect_false("logFC" %in% sample_cols)
})

test_that("detect_stat_columns finds statistical columns", {
  data1 <- data.frame(
    Feature = c("A", "B"),
    s1 = c(1, 2),
    P.Value = c(0.01, 0.05),
    adj.P.Val = c(0.02, 0.06),
    logFC = c(1.5, -2.0),
    AveExpr = c(10, 20)
  )

  stat_cols <- detect_stat_columns(data1)

  expect_true("P.Value" %in% stat_cols)
  expect_true("adj.P.Val" %in% stat_cols)
  expect_true("logFC" %in% stat_cols)
  expect_true("AveExpr" %in% stat_cols)

  expect_false("Feature" %in% stat_cols)
  expect_false("s1" %in% stat_cols)
})

test_that("runApp parameter aliases work correctly", {
  data_df <- data.frame(
    Feature = c("Gene_A", "Gene_B"),
    s1 = c(1, 2),
    s2 = c(3, 4)
  )



  expect_error(
    runApp(data = data_df, data1 = data_df),
    "Cannot specify both 'data' and 'data1'"
  )

  design_df <- data.frame(sample = c("s1", "s2"), condition = c("A", "B"))
  expect_error(
    runApp(design = design_df, design1 = design_df),
    "Cannot specify both 'design' and 'design1'"
  )
})

test_that("has_preloaded_data and get_preloaded_data work", {
  options(omicloupe.preloaded = NULL)

  expect_false(has_preloaded_data())
  expect_null(get_preloaded_data())

  test_data <- list(data1 = data.frame(x = 1:3))
  options(omicloupe.preloaded = test_data)

  expect_true(has_preloaded_data())
  expect_equal(get_preloaded_data(), test_data)

  options(omicloupe.preloaded = NULL)
})
