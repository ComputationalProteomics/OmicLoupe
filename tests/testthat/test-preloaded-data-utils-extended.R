

call_validate_preloaded_data <- function(...) {
  defaults <- list(
    data1 = make_feature_df(features = c("A", "B"), sample_cols = c("s1", "s2")),
    data2 = NULL,
    design1 = NULL,
    design2 = NULL,
    feature_col1 = "Feature",
    feature_col2 = NULL,
    sample_col1 = "sample",
    sample_col2 = "sample",
    two_datasets = FALSE
  )
  args <- defaults
  dots <- list(...)
  for (name in names(dots)) {
    args[name] <- list(dots[[name]])
  }
  do.call(validate_preloaded_data, args)
}

test_that("validate_preloaded_data rejects data1 with only 1 column", {
  data1 <- data.frame(Feature = c("A", "B"))

  expect_error(
    call_validate_preloaded_data(data1 = data1),
    "data1 must have at least 2 columns"
  )
})

test_that("validate_preloaded_data accepts NULL feature_col1", {
  data1 <- data.frame(Feature = c("A", "B"), s1 = c(1, 2), s2 = c(3, 4))

  expect_silent(
    call_validate_preloaded_data(data1 = data1, feature_col1 = NULL)
  )
})

test_that("validate_preloaded_data rejects non-dataframe design1", {
  data1 <- data.frame(Feature = c("A", "B"), s1 = c(1, 2), s2 = c(3, 4))

  expect_error(
    call_validate_preloaded_data(data1 = data1, design1 = "not a dataframe"),
    "design1 must be a data.frame"
  )
})

test_that("validate_preloaded_data handles design1 when feature_col1 is NULL", {
  data1 <- data.frame(Feature = c("A", "B"), s1 = c(1, 2), s2 = c(3, 4))
  design1 <- data.frame(sample = c("s1", "s2", "s99"), condition = c("A", "B", "C"))

  expect_silent(
    call_validate_preloaded_data(data1 = data1, design1 = design1, feature_col1 = NULL)
  )
})

test_that("validate_preloaded_data truncates long missing sample lists", {
  data1 <- data.frame(Feature = c("A", "B"), s1 = c(1, 2), s2 = c(3, 4))
  design1 <- data.frame(
    sample = c("s1", "s2", "m1", "m2", "m3", "m4", "m5", "m6", "m7"),
    condition = rep("X", 9)
  )

  expect_warning(
    result <- call_validate_preloaded_data(data1 = data1, design1 = design1),
    "m1, m2, m3, m4, m5"
  )
})


test_that("validate_preloaded_data rejects non-dataframe data2", {
  data1 <- data.frame(Feature = c("A", "B"), s1 = c(1, 2))

  expect_error(
    call_validate_preloaded_data(
      data1 = data1,
      data2 = "not a dataframe",
      feature_col2 = "ID",
      two_datasets = TRUE
    ),
    "data2 must be a data.frame"
  )
})

test_that("validate_preloaded_data rejects empty data2", {
  data1 <- data.frame(Feature = c("A", "B"), s1 = c(1, 2))
  data2 <- data.frame()

  expect_error(
    call_validate_preloaded_data(data1 = data1, data2 = data2, feature_col2 = "ID", two_datasets = TRUE),
    "data2 has no rows"
  )
})

test_that("validate_preloaded_data rejects data2 with only 1 column", {
  data1 <- data.frame(Feature = c("A", "B"), s1 = c(1, 2))
  data2 <- data.frame(ID = c("X", "Y"))

  expect_error(
    call_validate_preloaded_data(data1 = data1, data2 = data2, feature_col2 = "ID", two_datasets = TRUE),
    "data2 must have at least 2 columns"
  )
})

test_that("validate_preloaded_data rejects invalid feature_col2", {
  data1 <- data.frame(Feature = c("A", "B"), s1 = c(1, 2))
  data2 <- data.frame(ID = c("X", "Y"), s1 = c(3, 4))

  expect_error(
    call_validate_preloaded_data(
      data1 = data1,
      data2 = data2,
      feature_col2 = "NonExistent",
      two_datasets = TRUE
    ),
    "feature_col2 'NonExistent' not found in data2 columns"
  )
})

test_that("validate_preloaded_data accepts NULL feature_col2", {
  data1 <- data.frame(Feature = c("A", "B"), s1 = c(1, 2))
  data2 <- data.frame(ID = c("X", "Y"), s1 = c(3, 4))

  expect_silent(
    call_validate_preloaded_data(data1 = data1, data2 = data2, feature_col2 = NULL, two_datasets = TRUE)
  )
})

test_that("validate_preloaded_data rejects non-dataframe design2", {
  data1 <- data.frame(Feature = c("A", "B"), s1 = c(1, 2))
  data2 <- data.frame(ID = c("X", "Y"), s1 = c(3, 4))

  expect_error(
    call_validate_preloaded_data(
      data1 = data1,
      data2 = data2,
      design2 = list(sample = c("s1")),
      feature_col2 = "ID",
      two_datasets = TRUE
    ),
    "design2 must be a data.frame"
  )
})

test_that("validate_preloaded_data rejects invalid sample_col2 in design2", {
  data1 <- data.frame(Feature = c("A", "B"), s1 = c(1, 2))
  data2 <- data.frame(ID = c("X", "Y"), s1 = c(3, 4))
  design2 <- data.frame(sample = c("s1"), condition = c("A"))

  expect_error(
    call_validate_preloaded_data(
      data1 = data1,
      data2 = data2,
      design2 = design2,
      feature_col2 = "ID",
      sample_col2 = "NonExistent",
      two_datasets = TRUE
    ),
    "sample_col2 'NonExistent' not found in design2 columns"
  )
})

test_that("validate_preloaded_data validates design2 with valid data", {
  data1 <- data.frame(Feature = c("A", "B"), s1 = c(1, 2))
  data2 <- data.frame(ID = c("X", "Y"), s1 = c(3, 4), s2 = c(5, 6))
  design2 <- data.frame(sample = c("s1", "s2"), condition = c("A", "B"))

  expect_silent(
    call_validate_preloaded_data(
      data1 = data1,
      data2 = data2,
      design2 = design2,
      feature_col2 = "ID",
      two_datasets = TRUE
    )
  )
})


test_that("detect_feature_column rejects non-dataframe", {
  expect_error(
    detect_feature_column("not a dataframe"),
    "data must be a data.frame"
  )
})

test_that("detect_feature_column rejects dataframe with no columns", {
  empty_df <- data.frame(row.names = 1:5)

  expect_error(
    detect_feature_column(empty_df),
    "data has no columns"
  )
})

test_that("detect_feature_column handles single column dataframe", {
  single_col <- data.frame(OnlyColumn = c("A", "B", "C"))

  expect_equal(detect_feature_column(single_col), "OnlyColumn")
})

test_that("detect_feature_column handles unusual column names", {
  weird_df <- data.frame(
    `Feature ID` = c("A", "B"),
    `Sample 1` = c(1, 2),
    check.names = FALSE
  )

  expect_equal(detect_feature_column(weird_df), "Feature ID")
})


test_that("detect_sample_columns with design parameter", {
  data1 <- data.frame(
    Feature = c("A", "B"),
    s1 = c(1, 2),
    s2 = c(3, 4),
    P.Value = c(0.01, 0.05)
  )
  design1 <- data.frame(sample = c("s1", "s2"), condition = c("A", "B"))

  result_with_design <- detect_sample_columns(data1, "Feature", design = design1)
  result_without_design <- detect_sample_columns(data1, "Feature", design = NULL)

  expect_equal(result_with_design, result_without_design)
  expect_true("s1" %in% result_with_design)
  expect_true("s2" %in% result_with_design)
})

test_that("detect_sample_columns when all columns are non-sample", {
  data1 <- data.frame(
    Feature = c("A", "B"),
    P.Value = c(0.01, 0.05),
    logFC = c(1.5, -2.0)
  )

  sample_cols <- detect_sample_columns(data1, "Feature")

  expect_equal(length(sample_cols), 0)
  expect_type(sample_cols, "character")
})

test_that("detect_sample_columns when no stat columns exist", {
  data1 <- data.frame(
    Feature = c("A", "B"),
    s1 = c(1, 2),
    s2 = c(3, 4),
    s3 = c(5, 6)
  )

  sample_cols <- detect_sample_columns(data1, "Feature")

  expect_equal(sort(sample_cols), c("s1", "s2", "s3"))
})


test_that("detect_stat_columns handles case-insensitive matching", {
  data1 <- data.frame(
    Feature = c("A", "B"),
    s1 = c(1, 2),
    pvalue = c(0.01, 0.05),
    LOGFC = c(1.5, -2.0),
    Ave_Expr = c(10, 20)
  )

  stat_cols <- detect_stat_columns(data1)

  expect_true("pvalue" %in% stat_cols)
  expect_true("LOGFC" %in% stat_cols)
})

test_that("detect_stat_columns detects all statistical patterns", {
  data1 <- data.frame(
    Feature = c("A", "B"),
    P.Value = c(0.01, 0.05),
    adj.P.Val = c(0.02, 0.06),
    p.value = c(0.01, 0.05),
    pvalue = c(0.01, 0.05),
    logFC = c(1.5, -2.0),
    log2FC = c(1.2, -1.8),
    FoldChange = c(2.0, 0.5),
    AveExpr = c(10, 20),
    t.value = c(3.2, -2.5),
    B.value = c(1.0, -0.5)
  )

  stat_cols <- detect_stat_columns(data1)

  expect_true("P.Value" %in% stat_cols)
  expect_true("adj.P.Val" %in% stat_cols)
  expect_true("p.value" %in% stat_cols)
  expect_true("pvalue" %in% stat_cols)
  expect_true("logFC" %in% stat_cols)
  expect_true("log2FC" %in% stat_cols)
  expect_true("FoldChange" %in% stat_cols)
  expect_true("AveExpr" %in% stat_cols)
  expect_true("t.value" %in% stat_cols)
  expect_true("B.value" %in% stat_cols)
})

test_that("detect_stat_columns returns empty when no stat columns", {
  data1 <- data.frame(
    Feature = c("A", "B"),
    s1 = c(1, 2),
    s2 = c(3, 4)
  )

  stat_cols <- detect_stat_columns(data1)

  expect_equal(length(stat_cols), 0)
  expect_type(stat_cols, "character")
})

test_that("detect_stat_columns handles mixed case patterns", {
  data1 <- data.frame(
    Feature = c("A", "B"),
    P_Value = c(0.01, 0.05),
    Log2FC = c(1.5, -2.0),
    AVEEXPR = c(10, 20)
  )

  stat_cols <- detect_stat_columns(data1)

  expect_true("Log2FC" %in% stat_cols)
  expect_true("AVEEXPR" %in% stat_cols)
})

test_that("detect_stat_columns doesn't match unrelated columns", {
  data1 <- data.frame(
    Feature = c("A", "B"),
    sample1 = c(1, 2),
    sample2 = c(3, 4),
    expression = c(5, 6),
    intensity = c(7, 8),
    P.Value = c(0.01, 0.05)
  )

  stat_cols <- detect_stat_columns(data1)

  expect_equal(stat_cols, "P.Value")
  expect_false("sample1" %in% stat_cols)
  expect_false("expression" %in% stat_cols)
  expect_false("intensity" %in% stat_cols)
})


test_that("has_preloaded_data returns FALSE when preloaded exists but data1 is NULL", {
  options(omicloupe.preloaded = list(data1 = NULL))

  expect_false(has_preloaded_data())

  options(omicloupe.preloaded = NULL)
})

test_that("has_preloaded_data returns FALSE when preloaded is empty list", {
  options(omicloupe.preloaded = list())

  expect_false(has_preloaded_data())

  options(omicloupe.preloaded = NULL)
})

test_that("has_preloaded_data returns TRUE only when data1 is not NULL", {
  options(omicloupe.preloaded = list(
    data1 = data.frame(x = 1:3),
    data2 = NULL
  ))

  expect_true(has_preloaded_data())

  options(omicloupe.preloaded = NULL)
})


test_that("get_preloaded_data returns NULL when option not set", {
  options(omicloupe.preloaded = NULL)

  result <- get_preloaded_data()

  expect_null(result)
})

test_that("get_preloaded_data returns full configuration", {
  test_config <- list(
    data1 = data.frame(Feature = c("A", "B"), s1 = c(1, 2)),
    data2 = data.frame(ID = c("X", "Y"), s1 = c(3, 4)),
    design1 = data.frame(sample = c("s1"), condition = c("A")),
    design2 = NULL,
    feature_col1 = "Feature",
    feature_col2 = "ID",
    sample_col1 = "sample",
    sample_col2 = "sample",
    two_datasets = TRUE
  )

  options(omicloupe.preloaded = test_config)

  result <- get_preloaded_data()

  expect_equal(result, test_config)
  expect_equal(result$data1, test_config$data1)
  expect_equal(result$feature_col1, "Feature")
  expect_true(result$two_datasets)

  options(omicloupe.preloaded = NULL)
})


test_that("detect functions work together in typical workflow", {
  data1 <- data.frame(
    Gene = c("ACTB", "GAPDH", "TP53"),
    Sample_A = c(10.5, 12.3, 8.7),
    Sample_B = c(11.2, 13.1, 9.2),
    Sample_C = c(9.8, 11.9, 8.1),
    P.Value = c(0.001, 0.05, 0.3),
    logFC = c(2.5, 1.2, 0.5),
    adj.P.Val = c(0.01, 0.1, 0.4)
  )

  feature_col <- detect_feature_column(data1)
  expect_equal(feature_col, "Gene")

  stat_cols <- detect_stat_columns(data1)
  expect_equal(sort(stat_cols), sort(c("P.Value", "adj.P.Val", "logFC")))

  sample_cols <- detect_sample_columns(data1, feature_col)
  expect_equal(sort(sample_cols), c("Sample_A", "Sample_B", "Sample_C"))

  expect_silent(
    call_validate_preloaded_data(data1 = data1, feature_col1 = feature_col)
  )
})

test_that("validation handles complete two-dataset workflow", {
  data1 <- data.frame(
    Protein = c("P1", "P2", "P3"),
    Ctrl_1 = c(100, 200, 150),
    Ctrl_2 = c(110, 190, 145),
    Treat_1 = c(150, 180, 200),
    Treat_2 = c(160, 175, 195),
    P.Value = c(0.001, 0.05, 0.01),
    logFC = c(0.5, -0.1, 0.4)
  )

  data2 <- data.frame(
    Gene = c("G1", "G2", "G3"),
    Ctrl_1 = c(50, 60, 55),
    Ctrl_2 = c(52, 58, 54),
    Treat_1 = c(75, 70, 80),
    Treat_2 = c(78, 72, 82),
    P.Value = c(0.002, 0.03, 0.001),
    log2FC = c(0.6, 0.2, 0.5)
  )

  design1 <- data.frame(
    sample = c("Ctrl_1", "Ctrl_2", "Treat_1", "Treat_2"),
    condition = c("Control", "Control", "Treatment", "Treatment")
  )

  design2 <- data.frame(
    sample = c("Ctrl_1", "Ctrl_2", "Treat_1", "Treat_2"),
    condition = c("Control", "Control", "Treatment", "Treatment"),
    replicate = c(1, 2, 1, 2)
  )

  expect_silent(
    call_validate_preloaded_data(
      data1 = data1,
      data2 = data2,
      design1 = design1,
      design2 = design2,
      feature_col1 = "Protein",
      feature_col2 = "Gene",
      two_datasets = TRUE
    )
  )
})
