
test_that("minimap_hist creates histogram with cutoff line", {
  test_data <- data.frame(
    pvalue = c(0.001, 0.01, 0.05, 0.1, 0.5),
    feature = c("F1", "F2", "F3", "F4", "F5")
  )

  plt <- minimap_hist(test_data, "pvalue", 0.05, 20, "Test P-values")

  expect_s3_class(plt, "gg")
  expect_equal(plt$labels$title, "Test P-values distribution")
  expect_true(any(sapply(plt$layers, function(l) inherits(l$geom, "GeomVline"))))
})

test_that("minimap_hist handles NA values", {
  test_data <- data.frame(
    pvalue = c(0.001, NA, 0.05, 0.1, 0.5)
  )

  plt <- minimap_hist(test_data, "pvalue", 0.05, 20, "Test")

  expect_s3_class(plt, "gg")
})

test_that("pvaluehists generates side-by-side histograms", {
  test_data <- data.frame(
    cond.P.Value = c(0.001, 0.05, 0.1),
    condB.P.Value = c(0.01, 0.03, 0.2),
    pass_threshold_data = c(TRUE, TRUE, FALSE)
  )

  stat_cols1 <- list(P.Value = "cond.P.Value")
  stat_cols2 <- list(P.Value = "condB.P.Value")

  result <- pvaluehists(test_data, stat_cols1, stat_cols2, "CondA", "CondB", 10)

  expect_s3_class(result, "ggarrange")
})

test_that("scatterplots generates MA plot", {
  test_data <- data.frame(
    AveExpr = c(5, 6, 7, 8),
    logFC = c(1, -1, 2, -0.5),
    pass_threshold_data = c(TRUE, TRUE, FALSE, TRUE)
  )

  stat_cols1 <- list(AveExpr = "AveExpr", logFC = "logFC")
  stat_cols2 <- list(AveExpr = "AveExpr", logFC = "logFC")

  plots <- scatterplots(test_data, stat_cols1, stat_cols2, "Cond1", "Cond2", mode="ma")

  expect_length(plots, 2)
  expect_s3_class(plots[[1]], "gg")
  expect_s3_class(plots[[2]], "gg")
})

test_that("scatterplots generates volcano plot", {
  test_data <- data.frame(
    logFC_1 = c(1, -1, 2, -0.5),
    P.Value_1 = c(0.001, 0.05, 0.001, 0.5),
    logFC_2 = c(0.5, -2, 1.5, 0.2),
    P.Value_2 = c(0.01, 0.001, 0.05, 0.8),
    pass_threshold_data = c(TRUE, TRUE, FALSE, TRUE)
  )

  stat_cols1 <- list(logFC = "logFC_1", P.Value = "P.Value_1")
  stat_cols2 <- list(logFC = "logFC_2", P.Value = "P.Value_2")

  plots <- scatterplots(test_data, stat_cols1, stat_cols2, "A", "B", mode="volcano")

  expect_length(plots, 2)
  expect_s3_class(plots[[1]], "gg")
  expect_s3_class(plots[[2]], "gg")
})

test_that("scatterplots rejects invalid mode", {
  test_data <- data.frame(x = 1, y = 1)

  expect_error(
    scatterplots(test_data, list(), list(), "A", "B", mode="invalid"),
    "Unknown mode"
  )
})

test_that("custom_comp_plot highlights fold change contradictions", {
  test_data <- data.frame(
    logFC_1 = c(2, -1, 1),
    logFC_2 = c(1, 1, -1),
    P.Value = c(0.001, 0.05, 0.001),
    pass_threshold_data = c(TRUE, TRUE, FALSE)
  )

  stat_cols1 <- list(P.Value = "P.Value", logFC = "logFC_1")
  stat_cols2 <- list(logFC = "logFC_2")

  result <- custom_comp_plot(test_data, stat_cols1, stat_cols2, "P.Value", 0.05)

  expect_s3_class(result, "ggarrange")
})

test_that("exact_fold_comp_plot handles p-value filtering", {
  test_data <- data.frame(
    logFC_1 = c(2, 0.5, 1),
    logFC_2 = c(1, 0.3, -1),
    P.Value = c(0.001, 0.5, 0.001),
    AveExpr = c(5, 6, 7)
  )

  group1 <- list(logFC = "logFC_1", P.Value = "P.Value", AveExpr = "AveExpr")
  group2 <- list(logFC = "logFC_2")

  result <- exact_fold_comp_plot(
    test_data, group1, group2, "", "A", "B", "P.Value", 0.05
  )

  expect_s3_class(result, "gg")
})

test_that("exact_fold_comp_plot handles empty data", {
  test_data <- data.frame(
    logFC_1 = numeric(0),
    logFC_2 = numeric(0),
    P.Value = numeric(0),
    AveExpr = numeric(0)
  )

  group1 <- list(logFC = "logFC_1", P.Value = "P.Value", AveExpr = "AveExpr")
  group2 <- list(logFC = "logFC_2")

  expect_s3_class(
    exact_fold_comp_plot(test_data, group1, group2, "", "A", "B", "P.Value", 0.05),
    "gg"
  )
})
