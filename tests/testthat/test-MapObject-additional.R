

test_that("generate_correlation_table works with all correlation types", {
  ref_df <- data.frame(
    sample1 = c(1.0, 2.0, 3.0, 4.0),
    sample2 = c(1.5, 2.5, 3.5, 4.5)
  )

  comp_df <- data.frame(
    sampleA = c(1.1, 2.1, 3.1, 4.1),
    sampleB = c(1.4, 2.4, 3.4, 4.4)
  )

  mo <- MapObject$new(data.frame(feature = c("A", "B", "C", "D")), "feature")

  corr_all <- mo$generate_correlation_table(ref_df, comp_df,
                                             corr_types = list("pearson", "spearman", "kendall"))

  expect_true("pearson.pval" %in% colnames(corr_all))
  expect_true("spearman.pval" %in% colnames(corr_all))
  expect_true("kendall.pval" %in% colnames(corr_all))
  expect_true("pearson.fdr" %in% colnames(corr_all))
  expect_true("spearman.fdr" %in% colnames(corr_all))
  expect_true("kendall.fdr" %in% colnames(corr_all))
  expect_equal(nrow(corr_all), 4)
})

test_that("generate_correlation_table handles rows with insufficient data", {
  ref_df <- data.frame(
    sample1 = c(1.0, 2.0, NA, 4.0),
    sample2 = c(1.5, NA, NA, 4.5),
    sample3 = c(1.8, 2.8, NA, 4.8)
  )

  comp_df <- data.frame(
    sampleA = c(1.1, 2.1, 3.1, 4.1),
    sampleB = c(1.4, 2.4, 3.4, 4.4),
    sampleC = c(1.7, 2.7, 3.7, 4.7)
  )

  mo <- MapObject$new(data.frame(feature = c("A", "B", "C", "D")), "feature")

  corr_result <- mo$generate_correlation_table(ref_df, comp_df,
                                                corr_types = list("pearson", "spearman", "kendall"))

  expect_true(is.na(corr_result$pearson.pval[3]))
  expect_true(is.na(corr_result$pearson.cor[3]))
  expect_true(is.na(corr_result$spearman.cor[3]))
  expect_true(is.na(corr_result$kendall.cor[3]))

  expect_false(is.na(corr_result$pearson.pval[1]))
  expect_false(is.na(corr_result$pearson.pval[4]))

  expect_true(is.na(corr_result$pearson.pval[2]))
})

test_that("generate_correlation_table adds id_column when provided", {
  ref_df <- data.frame(
    sample1 = c(1.0, 2.0, 3.0),
    sample2 = c(1.5, 2.5, 3.5)
  )

  comp_df <- data.frame(
    sampleA = c(1.1, 2.1, 3.1),
    sampleB = c(1.4, 2.4, 3.4)
  )

  id_col <- c("protA", "protB", "protC")

  mo <- MapObject$new(data.frame(feature = id_col), "feature")

  corr_with_id <- mo$generate_correlation_table(ref_df, comp_df,
                                                 id_column = id_col)

  expect_true("id" %in% colnames(corr_with_id))
  expect_equal(corr_with_id$id, id_col)
  expect_equal(which(colnames(corr_with_id) == "id"), 1)
})

test_that("generate_correlation_table computes FDR correctly", {
  ref_df <- data.frame(
    sample1 = c(1.0, 2.0, 3.0, 4.0, 5.0),
    sample2 = c(1.5, 2.5, 3.5, 4.5, 5.5)
  )

  comp_df <- data.frame(
    sampleA = c(1.1, 2.1, 3.1, 4.1, 5.1),
    sampleB = c(1.4, 2.4, 3.4, 4.4, 5.4)
  )

  mo <- MapObject$new(data.frame(feature = letters[1:5]), "feature")

  corr_result <- mo$generate_correlation_table(ref_df, comp_df)

  expect_true("pearson.fdr" %in% colnames(corr_result))
  expect_true("spearman.fdr" %in% colnames(corr_result))
  expect_true("kendall.fdr" %in% colnames(corr_result))

  expect_true(all(corr_result$pearson.fdr >= corr_result$pearson.pval, na.rm = TRUE))
  expect_true(all(corr_result$spearman.fdr >= corr_result$spearman.pval, na.rm = TRUE))
  expect_true(all(corr_result$kendall.fdr >= corr_result$kendall.pval, na.rm = TRUE))
})

test_that("generate_correlation_table handles all NA rows", {
  ref_df <- data.frame(
    sample1 = c(1.0, NA, 3.0),
    sample2 = c(1.5, NA, 3.5)
  )

  comp_df <- data.frame(
    sampleA = c(1.1, NA, 3.1),
    sampleB = c(1.4, NA, 3.4)
  )

  mo <- MapObject$new(data.frame(feature = c("A", "B", "C")), "feature")

  corr_result <- mo$generate_correlation_table(ref_df, comp_df,
                                                corr_types = list("pearson", "spearman", "kendall"))

  expect_true(is.na(corr_result$pearson.pval[2]))
  expect_true(is.na(corr_result$pearson.cor[2]))
  expect_true(is.na(corr_result$spearman.cor[2]))
  expect_true(is.na(corr_result$kendall.cor[2]))
})


test_that("get_full_entries detects infinite values", {
  df1 <- data.frame(
    feature = c("protA", "protB", "protC", "protD"),
    sample1 = c(1.0, Inf, 3.0, 4.0),
    sample2 = c(1.5, 2.5, -Inf, 4.5)
  )

  mo <- MapObject$new(df1, "feature", samples1 = c("sample1", "sample2"))

  full_entries <- mo$get_full_entries(mo$dataset1, mo$samples1)

  expect_true(full_entries[1])
  expect_false(full_entries[2])
  expect_false(full_entries[3])
  expect_true(full_entries[4])
})

test_that("get_full_entries handles mix of NA and Inf", {
  df1 <- data.frame(
    feature = c("protA", "protB", "protC", "protD", "protE"),
    sample1 = c(1.0, NA, Inf, NA, 5.0),
    sample2 = c(1.5, 2.5, 3.5, Inf, 5.5),
    sample3 = c(1.8, NA, NA, 4.8, 5.8)
  )

  mo <- MapObject$new(df1, "feature", samples1 = c("sample1", "sample2", "sample3"))

  full_entries <- mo$get_full_entries(mo$dataset1, mo$samples1)

  expect_equal(sum(full_entries), 2)
  expect_true(full_entries[1])
  expect_true(full_entries[5])
})


test_that("get_matching_dataset1 returns full dataset when no dataset2", {
  df1 <- data.frame(
    feature = c("protA", "protB", "protC"),
    sample1 = c(1.0, 2.0, 3.0)
  )

  mo <- MapObject$new(df1, "feature")

  matching <- mo$get_matching_dataset1()

  expect_equal(nrow(matching), 3)
  expect_identical(matching, mo$dataset1)
})


test_that("get_matching_dataset2 errors when dataset2 is NULL", {
  df1 <- data.frame(
    feature = c("protA", "protB"),
    sample1 = c(1.0, 2.0)
  )

  mo <- MapObject$new(df1, "feature")

  expect_error(mo$get_matching_dataset2(), "Second dataset not present")
})


test_that("has_full_entries works with dataset2 only", {
  df2 <- data.frame(
    protein = c("protA", "protB"),
    sampleA = c(1.0, 2.0)
  )

  mo <- MapObject$new(data.frame(feature = "X"), "feature")
  mo$dataset1 <- NULL
  mo$dataset2 <- df2
  mo$samples2 <- c("sampleA")
  mo$samples1 <- NULL

  expect_true(mo$has_full_entries())

  mo$samples2 <- NULL
  expect_false(mo$has_full_entries())
})

test_that("has_full_entries with dual datasets checks both sample sets", {
  df1 <- data.frame(feature = c("A"), sample1 = c(1.0))
  df2 <- data.frame(protein = c("A"), sampleA = c(1.0))

  mo1 <- MapObject$new(df1, "feature", df2, "protein",
                       samples1 = "sample1", samples2 = "sampleA")
  expect_true(mo1$has_full_entries())

  mo2 <- MapObject$new(df1, "feature", df2, "protein", samples1 = "sample1")
  expect_false(mo2$has_full_entries())

  mo3 <- MapObject$new(df1, "feature", df2, "protein", samples2 = "sampleA")
  expect_false(mo3$has_full_entries())

  mo4 <- MapObject$new(df1, "feature", df2, "protein")
  expect_false(mo4$has_full_entries())
})


test_that("initialize warns when matched=TRUE but sample counts differ", {
  df1 <- data.frame(
    feature = c("protA", "protB"),
    s1 = c(1.0, 2.0),
    s2 = c(1.5, 2.5),
    s3 = c(1.8, 2.8)
  )

  df2 <- data.frame(
    protein = c("protA", "protB"),
    sA = c(1.1, 2.1),
    sB = c(1.4, 2.4)
  )

  expect_warning(
    mo <- MapObject$new(df1, "feature", df2, "protein",
                        samples1 = c("s1", "s2", "s3"),
                        samples2 = c("sA", "sB"),
                        matched = TRUE),
    "Number of samples.*does not match"
  )

  expect_false(mo$has_correlations())
})

test_that("initialize sorts datasets by target column", {
  df1 <- data.frame(
    feature = c("protC", "protA", "protB"),
    sample1 = c(3.0, 1.0, 2.0)
  )

  mo <- MapObject$new(df1, "feature")

  expect_equal(mo$dataset1$feature, c("protA", "protB", "protC"))
  expect_equal(mo$dataset1$sample1, c(1.0, 2.0, 3.0))
})

test_that("initialize handles duplicates in dataset2", {
  df1 <- data.frame(
    feature = c("protA", "protB"),
    sample1 = c(1.0, 2.0)
  )

  df2 <- data.frame(
    protein = c("protA", "protA", "protB"),
    sampleA = c(1.1, 1.2, 2.1)
  )

  mo <- MapObject$new(df1, "feature", df2, "protein", discard_dups = TRUE)

  expect_equal(nrow(mo$dataset2), 2)
})

test_that("initialize warns for invalid samples2", {
  df1 <- data.frame(feature = c("A"), s1 = c(1.0))
  df2 <- data.frame(protein = c("A"), sA = c(1.0))

  mo <- expect_warning(
    MapObject$new(
      df1, "feature", df2, "protein",
      samples2 = c("sA", "invalid_sample")
    ),
    "Invalid samples, not all present in colnames"
  )
  expect_true(inherits(mo, "MapObject"))
})

test_that("initialize handles empty samples arrays", {
  df1 <- data.frame(feature = c("A"), s1 = c(1.0))

  mo1 <- MapObject$new(df1, "feature", samples1 = character(0))
  expect_null(mo1$samples1)

  mo2 <- MapObject$new(df1, "feature", samples1 = NULL)
  expect_null(mo2$samples1)
})


test_that("get_combined_dataset returns NULL when no combined entries", {
  df1 <- data.frame(feature = c("protA", "protB"), s1 = c(1.0, 2.0))
  df2 <- data.frame(protein = c("protC", "protD"), sA = c(3.0, 4.0))

  mo <- MapObject$new(df1, "feature", df2, "protein",
                      samples1 = "s1", samples2 = "sA")

  result <- mo$get_combined_dataset(include_one_dataset_entries = FALSE)
  expect_null(result)
})

test_that("get_combined_dataset returns NULL when empty after filtering", {
  df1 <- data.frame(
    feature = c("protA"),
    s1 = c(NA),
    s2 = c(NA)
  )

  df2 <- data.frame(
    protein = c("protA"),
    sA = c(NA),
    sB = c(NA)
  )

  mo <- MapObject$new(df1, "feature", df2, "protein",
                      samples1 = c("s1", "s2"),
                      samples2 = c("sA", "sB"))

  result <- mo$get_combined_dataset(only_no_na_entries = TRUE,
                                     include_one_dataset_entries = FALSE)
  expect_null(result)
})

test_that("get_combined_dataset includes correlations when available", {
  df1 <- data.frame(
    feature = c("protA", "protB"),
    s1 = c(1.0, 2.0)
  )

  df2 <- data.frame(
    protein = c("protA", "protB"),
    sA = c(1.1, 2.1)
  )

  mo <- MapObject$new(df1, "feature", df2, "protein",
                      samples1 = "s1", samples2 = "sA")

  mo$correlations <- data.frame(
    id = c("protA", "protB"),
    pearson.cor = c(0.95, 0.90),
    pearson.pval = c(0.01, 0.02)
  )

  result <- mo$get_combined_dataset(include_one_dataset_entries = FALSE)

  expect_true("pearson.cor" %in% colnames(result))
  expect_true("pearson.pval" %in% colnames(result))
})

test_that("get_combined_dataset parameter interaction: only_no_na overrides include_one", {
  df1 <- data.frame(feature = c("A", "B"), s1 = c(1.0, 2.0))
  df2 <- data.frame(protein = c("A"), sA = c(1.1))

  mo <- MapObject$new(df1, "feature", df2, "protein")

  result <- mo$get_combined_dataset(only_no_na_entries = TRUE,
                                     include_one_dataset_entries = TRUE)

  expect_equal(nrow(result), 1)
})

test_that("get_combined_dataset handles single dataset with only_no_na_entries", {
  df1 <- data.frame(
    feature = c("protA", "protB", "protC"),
    s1 = c(1.0, NA, 3.0),
    s2 = c(1.5, 2.5, 3.5)
  )

  mo <- MapObject$new(df1, "feature", samples1 = c("s1", "s2"))

  result <- mo$get_combined_dataset(only_no_na_entries = TRUE)

  expect_equal(nrow(result), 2)
  expect_true(all(result$d1.feature %in% c("protA", "protC")))
})

test_that("get_combined_dataset includes comb_id for all rows", {
  df1 <- data.frame(
    feature = c("protA", "protB", "protC"),
    s1 = c(1.0, 2.0, 3.0)
  )

  mo <- MapObject$new(df1, "feature")

  result <- mo$get_combined_dataset()

  expect_true("comb_id" %in% colnames(result))
  expect_equal(result$comb_id, c("C1", "C2", "C3"))
})


test_that("prepare_single_dataset renames columns correctly", {
  df1 <- data.frame(
    feature = c("A", "B"),
    sample1 = c(1.0, 2.0),
    sample2 = c(1.5, 2.5)
  )

  mo <- MapObject$new(df1, "feature", samples1 = c("sample1", "sample2"))

  result <- mo$prepare_single_dataset(mo$dataset1, mo$samples1, "test_prefix", FALSE)

  expect_true("test_prefix.feature" %in% colnames(result))
  expect_true("test_prefix.sample1" %in% colnames(result))
  expect_true("test_prefix.sample2" %in% colnames(result))
})

test_that("prepare_single_dataset handles different sample subsets", {
  df1 <- data.frame(
    feature = c("A", "B", "C"),
    s1 = c(1.0, 2.0, 3.0),
    s2 = c(1.5, 2.5, 3.5),
    s3 = c(1.8, 2.8, 3.8),
    extra_col = c("x", "y", "z")
  )

  mo <- MapObject$new(df1, "feature", samples1 = c("s1", "s3"))

  result <- mo$prepare_single_dataset(mo$dataset1, mo$samples1, "d1", FALSE)

  expect_equal(ncol(result), 5)
  expect_true("d1.s1" %in% colnames(result))
  expect_true("d1.s2" %in% colnames(result))
  expect_true("d1.s3" %in% colnames(result))
  expect_true("d1.feature" %in% colnames(result))
  expect_true("d1.extra_col" %in% colnames(result))
})


test_that("initialize calculates joint_indices correctly with partial overlap", {
  df1 <- data.frame(
    feature = c("A", "B", "C", "D"),
    value = 1:4
  )

  df2 <- data.frame(
    protein = c("B", "C", "E", "F"),
    value = 5:8
  )

  mo <- MapObject$new(df1, "feature", df2, "protein")

  expect_equal(length(mo$joint_indices1), 2)
  expect_equal(length(mo$joint_indices2), 2)
  expect_equal(mo$dataset1$feature[mo$joint_indices1], c("B", "C"))
  expect_equal(mo$dataset2$protein[mo$joint_indices2], c("B", "C"))
})

test_that("initialize handles complete overlap", {
  df1 <- data.frame(feature = c("A", "B", "C"), value = 1:3)
  df2 <- data.frame(protein = c("A", "B", "C"), value = 4:6)

  mo <- MapObject$new(df1, "feature", df2, "protein")

  expect_equal(length(mo$joint_indices1), 3)
  expect_equal(length(mo$joint_indices2), 3)
  expect_equal(mo$joint_indices1, 1:3)
  expect_equal(mo$joint_indices2, 1:3)
})

test_that("initialize handles no overlap", {
  df1 <- data.frame(feature = c("A", "B"), value = 1:2)
  df2 <- data.frame(protein = c("C", "D"), value = 3:4)

  mo <- MapObject$new(df1, "feature", df2, "protein")

  expect_equal(length(mo$joint_indices1), 0)
  expect_equal(length(mo$joint_indices2), 0)
})


test_that("has_correlations returns FALSE when correlations is NULL", {
  df1 <- data.frame(feature = c("A"), s1 = c(1.0))
  mo <- MapObject$new(df1, "feature")

  expect_false(mo$has_correlations())
  expect_null(mo$correlations)
})

test_that("has_correlations returns TRUE when correlations exist", {
  df1 <- data.frame(feature = c("A"), s1 = c(1.0))
  mo <- MapObject$new(df1, "feature")

  mo$correlations <- data.frame(id = "A", pearson.cor = 0.9)

  expect_true(mo$has_correlations())
})


test_that("datasets are sorted after initialization", {
  df1 <- data.frame(
    feature = c("Z", "A", "M", "B"),
    value = c(4, 1, 3, 2)
  )

  mo <- MapObject$new(df1, "feature")

  expect_equal(mo$dataset1$feature, c("A", "B", "M", "Z"))
  expect_equal(mo$dataset1$value, c(1, 2, 3, 4))
})

test_that("both datasets are sorted independently", {
  df1 <- data.frame(feature = c("C", "A", "B"), v1 = c(3, 1, 2))
  df2 <- data.frame(protein = c("B", "D", "A"), v2 = c(2, 4, 1))

  mo <- MapObject$new(df1, "feature", df2, "protein")

  expect_equal(mo$dataset1$feature, c("A", "B", "C"))
  expect_equal(mo$dataset2$protein, c("A", "B", "D"))
})
