
test_that("MapObject initializes with single dataset", {
  df1 <- data.frame(
    feature = c("protA", "protB", "protC"),
    sample1 = c(1.0, 2.0, 3.0),
    sample2 = c(1.5, 2.5, 3.5),
    stringsAsFactors = FALSE
  )

  mo <- MapObject$new(df1, "feature")

  expect_false(mo$dual_datasets)
  expect_equal(nrow(mo$dataset1), 3)
  expect_equal(mo$target_col1, "feature")
  expect_null(mo$dataset2)
})

test_that("MapObject initializes with dual datasets", {
  df1 <- data.frame(
    feature = c("protA", "protB", "protC"),
    sample1 = c(1.0, 2.0, 3.0),
    stringsAsFactors = FALSE
  )

  df2 <- data.frame(
    protein = c("protA", "protB", "protD"),
    sampleA = c(1.2, 2.2, 4.0),
    stringsAsFactors = FALSE
  )

  mo <- MapObject$new(df1, "feature", df2, "protein")

  expect_true(mo$dual_datasets)
  expect_equal(nrow(mo$dataset1), 3)
  expect_equal(nrow(mo$dataset2), 3)
  expect_equal(mo$target_col1, "feature")
  expect_equal(mo$target_col2, "protein")

  expect_equal(length(mo$joint_indices1), 2)
  expect_equal(length(mo$joint_indices2), 2)
})

test_that("MapObject handles samples selection", {
  df1 <- data.frame(
    feature = c("protA", "protB"),
    sample1 = c(1.0, 2.0),
    sample2 = c(1.5, 2.5),
    sample3 = c(1.8, 2.8),
    stringsAsFactors = FALSE
  )

  mo <- MapObject$new(df1, "feature", samples1 = c("sample1", "sample2"))

  expect_equal(mo$samples1, c("sample1", "sample2"))
  expect_true(mo$has_full_entries())
})

test_that("MapObject validates invalid samples with warning", {
  df1 <- data.frame(
    feature = c("protA", "protB"),
    sample1 = c(1.0, 2.0),
    stringsAsFactors = FALSE
  )

  mo <- expect_warning(
    MapObject$new(df1, "feature", samples1 = c("sample1", "invalid_sample")),
    "Invalid samples, not all present in colnames"
  )
  expect_true(inherits(mo, "MapObject"))
})

test_that("MapObject get_combined_dataset works for single dataset", {
  df1 <- data.frame(
    feature = c("protA", "protB", "protC"),
    sample1 = c(1.0, 2.0, 3.0),
    sample2 = c(1.5, 2.5, 3.5),
    stringsAsFactors = FALSE
  )

  mo <- MapObject$new(df1, "feature", samples1 = c("sample1", "sample2"))

  comb_df <- mo$get_combined_dataset()

  expect_true(!is.null(comb_df))
  expect_equal(nrow(comb_df), 3)
  expect_true("comb_id" %in% colnames(comb_df))
  expect_true("d1.feature" %in% colnames(comb_df))
})

test_that("MapObject get_combined_dataset works for dual datasets", {
  df1 <- data.frame(
    feature = c("protA", "protB", "protC"),
    sample1 = c(1.0, 2.0, 3.0),
    stringsAsFactors = FALSE
  )

  df2 <- data.frame(
    protein = c("protA", "protB", "protD"),
    sampleA = c(1.2, 2.2, 4.0),
    stringsAsFactors = FALSE
  )

  mo <- MapObject$new(df1, "feature", df2, "protein",
                      samples1 = "sample1", samples2 = "sampleA")

  comb_df <- mo$get_combined_dataset(only_no_na_entries = FALSE,
                                     include_one_dataset_entries = FALSE)

  expect_true(!is.null(comb_df))
  expect_equal(nrow(comb_df), 2)
  expect_true("comb_id" %in% colnames(comb_df))
  expect_true("d1.feature" %in% colnames(comb_df))
  expect_true("d2.protein" %in% colnames(comb_df))
})

test_that("MapObject handles duplicate features", {
  df1 <- data.frame(
    feature = c("protA", "protA", "protB"),
    sample1 = c(1.0, 1.1, 2.0),
    stringsAsFactors = FALSE
  )

  mo <- MapObject$new(df1, "feature", discard_dups = TRUE)

  expect_equal(nrow(mo$dataset1), 2)
})

test_that("MapObject get_matching_dataset methods work", {
  df1 <- data.frame(
    feature = c("protA", "protB", "protC"),
    sample1 = c(1.0, 2.0, 3.0),
    stringsAsFactors = FALSE
  )

  df2 <- data.frame(
    protein = c("protA", "protB", "protD"),
    sampleA = c(1.2, 2.2, 4.0),
    stringsAsFactors = FALSE
  )

  mo <- MapObject$new(df1, "feature", df2, "protein")

  match1 <- mo$get_matching_dataset1()
  match2 <- mo$get_matching_dataset2()

  expect_equal(nrow(match1), 2)
  expect_equal(nrow(match2), 2)
  expect_true(all(match1$feature %in% c("protA", "protB")))
  expect_true(all(match2$protein %in% c("protA", "protB")))
})

test_that("MapObject has_combined method works", {
  df1 <- data.frame(
    feature = c("protA", "protB"),
    sample1 = c(1.0, 2.0),
    stringsAsFactors = FALSE
  )

  df2 <- data.frame(
    protein = c("protC", "protD"),
    sampleA = c(1.2, 2.2),
    stringsAsFactors = FALSE
  )

  mo_no_overlap <- MapObject$new(df1, "feature", df2, "protein")

  expect_false(mo_no_overlap$has_combined())

  df3 <- data.frame(
    protein = c("protA", "protB"),
    sampleA = c(1.2, 2.2),
    stringsAsFactors = FALSE
  )

  mo_with_overlap <- MapObject$new(df1, "feature", df3, "protein")
  expect_true(mo_with_overlap$has_combined())
})

test_that("MapObject get_full_entries identifies complete cases", {
  df1 <- data.frame(
    feature = c("protA", "protB", "protC"),
    sample1 = c(1.0, NA, 3.0),
    sample2 = c(1.5, 2.5, 3.5),
    stringsAsFactors = FALSE
  )

  mo <- MapObject$new(df1, "feature", samples1 = c("sample1", "sample2"))

  full_entries <- mo$get_full_entries(mo$dataset1, mo$samples1)

  expect_equal(sum(full_entries), 2)
  expect_false(full_entries[2])
})

test_that("MapObject dataset size methods work", {
  df1 <- data.frame(
    feature = c("protA", "protB", "protC"),
    sample1 = c(1.0, 2.0, 3.0),
    stringsAsFactors = FALSE
  )

  df2 <- data.frame(
    protein = c("protA", "protB"),
    sampleA = c(1.2, 2.2),
    stringsAsFactors = FALSE
  )

  mo <- MapObject$new(df1, "feature", df2, "protein")

  expect_equal(mo$get_dataset1_nrow(), 3)
  expect_equal(mo$get_dataset2_nrow(), 2)
})
