

test_that("MapObject skips correlations when requested", {
    ref_df <- data.frame(
        feature = c("protA", "protB"),
        sample1 = c(1.0, 2.0)
    )

    comp_df <- data.frame(
        feature = c("protA", "protB"),
        sampleA = c(1.1, 2.1)
    )

    mo <- MapObject$new(
        dataset1 = ref_df,
        target_col1 = "feature",
        dataset2 = comp_df,
        target_col2 = "feature",
        samples1 = c("sample1"),
        samples2 = c("sampleA"),
        matched = TRUE,
        skip_correlation = TRUE
    )

    expect_false(mo$has_correlations())
    expect_null(mo$correlation_tbl)
})

test_that("MapObject handles non-matched datasets without correlations", {
    ref_df <- data.frame(
        feature = c("protA", "protB"),
        sample1 = c(1.0, 2.0)
    )

    comp_df <- data.frame(
        feature = c("protA", "protB"),
        sampleA = c(1.1, 2.1)
    )

    mo <- MapObject$new(
        dataset1 = ref_df,
        target_col1 = "feature",
        dataset2 = comp_df,
        target_col2 = "feature",
        samples1 = c("sample1"),
        samples2 = c("sampleA"),
        matched = FALSE
    )

    expect_false(mo$has_correlations())
})

test_that("MapObject prepare_single_dataset handles only_no_na_entries", {
    df1 <- data.frame(
        feature = c("protA", "protB", "protC", "protD"),
        sample1 = c(1.0, NA, 3.0, 4.0),
        sample2 = c(1.5, 2.5, NA, 4.5)
    )

    mo <- MapObject$new(df1, "feature", samples1 = c("sample1", "sample2"))

    result_with_na <- mo$prepare_single_dataset(
        out_df = mo$dataset1,
        samples = mo$samples1,
        sample_prefix = "d1",
        only_no_na_entries = FALSE
    )

    expect_equal(nrow(result_with_na), 4)

    result_no_na <- mo$prepare_single_dataset(
        out_df = mo$dataset1,
        samples = mo$samples1,
        sample_prefix = "d1",
        only_no_na_entries = TRUE
    )

    expect_equal(nrow(result_no_na), 2)
})

test_that("MapObject get_combined_dataset includes one dataset entries", {
    df1 <- data.frame(
        feature = c("protA", "protB", "protC"),
        sample1 = c(1.0, 2.0, 3.0)
    )

    df2 <- data.frame(
        protein = c("protA", "protD"),
        sampleA = c(1.1, 4.0)
    )

    mo <- MapObject$new(df1, "feature", df2, "protein")

    comb_with_one <- mo$get_combined_dataset(
        only_no_na_entries = FALSE,
        include_one_dataset_entries = TRUE
    )

    expect_gte(nrow(comb_with_one), 4)

    comb_without_one <- mo$get_combined_dataset(
        only_no_na_entries = FALSE,
        include_one_dataset_entries = FALSE
    )

    expect_equal(nrow(comb_without_one), 1)
    expect_lt(nrow(comb_without_one), nrow(comb_with_one))
})

test_that("MapObject handles duplicate feature removal", {
    df1 <- data.frame(
        feature = c("protA", "protB", "protA", "protC"),
        sample1 = c(1.0, 2.0, 1.5, 3.0)
    )

    mo_discard <- MapObject$new(df1, "feature", discard_dups = TRUE)

    expect_equal(nrow(mo_discard$dataset1), 3)

    mo_keep <- MapObject$new(df1, "feature", discard_dups = FALSE)

    expect_equal(nrow(mo_keep$dataset1), 4)
})

test_that("MapObject has_same_number_entries checks joint indices", {

    df1 <- data.frame(
        feature = c("protA", "protB", "protC"),
        sample1 = c(1.0, 2.0, 3.0)
    )

    df2_all_match <- data.frame(
        protein = c("protA", "protB", "protC"),
        sampleA = c(1.1, 2.1, 3.1)
    )

    df2_partial_match <- data.frame(
        protein = c("protA", "protB"),
        sampleA = c(1.1, 2.1)
    )

    mo_all <- MapObject$new(df1, "feature", df2_all_match, "protein")
    expect_true(mo_all$has_same_number_entries())
    expect_equal(length(mo_all$joint_indices1), length(mo_all$joint_indices2))

    mo_partial <- MapObject$new(df1, "feature", df2_partial_match, "protein")
    expect_true(mo_partial$has_same_number_entries())
    expect_equal(length(mo_partial$joint_indices1), 2)
    expect_equal(length(mo_partial$joint_indices2), 2)
})

test_that("MapObject handles single dataset without samples specified", {
    df1 <- data.frame(
        feature = c("protA", "protB"),
        value1 = c(1.0, 2.0),
        value2 = c(1.5, 2.5)
    )

    mo <- MapObject$new(df1, "feature")

    expect_false(mo$has_full_entries())
    expect_null(mo$samples1)
})

test_that("MapObject column prefixes work correctly", {
    df1 <- data.frame(
        feature = c("protA", "protB"),
        sample1 = c(1.0, 2.0)
    )

    df2 <- data.frame(
        protein = c("protA", "protB"),
        sampleA = c(1.1, 2.1)
    )

    mo <- MapObject$new(
        df1, "feature", df2, "protein",
        samples1 = c("sample1"),
        samples2 = c("sampleA")
    )

    comb_df <- mo$get_combined_dataset()

    expect_true(any(grepl("^d1\\.", colnames(comb_df))))
    expect_true(any(grepl("^d2\\.", colnames(comb_df))))
    expect_true("comb_id" %in% colnames(comb_df))
})

test_that("MapObject handles empty joint indices", {
    df1 <- data.frame(
        feature = c("protA", "protB"),
        sample1 = c(1.0, 2.0)
    )

    df2 <- data.frame(
        protein = c("protC", "protD"),
        sampleA = c(3.0, 4.0)
    )

    mo <- MapObject$new(df1, "feature", df2, "protein")

    expect_equal(length(mo$joint_indices1), 0)
    expect_equal(length(mo$joint_indices2), 0)
    expect_false(mo$has_combined())
})


test_that("MapObject handles all NA samples in prepare_single_dataset", {
    df1 <- data.frame(
        feature = c("protA", "protB", "protC"),
        sample1 = c(NA, NA, NA),
        sample2 = c(NA, NA, NA)
    )

    mo <- MapObject$new(df1, "feature", samples1 = c("sample1", "sample2"))

    result <- mo$prepare_single_dataset(
        out_df = mo$dataset1,
        samples = mo$samples1,
        sample_prefix = "d1",
        only_no_na_entries = TRUE
    )

    expect_equal(nrow(result), 0)
})

test_that("MapObject get_combined_dataset handles only_no_na_entries correctly", {
    df1 <- data.frame(
        feature = c("protA", "protB", "protC"),
        sample1 = c(1.0, NA, 3.0),
        sample2 = c(1.5, 2.5, 3.5)
    )

    df2 <- data.frame(
        protein = c("protA", "protB", "protC"),
        sampleA = c(1.1, 2.1, NA),
        sampleB = c(1.4, 2.4, 3.4)
    )

    mo <- MapObject$new(
        df1, "feature", df2, "protein",
        samples1 = c("sample1", "sample2"),
        samples2 = c("sampleA", "sampleB")
    )

    comb_no_na <- mo$get_combined_dataset(
        only_no_na_entries = TRUE,
        include_one_dataset_entries = FALSE
    )

    expect_equal(nrow(comb_no_na), 1)

    comb_with_na <- mo$get_combined_dataset(
        only_no_na_entries = FALSE,
        include_one_dataset_entries = FALSE
    )

    expect_equal(nrow(comb_with_na), 3)
})
