
setup_test_data <- function() {
    df1 <- data.frame(
        id = c("A", "B", "C", "D"),
        sample1 = c(1.0, 2.0, 3.0, 4.0),
        sample2 = c(1.5, 2.5, 3.5, 4.5),
        sample3 = c(2.0, 3.0, 4.0, 5.0),
        stringsAsFactors = FALSE
    )

    df2 <- data.frame(
        id = c("A", "B", "C", "E"),
        sampleA = c(1.1, 2.1, 3.1, 4.1),
        sampleB = c(1.6, 2.6, 3.6, 4.6),
        sampleC = c(2.1, 3.1, 4.1, 5.1),
        stringsAsFactors = FALSE
    )

    list(df1 = df1, df2 = df2)
}

test_that("MapObject warns for invalid samples1", {
    data <- setup_test_data()

    obj <- expect_warning(
        MapObject$new(
            dataset1 = data$df1,
            target_col1 = "id",
            samples1 = c("sample1", "invalid_sample"),
            matched = FALSE
        ),
        "Invalid samples, not all present in colnames"
    )
    expect_true(inherits(obj, "MapObject"))

    obj <- expect_warning(
        MapObject$new(
            dataset1 = data$df1,
            target_col1 = "id",
            samples1 = c("bad1", "bad2"),
            matched = FALSE
        ),
        "Invalid samples, not all present in colnames"
    )
    expect_true(inherits(obj, "MapObject"))
})

test_that("MapObject warns for invalid samples2", {
    data <- setup_test_data()

    obj <- expect_warning(
        MapObject$new(
            dataset1 = data$df1,
            target_col1 = "id",
            dataset2 = data$df2,
            target_col2 = "id",
            samples1 = c("sample1", "sample2"),
            samples2 = c("sampleA", "invalid_sample"),
            matched = TRUE,
            skip_correlation = TRUE
        ),
        "Invalid samples, not all present in colnames"
    )
    expect_true(inherits(obj, "MapObject"))

    obj <- expect_warning(
        MapObject$new(
            dataset1 = data$df1,
            target_col1 = "id",
            dataset2 = data$df2,
            target_col2 = "id",
            samples1 = c("sample1", "sample2"),
            samples2 = c("bad1", "bad2"),
            matched = TRUE,
            skip_correlation = TRUE
        ),
        "Invalid samples, not all present in colnames"
    )
    expect_true(inherits(obj, "MapObject"))
})

test_that("MapObject accepts valid samples without error", {
    data <- setup_test_data()

    expect_silent({
        obj <- MapObject$new(
            dataset1 = data$df1,
            target_col1 = "id",
            samples1 = c("sample1", "sample2", "sample3"),
            matched = FALSE
        )
    })

    obj <- MapObject$new(
        dataset1 = data$df1,
        target_col1 = "id",
        samples1 = c("sample1", "sample2"),
        matched = FALSE
    )
    expect_equal(obj$samples1, c("sample1", "sample2"))
})

test_that("MapObject accepts valid samples for both datasets", {
    data <- setup_test_data()

    expect_message({
        obj <- MapObject$new(
            dataset1 = data$df1,
            target_col1 = "id",
            dataset2 = data$df2,
            target_col2 = "id",
            samples1 = c("sample1", "sample2"),
            samples2 = c("sampleA", "sampleB"),
            matched = TRUE,
            skip_correlation = TRUE
        )
    }, "skip_correlation")

    obj <- MapObject$new(
        dataset1 = data$df1,
        target_col1 = "id",
        dataset2 = data$df2,
        target_col2 = "id",
        samples1 = c("sample1", "sample2"),
        samples2 = c("sampleA", "sampleB"),
        matched = TRUE,
        skip_correlation = TRUE
    )
    expect_equal(obj$samples1, c("sample1", "sample2"))
    expect_equal(obj$samples2, c("sampleA", "sampleB"))
})

test_that("MapObject still warns for mismatched sample counts (informational)", {
    data <- setup_test_data()

    expect_warning(
        MapObject$new(
            dataset1 = data$df1,
            target_col1 = "id",
            dataset2 = data$df2,
            target_col2 = "id",
            samples1 = c("sample1", "sample2", "sample3"),
            samples2 = c("sampleA", "sampleB"),
            matched = TRUE
        ),
        "Number of samples.*does not match"
    )
})

test_that("MapObject handles NULL and empty samples gracefully", {
    data <- setup_test_data()

    expect_silent({
        obj <- MapObject$new(
            dataset1 = data$df1,
            target_col1 = "id",
            samples1 = NULL,
            matched = FALSE
        )
    })

    expect_silent({
        obj <- MapObject$new(
            dataset1 = data$df1,
            target_col1 = "id",
            samples1 = character(0),
            matched = FALSE
        )
    })
})
