make_pca_df <- function(n_features = 10,
                        samples = c("sample1", "sample2", "sample3"),
                        feature_col = "feature",
                        extra_col = FALSE) {
    out <- data.frame(
        stats::setNames(list(make_feature_ids(n_features)), feature_col),
        stringsAsFactors = FALSE
    )

    for (i in seq_along(samples)) {
        out[[samples[i]]] <- seq_len(n_features) + (i - 1) * 0.5
    }

    if (isTRUE(extra_col)) {
        out$extra_col <- rep("info", n_features)
    }

    out
}

test_that("calculate_pca_obj works with basic data", {
    samples <- c("sample1", "sample2", "sample3")
    test_data <- make_pca_df(n_features = 10, samples = samples)

    pca_result <- calculate_pca_obj(
        rdf = test_data,
        samples = samples,
        do_scale = TRUE,
        do_center = TRUE,
        var_cut = 0,
        return_df = FALSE
    )

    expect_s3_class(pca_result, "prcomp")
    expect_true("rotation" %in% names(pca_result))
    expect_true("x" %in% names(pca_result))
    expect_equal(nrow(pca_result$x), 3)
})

test_that("calculate_pca_obj handles variance filtering", {
    samples <- c("sample1", "sample2", "sample3")
    test_data <- make_pca_df(n_features = 20, samples = samples)
    test_data[1:10, samples] <- 1

    pca_result <- calculate_pca_obj(
        rdf = test_data,
        samples = samples,
        do_scale = TRUE,
        do_center = TRUE,
        var_cut = 0.5,
        return_df = FALSE
    )

    expect_s3_class(pca_result, "prcomp")
    expect_lt(nrow(pca_result$rotation), 20)
})

test_that("calculate_pca_obj handles missing values", {
    samples <- c("sample1", "sample2", "sample3")
    test_data <- make_pca_df(n_features = 10, samples = samples)
    test_data$sample1[3] <- NA

    pca_result <- calculate_pca_obj(
        rdf = test_data,
        samples = samples,
        do_scale = TRUE,
        do_center = TRUE,
        var_cut = 0,
        return_df = FALSE
    )

    expect_s3_class(pca_result, "prcomp")
    expect_lt(nrow(pca_result$rotation), 10)
    expect_gte(nrow(pca_result$rotation), 3)
})

test_that("calculate_pca_obj handles infinite values", {
    samples <- c("sample1", "sample2", "sample3")
    test_data <- make_pca_df(n_features = 10, samples = samples)
    test_data$sample1[3] <- Inf

    pca_result <- calculate_pca_obj(
        rdf = test_data,
        samples = samples,
        do_scale = TRUE,
        do_center = TRUE,
        var_cut = 0,
        return_df = FALSE
    )

    expect_s3_class(pca_result, "prcomp")
    expect_lt(nrow(pca_result$rotation), 10)
    expect_gte(nrow(pca_result$rotation), 3)
})

test_that("calculate_pca_obj returns dataframe when requested", {
    samples <- c("sample1", "sample2", "sample3")
    test_data <- make_pca_df(n_features = 10, samples = samples, extra_col = TRUE)

    result_df <- calculate_pca_obj(
        rdf = test_data,
        samples = samples,
        do_scale = TRUE,
        do_center = TRUE,
        var_cut = 0,
        return_df = TRUE
    )

    expect_s3_class(result_df, "data.frame")
    expect_true("feature" %in% colnames(result_df))
    expect_true(any(grepl("^PC", colnames(result_df))))
})

test_that("calculate_pca_obj applies column prefix correctly", {
    samples <- c("sample1", "sample2", "sample3")
    test_data <- make_pca_df(n_features = 10, samples = samples)

    result_df <- calculate_pca_obj(
        rdf = test_data,
        samples = samples,
        do_scale = TRUE,
        do_center = TRUE,
        var_cut = 0,
        return_df = TRUE,
        col_prefix = "d1."
    )

    expect_s3_class(result_df, "data.frame")
    expect_true(any(grepl("^d1\\.PC", colnames(result_df))))
})

test_that("calculate_pca_obj respects scale and center parameters", {
    samples <- c("sample1", "sample2", "sample3")
    test_data <- make_pca_df(n_features = 10, samples = samples)
    test_data$sample2 <- test_data$sample2 * 10
    test_data$sample3 <- test_data$sample3 * 100

    pca_scaled <- calculate_pca_obj(
        rdf = test_data,
        samples = samples,
        do_scale = TRUE,
        do_center = TRUE,
        var_cut = 0,
        return_df = FALSE
    )

    pca_unscaled <- calculate_pca_obj(
        rdf = test_data,
        samples = samples,
        do_scale = FALSE,
        do_center = TRUE,
        var_cut = 0,
        return_df = FALSE
    )

    expect_false(identical(pca_scaled$x, pca_unscaled$x))
})

test_that("pca_calculation validates input correctly", {
    data_with_na <- matrix(c(1, 2, NA, 4), nrow = 2)

    expect_error(
        pca_calculation(data_with_na),
        "Data contains missing values"
    )
})

test_that("pca_calculation transposes data correctly", {
    test_matrix <- matrix(
        c(1, 2, 3,
          4, 5, 6,
          7, 8, 9),
        nrow = 3,
        byrow = TRUE
    )

    result <- pca_calculation(test_matrix, center = TRUE, scale = TRUE)

    expect_s3_class(result, "prcomp")
    expect_equal(nrow(result$x), 3)
    expect_equal(nrow(result$rotation), 3)
})

test_that("pca_calculation respects center and scale parameters", {
    test_matrix <- matrix(seq_len(30), nrow = 10)

    pca_centered_scaled <- pca_calculation(test_matrix, center = TRUE, scale = TRUE)

    pca_no_center <- pca_calculation(test_matrix, center = FALSE, scale = FALSE)

    expect_s3_class(pca_centered_scaled, "prcomp")
    expect_s3_class(pca_no_center, "prcomp")

    expect_false(identical(pca_centered_scaled$x, pca_no_center$x))
})

test_that("calculate_pca_obj errors clearly on zero-variance data", {
    test_data <- data.frame(
        feature = paste0("F", 1:5),
        sample1 = rep(1, 5),
        sample2 = rep(1, 5),
        sample3 = rep(1, 5),
        stringsAsFactors = FALSE
    )

    expect_error(
        calculate_pca_obj(
            rdf = test_data,
            samples = c("sample1", "sample2", "sample3"),
            do_scale = TRUE,
            do_center = TRUE,
            var_cut = 0.1,
            return_df = FALSE
        ),
        "non-zero variance"
    )
})
