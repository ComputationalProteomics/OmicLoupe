test_that("get_ordered_sets orders by frequency", {
    test_data <- data.frame(
        Set1 = c(1, 1, 0, 1, 0, 0, 1, 1),
        Set2 = c(1, 0, 1, 1, 0, 1, 0, 0),
        Set3 = c(0, 1, 1, 1, 0, 0, 0, 1),
        feature_id = paste0("F", 1:8)
    )

    name_order <- c("Set1", "Set2", "Set3")

    result <- get_ordered_sets(
        upset_list = test_data,
        order_on = "freq",
        name_order = name_order,
        omit_empty = TRUE
    )

    expect_s3_class(result, "data.frame")
    expect_true("nbr" %in% colnames(result))
    expect_true("grade" %in% colnames(result))
    expect_true("union_contrast_string" %in% colnames(result))

    expect_true(all(diff(result$nbr) <= 0))
})

test_that("get_ordered_sets orders by degree", {
    test_data <- data.frame(
        Set1 = c(1, 1, 0, 1, 0, 0, 1, 1),
        Set2 = c(1, 0, 1, 1, 0, 1, 0, 0),
        Set3 = c(0, 1, 1, 1, 0, 0, 0, 1),
        feature_id = paste0("F", 1:8)
    )

    name_order <- c("Set1", "Set2", "Set3")

    result <- get_ordered_sets(
        upset_list = test_data,
        order_on = "degree",
        name_order = name_order,
        omit_empty = TRUE
    )

    expect_s3_class(result, "data.frame")

    expect_true(all(diff(result$grade) <= 0))
})

test_that("get_ordered_sets handles empty sets", {
    test_data <- data.frame(
        Set1 = c(1, 1, 0, 0, 0),
        Set2 = c(1, 0, 1, 0, 0),
        Set3 = c(0, 1, 1, 0, 0),
        feature_id = paste0("F", 1:5)
    )

    name_order <- c("Set1", "Set2", "Set3")

    result_omit <- get_ordered_sets(
        upset_list = test_data,
        order_on = "freq",
        name_order = name_order,
        omit_empty = TRUE
    )

    expect_s3_class(result_omit, "data.frame")
    expect_true(all(result_omit$grade > 0))

    result_keep <- get_ordered_sets(
        upset_list = test_data,
        order_on = "freq",
        name_order = name_order,
        omit_empty = FALSE
    )

    expect_s3_class(result_keep, "data.frame")
    expect_gte(nrow(result_keep), nrow(result_omit))
})

test_that("get_ordered_sets includes correct set names", {
    test_data <- data.frame(
        Set1 = c(1, 1, 0),
        Set2 = c(1, 0, 1),
        Set3 = c(0, 1, 1),
        feature_id = paste0("F", 1:3)
    )

    name_order <- c("Set1", "Set2", "Set3")

    result <- get_ordered_sets(
        upset_list = test_data,
        order_on = "freq",
        name_order = name_order,
        omit_empty = TRUE
    )

    expect_s3_class(result, "data.frame")
    expect_true("string_entries" %in% colnames(result))
    expect_true("included_entries" %in% colnames(result))
})

test_that("get_ordered_sets errors on unknown ordering", {
    test_data <- data.frame(
        Set1 = c(1, 1, 0),
        Set2 = c(1, 0, 1),
        feature_id = paste0("F", 1:3)
    )

    name_order <- c("Set1", "Set2")

    expect_error(
        get_ordered_sets(
            upset_list = test_data,
            order_on = "invalid_order",
            name_order = name_order,
            omit_empty = TRUE
        ),
        "Unknown ordering condition"
    )
})

test_that("upset_get_plot_list processes names correctly", {
    names_list <- list(
        list(a = c("F1", "F2"), b = c("F3", "F4")),
        list(a = c("F5"), b = c("F6", "F7"))
    )

    comparisons <- c("Comparison1.", "Comparison2.")

    result <- upset_get_plot_list(
        names_list = names_list,
        comparisons = comparisons,
        split_on_fold = FALSE
    )

    expect_type(result, "list")
    expect_equal(length(result), 2)
    expect_true("Comparison1" %in% names(result))
    expect_true("Comparison2" %in% names(result))
})

test_that("upset_get_plot_list handles fold splitting", {
    names_list <- list(
        list(up = c("F1", "F2"), down = c("F3", "F4")),
        list(up = c("F5"), down = c("F6", "F7"))
    )

    comparisons <- c("Comparison1.", "Comparison2.")

    result <- upset_get_plot_list(
        names_list = names_list,
        comparisons = comparisons,
        split_on_fold = TRUE
    )

    expect_type(result, "list")
    expect_gt(length(result), 2)
})

test_that("upset_get_name_order returns correct order without fold split", {
    plot_list <- list(
        "Set1" = c("F1", "F2"),
        "Set2" = c("F3", "F4"),
        "Set3" = c("F5")
    )

    result <- upset_get_name_order(
        plot_list = plot_list,
        split_on_fold = FALSE
    )

    expect_equal(result, names(plot_list))
    expect_equal(length(result), 3)
})

test_that("upset_get_name_order reorders with fold split", {
    plot_list <- list(
        "Set1.up" = c("F1", "F2"),
        "Set1.down" = c("F3", "F4"),
        "Set2.up" = c("F5"),
        "Set2.down" = c("F6")
    )

    result <- upset_get_name_order(
        plot_list = plot_list,
        split_on_fold = TRUE
    )

    expect_equal(length(result), 4)
    expect_equal(result[1], "Set1.up")
    expect_equal(result[2], "Set2.up")
    expect_equal(result[3], "Set1.down")
    expect_equal(result[4], "Set2.down")
})

test_that("upset_get_metadata creates metadata without fold split", {
    plot_list <- list(
        "Set1" = c("F1", "F2"),
        "Set2" = c("F3", "F4")
    )

    result <- upset_get_metadata(
        plot_list = plot_list,
        split_on_fold = FALSE
    )

    expect_type(result, "list")
    expect_true("data" %in% names(result))
    expect_s3_class(result$data, "data.frame")
    expect_true("comparison" %in% colnames(result$data))
    expect_true("dataset" %in% colnames(result$data))
    expect_equal(nrow(result$data), 2)
})

test_that("upset_get_metadata creates metadata with fold split", {
    plot_list <- list(
        "Set1.up" = c("F1", "F2"),
        "Set1.down" = c("F3", "F4"),
        "Set2.up" = c("F5"),
        "Set2.down" = c("F6")
    )

    result <- upset_get_metadata(
        plot_list = plot_list,
        split_on_fold = TRUE
    )

    expect_type(result, "list")
    expect_true("data" %in% names(result))
    expect_true("plots" %in% names(result))
    expect_s3_class(result$data, "data.frame")
    expect_true("comparison" %in% colnames(result$data))
    expect_true("fold_dir" %in% colnames(result$data))
    expect_equal(nrow(result$data), 4)

    expect_equal(result$data$fold_dir, rep(c("up", "down"), 2))

    expect_type(result$plots, "list")
    expect_equal(result$plots[[1]]$type, "matrix_rows")
    expect_equal(result$plots[[1]]$column, "fold_dir")
})

