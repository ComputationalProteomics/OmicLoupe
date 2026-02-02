test_that("make_pca_plt handles NULL color/shape (plotly build)", {
    set.seed(1)

    mat <- matrix(rnorm(4 * 20), nrow = 4)
    rownames(mat) <- paste0("S", 1:4)
    colnames(mat) <- paste0("F", 1:20)
    pca_obj <- prcomp(mat, center = TRUE, scale. = TRUE)

    ddf <- data.frame(
        sample = rownames(mat),
        group = c("A", "A", "B", "B"),
        batch = c("x", "y", "x", "y"),
        stringsAsFactors = FALSE
    )

    plt <- make_pca_plt(
        ddf = ddf,
        pca_obj = pca_obj,
        pc1 = 1,
        pc2 = 2,
        color = NULL,
        shape = NULL,
        sample = "sample",
        label_col = "sample",
        title_label = "Test",
        dot_size = 2,
        show_labels = FALSE,
        color_as_fact = TRUE
    )

    expect_s3_class(plt, "gg")
    expect_s3_class(plotly::ggplotly(plt), "plotly")
})

test_that("make_pca_plt handles color/shape mappings (plotly build)", {
    set.seed(2)

    mat <- matrix(rnorm(6 * 10), nrow = 6)
    rownames(mat) <- paste0("S", 1:6)
    colnames(mat) <- paste0("F", 1:10)
    pca_obj <- prcomp(mat, center = TRUE, scale. = TRUE)

    ddf <- data.frame(
        sample = rownames(mat),
        group = rep(c("A", "B"), each = 3),
        batch = rep(c("x", "y", "z"), 2),
        stringsAsFactors = FALSE
    )

    plt <- make_pca_plt(
        ddf = ddf,
        pca_obj = pca_obj,
        pc1 = 1,
        pc2 = 2,
        color = "group",
        shape = "batch",
        sample = "sample",
        label_col = "sample",
        title_label = "Test",
        dot_size = 2,
        show_labels = FALSE,
        color_as_fact = TRUE
    )

    expect_s3_class(plt, "gg")
    expect_s3_class(plotly::ggplotly(plt), "plotly")
})

test_that("make_pair_pca_plot handles NULL color", {
    set.seed(3)

    mat <- matrix(rnorm(5 * 12), nrow = 5)
    rownames(mat) <- paste0("S", 1:5)
    colnames(mat) <- paste0("F", 1:12)
    pca_obj <- prcomp(mat, center = TRUE, scale. = TRUE)

    ddf <- data.frame(
        sample = rownames(mat),
        group = c("A", "A", "B", "B", "B"),
        stringsAsFactors = FALSE
    )

    plt <- make_pair_pca_plot(
        ddf = ddf,
        pca_obj = pca_obj,
        color = NULL,
        color_as_fact = TRUE,
        pcs = 2
    )

    expect_s3_class(plt, "ggmatrix")
})

test_that("make_pair_pca_plot handles color mapping", {
    set.seed(4)

    mat <- matrix(rnorm(5 * 12), nrow = 5)
    rownames(mat) <- paste0("S", 1:5)
    colnames(mat) <- paste0("F", 1:12)
    pca_obj <- prcomp(mat, center = TRUE, scale. = TRUE)

    ddf <- data.frame(
        sample = rownames(mat),
        group = c("A", "A", "B", "B", "B"),
        stringsAsFactors = FALSE
    )

    plt <- make_pair_pca_plot(
        ddf = ddf,
        pca_obj = pca_obj,
        color = "group",
        color_as_fact = TRUE,
        pcs = 2
    )

    expect_s3_class(plt, "ggmatrix")
})

test_that("make_pca_plt errors on out-of-range PCs", {
    set.seed(5)

    mat <- matrix(rnorm(4 * 10), nrow = 4)
    rownames(mat) <- paste0("S", 1:4)
    colnames(mat) <- paste0("F", 1:10)
    pca_obj <- prcomp(mat, center = TRUE, scale. = TRUE)

    ddf <- data.frame(
        sample = rownames(mat),
        group = c("A", "A", "B", "B"),
        stringsAsFactors = FALSE
    )

    expect_error(
        make_pca_plt(
            ddf = ddf,
            pca_obj = pca_obj,
            pc1 = 10,
            pc2 = 2,
            color = NULL,
            shape = NULL,
            sample = "sample",
            label_col = "sample",
            title_label = "Test",
            dot_size = 2
        ),
        "only has"
    )
})

test_that("make_pair_pca_plot errors when requesting too many PCs", {
    set.seed(6)

    mat <- matrix(rnorm(3 * 10), nrow = 3)
    rownames(mat) <- paste0("S", 1:3)
    colnames(mat) <- paste0("F", 1:10)
    pca_obj <- prcomp(mat, center = TRUE, scale. = TRUE)

    ddf <- data.frame(
        sample = rownames(mat),
        group = c("A", "B", "B"),
        stringsAsFactors = FALSE
    )

    expect_error(
        make_pair_pca_plot(
            ddf = ddf,
            pca_obj = pca_obj,
            color = NULL,
            pcs = 10
        ),
        "only has"
    )
})

test_that("make_pca_plt errors on mismatched row counts", {
    set.seed(7)

    mat <- matrix(rnorm(4 * 10), nrow = 4)
    rownames(mat) <- paste0("S", 1:4)
    colnames(mat) <- paste0("F", 1:10)
    pca_obj <- prcomp(mat, center = TRUE, scale. = TRUE)

    ddf <- data.frame(
        sample = rownames(mat)[1:3],
        group = c("A", "A", "B"),
        stringsAsFactors = FALSE
    )

    expect_error(
        make_pca_plt(
            ddf = ddf,
            pca_obj = pca_obj,
            pc1 = 1,
            pc2 = 2,
            color = NULL,
            shape = NULL,
            sample = "sample",
            label_col = "sample",
            title_label = "Test",
            dot_size = 2
        ),
        "design data has"
    )
})
