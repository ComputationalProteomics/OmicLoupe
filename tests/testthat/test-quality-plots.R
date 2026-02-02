make_boxplot_input <- function() {
    conditions <- c("Control", "Treated", "Control")
    df <- make_sample_long_df(
        sample_col = "Sample",
        value_col = "Abundance",
        n_per_sample = 10,
        bases = c(5, 6, 7)
    )
    df$Condition <- rep(conditions, each = 10)

    list(
        df = df,
        base_plot = ggplot(df, aes(x = Sample, y = Abundance)),
        ddf = make_sample_design_df(sample_col = "Sample", condition_col = "Condition")
    )
}

test_that("adjust_boxplot creates boxplot by default", {
    input <- make_boxplot_input()

    result_plot <- adjust_boxplot(
        plt = input$base_plot,
        do_violin = FALSE,
        rotate_x_labels = FALSE,
        order_on_condition = FALSE,
        ddf = input$ddf,
        ddf_sample_col = "Sample",
        ddf_cond_col = "Condition",
        text_size = 12
    )

    expect_s3_class(result_plot, "gg")
    expect_s3_class(result_plot, "ggplot")

    geom_names <- sapply(result_plot$layers, function(x) class(x$geom)[1])
    expect_true("GeomBoxplot" %in% geom_names)
})

test_that("adjust_boxplot creates violin plot when requested", {
    input <- make_boxplot_input()

    result_plot <- adjust_boxplot(
        plt = input$base_plot,
        do_violin = TRUE,
        rotate_x_labels = FALSE,
        order_on_condition = FALSE,
        ddf = input$ddf,
        ddf_sample_col = "Sample",
        ddf_cond_col = "Condition",
        text_size = 12
    )

    expect_s3_class(result_plot, "gg")

    geom_names <- sapply(result_plot$layers, function(x) class(x$geom)[1])
    expect_true("GeomViolin" %in% geom_names)
})

test_that("adjust_boxplot rotates x-axis labels", {
    input <- make_boxplot_input()

    result_plot <- adjust_boxplot(
        plt = input$base_plot,
        do_violin = FALSE,
        rotate_x_labels = TRUE,
        order_on_condition = FALSE,
        ddf = input$ddf,
        ddf_sample_col = "Sample",
        ddf_cond_col = "Condition",
        text_size = 12
    )

    expect_s3_class(result_plot, "gg")

    expect_true(!is.null(result_plot$theme$axis.text.x))
})

test_that("adjust_boxplot sets custom labels", {
    input <- make_boxplot_input()

    result_plot <- adjust_boxplot(
        plt = input$base_plot,
        do_violin = FALSE,
        rotate_x_labels = FALSE,
        order_on_condition = FALSE,
        ddf = input$ddf,
        ddf_sample_col = "Sample",
        ddf_cond_col = "Condition",
        text_size = 12,
        xlab = "Custom X",
        ylab = "Custom Y"
    )

    expect_s3_class(result_plot, "gg")
    expect_equal(result_plot$labels$x, "Custom X")
    expect_equal(result_plot$labels$y, "Custom Y")
})

make_density_input <- function(with_color = TRUE) {
    conditions <- c("Control", "Treated", "Control")
    df <- make_sample_long_df(n_per_sample = 100, bases = c(5, 6, 7))
    if (with_color) {
        df$Condition <- rep(conditions, each = 100)
    }
    df
}

test_that("make_density_plot creates plotly object", {
    test_df <- make_density_input(with_color = TRUE)

    result_plot <- make_density_plot(
        sdf = test_df,
        color = "Condition",
        curr_dataset = "Dataset1",
        title = NULL,
        text_size = 10
    )

    expect_s3_class(result_plot, "plotly")
})

test_that("make_density_plot handles custom title", {
    test_df <- make_density_input(with_color = TRUE)

    custom_title <- "My Custom Density Plot"
    result_plot <- make_density_plot(
        sdf = test_df,
        color = "Condition",
        title = custom_title,
        text_size = 10
    )

    expect_s3_class(result_plot, "plotly")
    expect_true(any(grepl(custom_title, as.character(result_plot))))
})

test_that("make_density_plot handles custom axis labels", {
    test_df <- make_density_input(with_color = TRUE)

    result_plot <- make_density_plot(
        sdf = test_df,
        color = "Condition",
        text_size = 10,
        xlab = "Custom X",
        ylab = "Custom Y"
    )

    expect_s3_class(result_plot, "plotly")
})

test_that("make_density_plot handles NULL color", {
    test_df <- make_density_input(with_color = FALSE)

    result_plot <- make_density_plot(
        sdf = test_df,
        color = NULL,
        curr_dataset = "Dataset1",
        text_size = 10
    )

    expect_s3_class(result_plot, "plotly")
})

test_that("do_dendrogram creates ggplot object", {
    test_matrix <- make_sample_matrix(n_features = 10, n_samples = 10)

    color_levels <- rep(c("Control", "Treated"), 5)

    result_plot <- do_dendrogram(
        raw_data_m = test_matrix,
        raw_color_levels = color_levels,
        title = "Test Dendrogram"
    )

    expect_s3_class(result_plot, "gg")
    expect_s3_class(result_plot, "ggplot")
})

test_that("do_dendrogram handles custom labels", {
    test_matrix <- make_sample_matrix(n_features = 6, n_samples = 10)

    color_levels <- rep(c("Control", "Treated"), 5)
    custom_labels <- paste0("Sample_", 1:10)

    result_plot <- do_dendrogram(
        raw_data_m = test_matrix,
        raw_color_levels = color_levels,
        labels = custom_labels,
        title = "Test Dendrogram"
    )

    expect_s3_class(result_plot, "gg")
})

test_that("do_dendrogram handles sample omission", {
    test_matrix <- make_sample_matrix(n_features = 6, n_samples = 10)

    color_levels <- rep(c("Control", "Treated"), 5)

    result_plot <- do_dendrogram(
        raw_data_m = test_matrix,
        raw_color_levels = color_levels,
        omit_samples = c("S1", "S2"),
        title = "Test Dendrogram"
    )

    expect_s3_class(result_plot, "gg")
})

test_that("do_dendrogram handles missing values", {
    test_matrix <- make_sample_matrix(n_features = 6, n_samples = 10)

    test_matrix[1, 1] <- NA
    test_matrix[2, 3] <- NA

    color_levels <- rep(c("Control", "Treated"), 5)

    result_plot <- do_dendrogram(
        raw_data_m = test_matrix,
        raw_color_levels = color_levels,
        title = "Test Dendrogram"
    )

    expect_s3_class(result_plot, "gg")
})

test_that("make_barplot creates summed abundance plot", {
    long_df <- make_sample_long_df(n_per_sample = 10, bases = c(5, 6, 7))
    design_df <- make_sample_design_df()

    result_plot <- make_barplot(
        long_sdf = long_df,
        value_col = "value",
        ddf = design_df,
        sample_col = "Sample",
        dataset = "Dataset1",
        color = "Condition",
        show_missing = FALSE
    )

    expect_s3_class(result_plot, "gg")
    expect_s3_class(result_plot, "ggplot")

    geom_names <- sapply(result_plot$layers, function(x) class(x$geom)[1])
    expect_true("GeomCol" %in% geom_names)
})

test_that("make_barplot creates missing values plot", {
    long_df <- make_sample_long_df(n_per_sample = 10, bases = c(1, 1, 1))
    long_df$value[3] <- NA
    long_df$value[14] <- NA
    long_df$value[15] <- NA

    design_df <- make_sample_design_df()

    result_plot <- make_barplot(
        long_sdf = long_df,
        value_col = "value",
        ddf = design_df,
        sample_col = "Sample",
        dataset = "Dataset1",
        color = "Condition",
        show_missing = TRUE
    )

    expect_s3_class(result_plot, "gg")

    geom_names <- sapply(result_plot$layers, function(x) class(x$geom)[1])
    expect_true("GeomCol" %in% geom_names)
})

test_that("make_barplot handles infinite values", {
    long_df <- make_sample_long_df(n_per_sample = 10, bases = c(1, 20, 40))
    long_df$value[3] <- Inf

    design_df <- make_sample_design_df()

    result_plot <- make_barplot(
        long_sdf = long_df,
        value_col = "value",
        ddf = design_df,
        sample_col = "Sample",
        dataset = "Dataset1",
        color = "Condition",
        show_missing = FALSE
    )

    expect_s3_class(result_plot, "gg")
})

test_that("make_barplot rotates labels when requested", {
    long_df <- make_sample_long_df(n_per_sample = 10, bases = c(5, 6, 7))
    design_df <- make_sample_design_df()

    result_plot <- make_barplot(
        long_sdf = long_df,
        value_col = "value",
        ddf = design_df,
        sample_col = "Sample",
        dataset = "Dataset1",
        color = "Condition",
        show_missing = FALSE,
        rotate_labels = TRUE
    )

    expect_s3_class(result_plot, "gg")
    expect_true(!is.null(result_plot$theme$axis.text.x))
})

test_that("make_barplot handles custom labels", {
    long_df <- make_sample_long_df(n_per_sample = 10, bases = c(5, 6, 7))
    design_df <- make_sample_design_df()

    result_plot <- make_barplot(
        long_sdf = long_df,
        value_col = "value",
        ddf = design_df,
        sample_col = "Sample",
        dataset = "Dataset1",
        color = "Condition",
        show_missing = FALSE,
        title = "Custom Title",
        xlab = "Custom X",
        ylab = "Custom Y"
    )

    expect_s3_class(result_plot, "gg")
    expect_equal(result_plot$labels$x, "Custom X")
    expect_equal(result_plot$labels$y, "Custom Y")
})

test_that("make_barplot handles NULL color", {
    long_df <- make_sample_long_df(n_per_sample = 10, bases = c(5, 6, 7))
    design_df <- make_sample_design_df()

    result_plot <- make_barplot(
        long_sdf = long_df,
        value_col = "value",
        ddf = design_df,
        sample_col = "Sample",
        dataset = "Dataset1",
        color = NULL,
        show_missing = FALSE
    )

    expect_s3_class(result_plot, "gg")
})

test_that("adjust_boxplot ignores ordering when ddf_cond_col is 'None'", {
    df <- make_sample_long_df(
        sample_col = "Sample",
        value_col = "Abundance",
        n_per_sample = 10,
        bases = c(5, 6, 7)
    )
    base_plot <- ggplot(df, aes(x = Sample, y = Abundance))

    result_plot <- adjust_boxplot(
        plt = base_plot,
        do_violin = FALSE,
        rotate_x_labels = FALSE,
        order_on_condition = TRUE,
        ddf = make_sample_design_df(sample_col = "Sample", condition_col = "Condition"),
        ddf_sample_col = "Sample",
        ddf_cond_col = "None",
        text_size = 12
    )

    expect_s3_class(result_plot, "gg")
})
