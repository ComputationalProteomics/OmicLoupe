
test_that("Venn simple_venn method works with basic input", {
    venn <- Venn$new()

    col1 <- c("protA", "protB", "protC")
    col2 <- c("protB", "protC", "protD")

    plt <- venn$simple_venn(
        col1 = col1,
        col2 = col2,
        title = "Simple Venn Test",
        labels = c("Set1", "Set2")
    )

    expect_s3_class(plt, "gg")
})

test_that("Venn simple_venn handles duplicates", {
    venn <- Venn$new()

    col1 <- c("protA", "protB", "protA", "protC")
    col2 <- c("protB", "protC", "protD", "protD")

    plt_dedup <- venn$simple_venn(
        col1 = col1,
        col2 = col2,
        dereplicate = TRUE,
        title = "Dereplicated"
    )

    expect_s3_class(plt_dedup, "gg")

    plt_no_dedup <- venn$simple_venn(
        col1 = col1,
        col2 = col2,
        dereplicate = FALSE,
        title = "Not Dereplicated"
    )

    expect_s3_class(plt_no_dedup, "gg")
})

test_that("Venn simple_venn handles empty sets", {
    venn <- Venn$new()

    col1 <- c("protA", "protB")
    col2 <- character(0)

    plt <- venn$simple_venn(
        col1 = col1,
        col2 = col2,
        title = "One Empty Set"
    )

    expect_s3_class(plt, "gg")
})

test_that("Venn simple_venn handles custom colors", {
    venn <- Venn$new()

    col1 <- c("protA", "protB")
    col2 <- c("protB", "protC")

    plt <- venn$simple_venn(
        col1 = col1,
        col2 = col2,
        colors = c("red", "blue"),
        title = "Custom Colors"
    )

    expect_s3_class(plt, "gg")
})

test_that("Venn count_venn method works correctly", {
    venn <- Venn$new()

    plt <- venn$count_venn(
        left_count = 5,
        right_count = 3,
        joint_count = 2,
        title = "Count Venn",
        colors = c("lightblue", "lightgreen"),
        labels = c("Left", "Right")
    )

    expect_s3_class(plt, "gg")
})

test_that("Venn count_venn handles zero counts", {
    venn <- Venn$new()

    plt_no_overlap <- venn$count_venn(
        left_count = 5,
        right_count = 3,
        joint_count = 0,
        title = "No Overlap"
    )

    expect_s3_class(plt_no_overlap, "gg")

    plt_all_overlap <- venn$count_venn(
        left_count = 0,
        right_count = 0,
        joint_count = 10,
        title = "All Overlap"
    )

    expect_s3_class(plt_all_overlap, "gg")
})

test_that("Venn do_paired_expression_venn works with basic fold data", {
    venn <- Venn$new()

    col1_fold <- c(2.0, 1.5, NA, -1.2)
    col2_fold <- c(1.8, NA, 2.2, -1.1)

    plt <- venn$do_paired_expression_venn(
        col1_w_fold = col1_fold,
        col2_w_fold = col2_fold,
        title = "Expression Venn",
        colors = c("blue", "red"),
        highlight = "A&B",
        contrast_names = c("Condition A", "Condition B")
    )

    expect_s3_class(plt, "gg")
})

test_that("Venn do_paired_expression_venn handles different highlights", {
    venn <- Venn$new()

    col1_fold <- c(2.0, 1.5, NA, -1.2)
    col2_fold <- c(1.8, NA, 2.2, -1.1)

    plt_a <- venn$do_paired_expression_venn(
        col1_w_fold = col1_fold,
        col2_w_fold = col2_fold,
        title = "Highlight A",
        highlight = "A"
    )

    expect_s3_class(plt_a, "gg")

    plt_b <- venn$do_paired_expression_venn(
        col1_w_fold = col1_fold,
        col2_w_fold = col2_fold,
        title = "Highlight B",
        highlight = "B"
    )

    expect_s3_class(plt_b, "gg")

    plt_both <- venn$do_paired_expression_venn(
        col1_w_fold = col1_fold,
        col2_w_fold = col2_fold,
        title = "Highlight Both",
        highlight = "A&B"
    )

    expect_s3_class(plt_both, "gg")
})

test_that("Venn simple_venn handles legend position", {
    venn <- Venn$new()

    col1 <- c("protA", "protB")
    col2 <- c("protB", "protC")

    for (pos in c("right", "left", "top", "bottom", "none")) {
        plt <- venn$simple_venn(
            col1 = col1,
            col2 = col2,
            legend.position = pos,
            title = paste("Legend", pos)
        )

        expect_s3_class(plt, "gg")
    }
})
