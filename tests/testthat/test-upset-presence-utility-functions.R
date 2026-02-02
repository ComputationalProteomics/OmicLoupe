test_that("parse_na_nbrs_to_upset_table creates binary table with zero threshold", {
    nbr_nas_df <- data.frame(
        comb_id = c("F1", "F2", "F3", "F4"),
        Control.nbr_na = c(3, 0, 2, 1),
        Treated.nbr_na = c(0, 3, 2, 0)
    )

    ddf <- data.frame(
        Sample = c("S1", "S2", "S3", "S4", "S5", "S6"),
        Condition = c("Control", "Control", "Control", "Treated", "Treated", "Treated")
    )

    result <- parse_na_nbrs_to_upset_table(
        nbr_nas_df = nbr_nas_df,
        dataset = "d1",
        ddf = ddf,
        selected_cond = "Condition",
        selected_levels = c("Control", "Treated"),
        presence_fraction_thres = 0
    )

    expect_s3_class(result, "data.frame")
    expect_true("comb_id" %in% colnames(result))
    expect_true("Control" %in% colnames(result))
    expect_true("Treated" %in% colnames(result))

    expect_equal(result$Control, c(1, 0, 1, 1))
    expect_equal(result$Treated, c(0, 1, 1, 0))
})

test_that("parse_na_nbrs_to_upset_table applies fraction threshold", {
    nbr_nas_df <- data.frame(
        comb_id = c("F1", "F2", "F3", "F4"),
        Control.nbr_na = c(3, 1, 0, 2),
        Treated.nbr_na = c(2, 3, 1, 0)
    )

    ddf <- data.frame(
        Sample = c("S1", "S2", "S3", "S4", "S5", "S6"),
        Condition = c("Control", "Control", "Control", "Treated", "Treated", "Treated")
    )

    result <- parse_na_nbrs_to_upset_table(
        nbr_nas_df = nbr_nas_df,
        dataset = "d1",
        ddf = ddf,
        selected_cond = "Condition",
        selected_levels = c("Control", "Treated"),
        presence_fraction_thres = 0.5
    )

    expect_s3_class(result, "data.frame")

    expect_equal(result$Control[1], 1)
    expect_equal(result$Treated[1], 1)

    expect_equal(result$Control[2], 0)
    expect_equal(result$Treated[2], 1)

    expect_equal(result$Control[3], 0)
    expect_equal(result$Treated[3], 0)

    expect_equal(result$Control[4], 1)
    expect_equal(result$Treated[4], 0)
})

test_that("parse_na_nbrs_to_upset_table handles single condition", {
    nbr_nas_df <- data.frame(
        comb_id = c("F1", "F2", "F3"),
        Control.nbr_na = c(3, 0, 2)
    )

    ddf <- data.frame(
        Sample = c("S1", "S2", "S3"),
        Condition = c("Control", "Control", "Control")
    )

    result <- parse_na_nbrs_to_upset_table(
        nbr_nas_df = nbr_nas_df,
        dataset = "d1",
        ddf = ddf,
        selected_cond = "Condition",
        selected_levels = c("Control"),
        presence_fraction_thres = 0
    )

    expect_s3_class(result, "data.frame")
    expect_equal(ncol(result), 2)
    expect_true("Control" %in% colnames(result))
})

test_that("parse_na_nbrs_to_upset_table handles multiple conditions", {
    nbr_nas_df <- data.frame(
        comb_id = c("F1", "F2"),
        Control.nbr_na = c(2, 1),
        Treated.nbr_na = c(1, 2),
        Vehicle.nbr_na = c(3, 0)
    )

    ddf <- data.frame(
        Sample = paste0("S", 1:9),
        Condition = c(rep("Control", 3), rep("Treated", 3), rep("Vehicle", 3))
    )

    result <- parse_na_nbrs_to_upset_table(
        nbr_nas_df = nbr_nas_df,
        dataset = "d1",
        ddf = ddf,
        selected_cond = "Condition",
        selected_levels = c("Control", "Treated", "Vehicle"),
        presence_fraction_thres = 0
    )

    expect_s3_class(result, "data.frame")
    expect_equal(ncol(result), 4)
    expect_true(all(c("Control", "Treated", "Vehicle") %in% colnames(result)))
})

test_that("parse_na_nbrs_to_upset_table preserves comb_id", {
    nbr_nas_df <- data.frame(
        comb_id = c("Feature1", "Feature2", "Feature3"),
        Control.nbr_na = c(3, 0, 2),
        Treated.nbr_na = c(0, 3, 1)
    )

    ddf <- data.frame(
        Sample = c("S1", "S2", "S3", "S4", "S5", "S6"),
        Condition = c("Control", "Control", "Control", "Treated", "Treated", "Treated")
    )

    result <- parse_na_nbrs_to_upset_table(
        nbr_nas_df = nbr_nas_df,
        dataset = "d1",
        ddf = ddf,
        selected_cond = "Condition",
        selected_levels = c("Control", "Treated"),
        presence_fraction_thres = 0
    )

    expect_equal(result$comb_id, c("Feature1", "Feature2", "Feature3"))
})

test_that("parse_na_nbrs_to_upset_table handles all zeros", {
    nbr_nas_df <- data.frame(
        comb_id = c("F1", "F2"),
        Control.nbr_na = c(0, 0),
        Treated.nbr_na = c(0, 0)
    )

    ddf <- data.frame(
        Sample = c("S1", "S2", "S3", "S4"),
        Condition = c("Control", "Control", "Treated", "Treated")
    )

    result <- parse_na_nbrs_to_upset_table(
        nbr_nas_df = nbr_nas_df,
        dataset = "d1",
        ddf = ddf,
        selected_cond = "Condition",
        selected_levels = c("Control", "Treated"),
        presence_fraction_thres = 0
    )

    expect_s3_class(result, "data.frame")
    expect_equal(result$Control, c(0, 0))
    expect_equal(result$Treated, c(0, 0))
})

test_that("parse_na_nbrs_to_upset_table handles all ones", {
    nbr_nas_df <- data.frame(
        comb_id = c("F1", "F2"),
        Control.nbr_na = c(3, 3),
        Treated.nbr_na = c(3, 3)
    )

    ddf <- data.frame(
        Sample = c("S1", "S2", "S3", "S4", "S5", "S6"),
        Condition = c("Control", "Control", "Control", "Treated", "Treated", "Treated")
    )

    result <- parse_na_nbrs_to_upset_table(
        nbr_nas_df = nbr_nas_df,
        dataset = "d1",
        ddf = ddf,
        selected_cond = "Condition",
        selected_levels = c("Control", "Treated"),
        presence_fraction_thres = 0
    )

    expect_s3_class(result, "data.frame")
    expect_equal(result$Control, c(1, 1))
    expect_equal(result$Treated, c(1, 1))
})
