
test_that("Venn class initializes with expected methods", {
  venn <- Venn$new()
  expect_true(R6::is.R6(venn))
  expect_true(is.function(venn$simple_venn))
  expect_true(is.function(venn$count_venn))
  expect_true(is.function(venn$do_paired_expression_venn))
})

test_that("Venn simple_venn returns a ggplot object", {
  venn <- Venn$new()

  plt <- venn$simple_venn(
    col1 = c("protA", "protB"),
    col2 = c("protB", "protC"),
    title = "Simple",
    labels = c("A", "B")
  )

  expect_s3_class(plt, "gg")
})

test_that("Venn count_venn returns a ggplot object", {
  venn <- Venn$new()

  plt <- venn$count_venn(
    left_count = 5,
    right_count = 4,
    joint_count = 2,
    title = "Counts",
    labels = c("Left", "Right")
  )

  expect_s3_class(plt, "gg")
})

test_that("Venn do_paired_expression_venn validates highlight", {
  venn <- Venn$new()

  expect_error(
    venn$do_paired_expression_venn(
      col1_w_fold = c(2, -1),
      col2_w_fold = c(1.5, -1.2),
      highlight = "invalid_option"
    ),
    "Unknown highlight"
  )
})
