
test_that("MapObject handles single-feature datasets", {
  df <- data.frame(
    Gene = "only_gene",
    Sample1 = 100
  )

  mo <- MapObject$new(df, "Gene", samples1 = "Sample1")

  expect_equal(nrow(mo$dataset1), 1)
  expect_equal(ncol(mo$dataset1), 2)
})

test_that("MapObject handles single-sample datasets", {
  df <- data.frame(
    Gene = c("A", "B", "C", "D"),
    Sample1 = c(1, 2, 3, 4)
  )

  mo <- MapObject$new(df, "Gene", samples1 = "Sample1")

  expect_equal(nrow(mo$dataset1), 4)
  expect_equal(ncol(mo$dataset1), 2)
})

test_that("MapObject preserves negative values", {
  df <- data.frame(
    Gene = c("A", "B", "C"),
    Sample1 = c(-5, 0, 5)
  )

  mo <- MapObject$new(df, "Gene", samples1 = "Sample1")
  comb <- mo$get_combined_dataset()

  expect_true(any(comb$d1.Sample1 < 0))
  expect_true(any(comb$d1.Sample1 == 0))
  expect_true(any(comb$d1.Sample1 > 0))
})

test_that("MapObject handles very small p-values", {
  df <- data.frame(
    Gene = c("A", "B", "C"),
    Sample1 = c(1e-300, 1e-100, 1e-10)
  )

  mo <- MapObject$new(df, "Gene", samples1 = "Sample1")
  comb <- mo$get_combined_dataset()

  expect_equal(nrow(comb), 3)
  expect_true(all(comb$d1.Sample1 > 0))
})

test_that("Venn handles 0-way intersection", {
  set1 <- c("A", "B")
  set2 <- c("C", "D")

  venn <- Venn$new()
  plt <- venn$simple_venn(set1, set2, title = "No Overlap", labels = c("S1", "S2"))

  expect_s3_class(plt, "gg")
  expect_equal(length(intersect(set1, set2)), 0)
})

test_that("MapObject handles completely identical samples", {
  df <- data.frame(
    Gene = c("A", "B"),
    S1 = c(1, 2),
    S2 = c(1, 2),
    S3 = c(3, 4)
  )

  mo <- MapObject$new(df, "Gene", samples1 = c("S1", "S2"))

  expect_equal(nrow(mo$dataset1), 2)
})

test_that("MapObject handles empty intersection between datasets", {
  df1 <- data.frame(
    Gene = c("A", "B", "C"),
    Sample1 = c(1, 2, 3)
  )

  df2 <- data.frame(
    Gene = c("D", "E", "F"),
    Sample2 = c(4, 5, 6)
  )

  mo <- MapObject$new(df1, "Gene", df2, "Gene", samples1 = "Sample1", samples2 = "Sample2")

  expect_equal(nrow(mo$dataset1), 3)
  expect_equal(nrow(mo$dataset2), 3)
})

test_that("MapObject handles large datasets efficiently", {
  df <- data.frame(
    Gene = paste0("Gene_", 1:1000),
    matrix(rnorm(10000), nrow = 1000, ncol = 10)
  )
  colnames(df)[-1] <- paste0("Sample", 1:10)

  mo <- MapObject$new(df, "Gene", samples1 = paste0("Sample", 1:5))

  expect_equal(nrow(mo$dataset1), 1000)
  expect_equal(ncol(mo$dataset1), 11)
})

test_that("MapObject handles datasets with missing values", {
  df1 <- data.frame(
    Gene = c("A", "B", "C"),
    Sample1 = c(1, NA, 3),
    Sample2 = c(NA, 2, 3)
  )

  df2 <- data.frame(
    Gene = c("A", "B", "C"),
    Sample3 = c(4, 5, NA),
    Sample4 = c(6, NA, 8)
  )

  mo <- MapObject$new(df1, "Gene", df2, "Gene",
                      samples1 = c("Sample1", "Sample2"),
                      samples2 = c("Sample3", "Sample4"))

  comb <- mo$get_combined_dataset()

  expect_true(any(is.na(comb$d1.Sample1)))
  expect_true(any(is.na(comb$d2.Sample3)))
})

test_that("Venn handles empty second set", {
  set1 <- c("A", "B", "C")
  set2 <- character(0)

  venn <- Venn$new()
  plt <- venn$simple_venn(set1, set2, title = "Single Set", labels = c("S1", "S2"))

  expect_s3_class(plt, "gg")
})

test_that("MapObject handles duplicate feature IDs with discard_dups", {
  df <- data.frame(
    Gene = c("A", "A", "B", "B", "C"),
    Sample1 = c(1, 2, 3, 4, 5)
  )

  mo <- MapObject$new(df, "Gene", samples1 = "Sample1", discard_dups = TRUE)

  expect_lt(nrow(mo$dataset1), 5)
})

test_that("MapObject correlation skipping works", {
  df1 <- data.frame(
    Gene = c("A", "B", "C"),
    Sample1 = c(1, 2, 3)
  )

  df2 <- data.frame(
    Gene = c("A", "B", "C"),
    Sample2 = c(4, 5, 6)
  )

  mo <- MapObject$new(df1, "Gene", df2, "Gene",
                      samples1 = "Sample1", samples2 = "Sample2",
                      matched = FALSE, skip_correlation = TRUE)

  expect_false(mo$has_correlations())
})
