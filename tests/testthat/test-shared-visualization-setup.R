


test_that("get_dataset_choices returns only dataset1 when dataset2 is NULL", {
  rv <- list(
    filedata_1 = function() data.frame(a = 1:3),
    filedata_2 = function() NULL,
    filename_1 = function() "Dataset1"
  )

  choices <- get_dataset_choices(rv)

  expect_equal(length(choices), 1)
  expect_equal(choices[1], "Dataset1")
  expect_true(is.character(choices))
})

test_that("get_dataset_choices returns only dataset2 when dataset1 is NULL", {
  rv <- list(
    filedata_1 = function() NULL,
    filedata_2 = function() data.frame(b = 1:3),
    filename_2 = function() "Dataset2"
  )

  choices <- get_dataset_choices(rv)

  expect_equal(length(choices), 1)
  expect_equal(choices[1], "Dataset2")
  expect_true(is.character(choices))
})

test_that("get_dataset_choices returns both datasets when both are present", {
  rv <- list(
    filedata_1 = function() data.frame(a = 1:3),
    filedata_2 = function() data.frame(b = 1:3),
    filename_1 = function() "Dataset1",
    filename_2 = function() "Dataset2"
  )

  choices <- get_dataset_choices(rv)

  expect_equal(length(choices), 2)
  expect_true("Dataset1" %in% choices)
  expect_true("Dataset2" %in% choices)
  expect_equal(choices[1], "Dataset1")
  expect_equal(choices[2], "Dataset2")
})

test_that("get_dataset_choices handles filenames with spaces", {
  rv <- list(
    filedata_1 = function() data.frame(a = 1:3),
    filedata_2 = function() data.frame(b = 1:3),
    filename_1 = function() "My Dataset 1",
    filename_2 = function() "Another Dataset 2"
  )

  choices <- get_dataset_choices(rv)

  expect_equal(length(choices), 2)
  expect_equal(choices[1], "My Dataset 1")
  expect_equal(choices[2], "Another Dataset 2")
})

test_that("get_dataset_choices handles filenames with special characters", {
  rv <- list(
    filedata_1 = function() data.frame(a = 1:3),
    filedata_2 = function() data.frame(b = 1:3),
    filename_1 = function() "dataset_v1.2-final",
    filename_2 = function() "data@2023!results#1"
  )

  choices <- get_dataset_choices(rv)

  expect_equal(length(choices), 2)
  expect_equal(choices[1], "dataset_v1.2-final")
  expect_equal(choices[2], "data@2023!results#1")
})

test_that("get_dataset_choices handles filenames with Unicode characters", {
  rv <- list(
    filedata_1 = function() data.frame(a = 1:3),
    filedata_2 = function() data.frame(b = 1:3),
    filename_1 = function() "Dätasét_1",
    filename_2 = function() "データセット_2"
  )

  choices <- get_dataset_choices(rv)

  expect_equal(length(choices), 2)
  expect_equal(choices[1], "Dätasét_1")
  expect_equal(choices[2], "データセット_2")
})

test_that("get_dataset_choices handles very long filenames", {
  long_name1 <- paste0(rep("a", 200), collapse = "")
  long_name2 <- paste0(rep("b", 250), collapse = "")

  rv <- list(
    filedata_1 = function() data.frame(a = 1:3),
    filedata_2 = function() data.frame(b = 1:3),
    filename_1 = function() long_name1,
    filename_2 = function() long_name2
  )

  choices <- get_dataset_choices(rv)

  expect_equal(length(choices), 2)
  expect_equal(nchar(choices[1]), 200)
  expect_equal(nchar(choices[2]), 250)
  expect_equal(choices[1], long_name1)
  expect_equal(choices[2], long_name2)
})

test_that("get_dataset_choices handles empty string filenames", {
  rv <- list(
    filedata_1 = function() data.frame(a = 1:3),
    filedata_2 = function() data.frame(b = 1:3),
    filename_1 = function() "",
    filename_2 = function() ""
  )

  choices <- get_dataset_choices(rv)

  expect_equal(length(choices), 2)
  expect_equal(choices[1], "")
  expect_equal(choices[2], "")
})

test_that("get_dataset_choices handles whitespace-only filenames", {
  rv <- list(
    filedata_1 = function() data.frame(a = 1:3),
    filedata_2 = function() data.frame(b = 1:3),
    filename_1 = function() "   ",
    filename_2 = function() "\t\n"
  )

  choices <- get_dataset_choices(rv)

  expect_equal(length(choices), 2)
  expect_equal(choices[1], "   ")
  expect_equal(choices[2], "\t\n")
})


test_that("get_dataset_choices handles duplicate filenames", {
  rv <- list(
    filedata_1 = function() data.frame(a = 1:3),
    filedata_2 = function() data.frame(b = 1:3),
    filename_1 = function() "SameName",
    filename_2 = function() "SameName"
  )

  choices <- get_dataset_choices(rv)

  expect_equal(length(choices), 2)
  expect_equal(choices[1], "SameName")
  expect_equal(choices[2], "SameName")
  expect_true(all(choices == "SameName"))
})


test_that("get_dataset_choices works with only filedata_1 and filename_1", {
  rv <- list(
    filedata_1 = function() data.frame(x = 1:5, y = 6:10),
    filename_1 = function() "OnlyDataset",
    filedata_2 = function() NULL,
    filename_2 = function() NULL
  )

  choices <- get_dataset_choices(rv)

  expect_equal(length(choices), 1)
  expect_equal(choices, "OnlyDataset")
})

test_that("get_dataset_choices works with only filedata_2 and filename_2", {
  rv <- list(
    filedata_2 = function() data.frame(x = 1:5, y = 6:10),
    filename_2 = function() "OnlySecondDataset"
  )

  rv$filedata_1 <- function() NULL

  choices <- get_dataset_choices(rv)

  expect_equal(length(choices), 1)
  expect_equal(choices, "OnlySecondDataset")
})

test_that("get_dataset_choices handles path-like filenames", {
  rv <- list(
    filedata_1 = function() data.frame(a = 1:3),
    filedata_2 = function() data.frame(b = 1:3),
    filename_1 = function() "/path/to/my/dataset1.tsv",
    filename_2 = function() "C:\\Users\\Data\\dataset2.csv"
  )

  choices <- get_dataset_choices(rv)

  expect_equal(length(choices), 2)
  expect_equal(choices[1], "/path/to/my/dataset1.tsv")
  expect_equal(choices[2], "C:\\Users\\Data\\dataset2.csv")
})

test_that("get_dataset_choices preserves filename order (dataset1, then dataset2)", {
  rv <- list(
    filedata_1 = function() data.frame(a = 1:3),
    filedata_2 = function() data.frame(b = 1:3),
    filename_1 = function() "ZZZ_Last",
    filename_2 = function() "AAA_First"
  )

  choices <- get_dataset_choices(rv)

  expect_equal(length(choices), 2)
  expect_equal(choices[1], "ZZZ_Last")
  expect_equal(choices[2], "AAA_First")
})



test_that("get_dataset_choices handles NA filenames", {
  rv <- list(
    filedata_1 = function() data.frame(a = 1:3),
    filedata_2 = function() data.frame(b = 1:3),
    filename_1 = function() NA_character_,
    filename_2 = function() NA
  )

  choices <- get_dataset_choices(rv)

  expect_equal(length(choices), 2)
  expect_true(is.na(choices[1]))
  expect_true(is.na(choices[2]))
})

test_that("get_dataset_choices returns named vector structure", {
  rv <- list(
    filedata_1 = function() data.frame(a = 1:3),
    filedata_2 = function() data.frame(b = 1:3),
    filename_1 = function() "Dataset1",
    filename_2 = function() "Dataset2"
  )

  choices <- get_dataset_choices(rv)

  expect_true(is.character(choices))
  expect_true(is.vector(choices))
  expect_false(is.list(choices))
})

test_that("get_dataset_choices works with different data frame sizes", {
  rv <- list(
    filedata_1 = function() data.frame(),
    filedata_2 = function() data.frame(x = 1:1000),
    filename_1 = function() "EmptyDataFrame",
    filename_2 = function() "LargeDataFrame"
  )

  choices <- get_dataset_choices(rv)

  expect_equal(length(choices), 2)
  expect_equal(choices[1], "EmptyDataFrame")
  expect_equal(choices[2], "LargeDataFrame")
})

test_that("get_dataset_choices handles single column data frames", {
  rv <- list(
    filedata_1 = function() data.frame(single = 1:5),
    filedata_2 = function() data.frame(another = letters[1:3]),
    filename_1 = function() "SingleColumn1",
    filename_2 = function() "SingleColumn2"
  )

  choices <- get_dataset_choices(rv)

  expect_equal(length(choices), 2)
  expect_equal(choices[1], "SingleColumn1")
  expect_equal(choices[2], "SingleColumn2")
})

test_that("get_dataset_choices handles many column data frames", {
  df1 <- as.data.frame(matrix(1:100, nrow = 10, ncol = 10))
  df2 <- as.data.frame(matrix(101:200, nrow = 10, ncol = 10))

  rv <- list(
    filedata_1 = function() df1,
    filedata_2 = function() df2,
    filename_1 = function() "ManyColumns1",
    filename_2 = function() "ManyColumns2"
  )

  choices <- get_dataset_choices(rv)

  expect_equal(length(choices), 2)
  expect_equal(choices[1], "ManyColumns1")
  expect_equal(choices[2], "ManyColumns2")
})


