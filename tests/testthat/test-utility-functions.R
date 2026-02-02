
test_that("get_dataset_choices returns empty for null data", {
  rv <- list(
    filedata_1 = function() NULL,
    filedata_2 = function() NULL
  )

  choices <- get_dataset_choices(rv)

  expect_equal(length(choices), 0)
})

test_that("get_dataset_choices returns single dataset", {
  rv <- list(
    filedata_1 = function() data.frame(a=1:3),
    filedata_2 = function() NULL,
    filename_1 = function() "Dataset1"
  )

  choices <- get_dataset_choices(rv)

  expect_equal(length(choices), 1)
  expect_equal(choices[1], "Dataset1")
})

test_that("get_dataset_choices returns both datasets", {
  rv <- list(
    filedata_1 = function() data.frame(a=1:3),
    filedata_2 = function() data.frame(b=1:3),
    filename_1 = function() "Dataset1",
    filename_2 = function() "Dataset2"
  )

  choices <- get_dataset_choices(rv)

  expect_equal(length(choices), 2)
  expect_true("Dataset1" %in% choices)
  expect_true("Dataset2" %in% choices)
})


test_that("Constants are defined correctly", {
  expect_true(exists("MAX_FILE_SIZE_MB"))
  expect_true(exists("DEFAULT_PAGE_LENGTH"))
  expect_true(exists("MY_COLORS_COMPARISON"))
  expect_true(exists("MY_COLORS_SELECTED"))
  expect_true(exists("SET1_COLORS"))
  expect_true(exists("MAX_DISCRETE_LEVELS"))
  expect_true(exists("MAX_COLORS"))
})

test_that("Color palettes have expected structure", {
  expect_true(is.vector(MY_COLORS_COMPARISON))
  expect_true(is.vector(MY_COLORS_SELECTED))
  expect_true(is.vector(SET1_COLORS))

  expect_true(!is.null(names(MY_COLORS_COMPARISON)))
  expect_true(all(c("None", "Both", "First", "Second", "Contra") %in% names(MY_COLORS_COMPARISON)))
})

test_that("Numeric constants are reasonable", {
  expect_true(is.numeric(DEFAULT_PAGE_LENGTH))
  expect_true(DEFAULT_PAGE_LENGTH > 0)

  expect_true(is.numeric(MAX_FILE_SIZE_MB))
  expect_true(MAX_FILE_SIZE_MB > 0)

  expect_true(is.numeric(MAX_DISCRETE_LEVELS))
  expect_true(MAX_DISCRETE_LEVELS > 0)

  expect_true(is.numeric(MAX_COLORS))
  expect_true(MAX_COLORS > 0)
})
