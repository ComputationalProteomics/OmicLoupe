

local_omicloupe_preloaded <- function(value = NULL) {
  old_preloaded <- getOption("omicloupe.preloaded", default = NULL)
  on.exit(options(omicloupe.preloaded = old_preloaded), add = TRUE)
  options(omicloupe.preloaded = value)
}

set_default_rv_inputs <- function(session, ...) {
  defaults <- list(
    two_datasets = FALSE,
    matched_samples = FALSE,
    design_sample_col_1 = "sample",
    design_sample_col_2 = "sample",
    design_cond_col_1 = "condition",
    design_cond_col_2 = "condition",
    feature_col_1 = "Feature",
    feature_col_2 = "Feature",
    annot_col_1 = "",
    annot_col_2 = "",
    figure_save_format = "png",
    figure_save_width = 800,
    figure_save_height = 600,
    figure_save_dpi = 300
  )

  dots <- list(...)
  for (name in names(dots)) {
    defaults[[name]] <- dots[[name]]
  }

  do.call(session$setInputs, defaults)
  session$flushReact()
}

make_upload_file <- function(data, name = "upload.tsv") {
  upload_path <- tempfile(fileext = ".tsv")
  readr::write_tsv(data, upload_path)

  data.frame(
    name = name,
    size = file.info(upload_path)$size,
    type = "text/tab-separated-values",
    datapath = upload_path,
    stringsAsFactors = FALSE
  )
}

test_that("setup_reactive_values_obj creates an rv interface in a Shiny context", {
  skip_if_not_installed("shiny")
  local_omicloupe_preloaded(NULL)

  shiny::testServer(
    function(input, output, session) {
      session$userData$rv <- setup_reactive_values_obj(input)
    },
    {
      rv <- session$userData$rv

      set_default_rv_inputs(session, feature_col_1 = "feature", feature_col_2 = "feature")

      expect_type(rv, "list")
      expect_true(is.function(rv$filedata_1))
      expect_true(is.function(rv$design_1))
      expect_true(is.function(rv$filename_1))
      expect_true(is.function(rv$set_selected_feature))

      rv$set_selected_feature("F1", "TestModule")
      expect_equal(rv$selected_feature(), "F1")
      expect_equal(rv$selected_feature_module(), "TestModule")
      expect_equal(rv$figure_save_format(), "png")
    }
  )
})

test_that("setup_reactive_values_obj accepts a reactive preloaded source", {
  skip_if_not_installed("shiny")
  local_omicloupe_preloaded(NULL)

  data1 <- data.frame(Feature = c("A", "B"), s1 = c(1, 2), s2 = c(3, 4))
  design1 <- data.frame(sample = c("s1", "s2"), condition = c("G1", "G2"))

  shiny::testServer(
    function(input, output, session) {
      session$userData$preloaded_rv <- shiny::reactiveVal(NULL)
      session$userData$rv <- setup_reactive_values_obj(input, preloaded = session$userData$preloaded_rv)
    },
    {
      rv <- session$userData$rv
      preloaded_rv <- session$userData$preloaded_rv

      set_default_rv_inputs(session, feature_col_1 = "Feature", feature_col_2 = "Feature")

      expect_null(rv$filedata_1())
      expect_null(rv$design_1())
      expect_null(rv$filename_1())

      preloaded_rv(list(
        data1 = data1,
        data2 = NULL,
        design1 = design1,
        design2 = NULL,
        matched_samples = FALSE
      ))
      session$flushReact()

      expect_true(is.data.frame(rv$filedata_1()))
      expect_true(is.data.frame(rv$design_1()))
      expect_equal(rv$filename_1(), "PreloadedData1")
    }
  )
})

test_that("uploads override preloaded inputs in reactive values", {
  skip_if_not_installed("shiny")
  local_omicloupe_preloaded(NULL)

  preloaded_data <- data.frame(Feature = c("A", "B"), s1 = c(1, 2))
  upload_data <- data.frame(Feature = c("X", "Y"), s1 = c(10, 20))

  upload_file <- make_upload_file(upload_data, name = "upload.tsv")

  shiny::testServer(
    function(input, output, session) {
      session$userData$preloaded_rv <- shiny::reactiveVal(NULL)
      session$userData$rv <- setup_reactive_values_obj(input, preloaded = session$userData$preloaded_rv)
    },
    {
      rv <- session$userData$rv
      preloaded_rv <- session$userData$preloaded_rv

      set_default_rv_inputs(session, feature_col_1 = "Feature", feature_col_2 = "Feature")

      preloaded_rv(list(
        data1 = preloaded_data,
        data2 = NULL,
        design1 = NULL,
        design2 = NULL,
        matched_samples = FALSE
      ))
      session$flushReact()

      expect_equal(rv$filename_1(), "PreloadedData1")
      expect_equal(as.data.frame(rv$filedata_1()), preloaded_data)

      session$setInputs(data_file_1 = upload_file)
      session$flushReact()

      expect_equal(rv$filename_1(), "upload.tsv")
      expect_equal(as.data.frame(rv$filedata_1()), upload_data)
    }
  )
})

test_that("constants used in reactive_values.R are defined", {
  expect_true(exists("DEFAULT_PAGE_LENGTH"))
  expect_equal(DEFAULT_PAGE_LENGTH, 10)
  expect_type(DEFAULT_PAGE_LENGTH, "double")
})


test_that("filename extraction logic (currently not extracted)", {

  extract_filename_logic <- function(in_file, preloaded_name = NULL) {
    if (!is.null(preloaded_name)) {
      return(preloaded_name)
    }
    if (is.null(in_file)) {
      return(NULL)
    }
    if (is.list(in_file) && !is.null(in_file$name)) {
      return(in_file$name)
    }
    return(NULL)
  }

  expect_null(extract_filename_logic(NULL, NULL))

  expect_equal(extract_filename_logic(NULL, "PreloadedData1"), "PreloadedData1")

  mock_file <- list(name = "test_data.tsv", datapath = "/tmp/test.tsv")
  expect_equal(extract_filename_logic(mock_file, NULL), "test_data.tsv")

  expect_equal(extract_filename_logic(mock_file, "PreloadedData1"), "PreloadedData1")
})

test_that("row index calculation logic (currently not extracted)", {

  calculate_preselect_index_logic <- function(selected_feature, shown_data) {
    if (is.null(selected_feature)) {
      return(1)
    } else {
      matches <- which(as.character(shown_data$comb_id) %in% selected_feature)
      if (length(matches) == 0) {
        return(1)
      }
      return(matches[1])
    }
  }

  test_data <- data.frame(
    comb_id = c("PROT_001", "PROT_002", "PROT_003", "PROT_004"),
    value = c(1.5, 2.3, 3.1, 4.2)
  )

  expect_equal(calculate_preselect_index_logic(NULL, test_data), 1)

  expect_equal(calculate_preselect_index_logic("PROT_001", test_data), 1)

  expect_equal(calculate_preselect_index_logic("PROT_003", test_data), 3)

  expect_equal(calculate_preselect_index_logic("PROT_004", test_data), 4)

  expect_equal(calculate_preselect_index_logic("PROT_999", test_data), 1)

  test_data_numeric <- data.frame(
    comb_id = 1:4,
    value = c(1.5, 2.3, 3.1, 4.2)
  )
  expect_equal(calculate_preselect_index_logic("3", test_data_numeric), 3)
})

test_that("data parsing logic (currently not extracted)", {

  parse_table_data_logic <- function(data, trunc_length, round_digits) {
    library(dplyr)
    library(stringr)

    parsed <- data %>%
      mutate(across(where(is.character), ~str_trunc(., trunc_length))) %>%
      mutate(across(where(is.numeric), ~round(., round_digits)))

    return(parsed)
  }

  test_data <- data.frame(
    id = c("PROT_001", "PROT_002_WITH_VERY_LONG_NAME_THAT_SHOULD_BE_TRUNCATED"),
    value1 = c(1.23456789, 2.34567890),
    value2 = c(3.14159265, 2.71828182),
    stringsAsFactors = FALSE
  )

  result <- parse_table_data_logic(test_data, trunc_length = 20, round_digits = 2)

  expect_equal(nchar(result$id[1]), nchar("PROT_001"))
  expect_true(nchar(result$id[2]) <= 20)

  expect_equal(result$value1[1], 1.23)
  expect_equal(result$value1[2], 2.35)
  expect_equal(result$value2[1], 3.14)
  expect_equal(result$value2[2], 2.72)

  result2 <- parse_table_data_logic(test_data, trunc_length = 50, round_digits = 4)
  expect_equal(result2$value1[1], 1.2346)
  expect_equal(result2$value2[1], 3.1416)
})

test_that("display position calculation logic (currently not extracted)", {

  calculate_display_position_logic <- function(selected_row_nbr, page_length = 10) {
    display_pos <- (selected_row_nbr - 1) - ((selected_row_nbr - 1) %% page_length)
    return(display_pos)
  }

  expect_equal(calculate_display_position_logic(1, 10), 0)
  expect_equal(calculate_display_position_logic(5, 10), 0)
  expect_equal(calculate_display_position_logic(10, 10), 0)

  expect_equal(calculate_display_position_logic(11, 10), 10)
  expect_equal(calculate_display_position_logic(15, 10), 10)
  expect_equal(calculate_display_position_logic(20, 10), 10)

  expect_equal(calculate_display_position_logic(21, 10), 20)
  expect_equal(calculate_display_position_logic(25, 10), 20)
  expect_equal(calculate_display_position_logic(30, 10), 20)

  expect_equal(calculate_display_position_logic(1, 25), 0)
  expect_equal(calculate_display_position_logic(26, 25), 25)
  expect_equal(calculate_display_position_logic(51, 25), 50)

  expect_equal(calculate_display_position_logic(1, 1), 0)
  expect_equal(calculate_display_position_logic(100, 10), 90)
})
