
test_that("Setup module can discard a NormalyzerDE handoff preloaded dataset", {
  skip_if_not_installed("shiny")

  old_preloaded <- getOption("omicloupe.preloaded", default = NULL)
  on.exit(options(omicloupe.preloaded = old_preloaded), add = TRUE)

  data1 <- data.frame(Feature = c("A", "B"), s1 = c(1, 2), s2 = c(3, 4))
  design1 <- data.frame(sample = c("s1", "s2"), condition = c("G1", "G2"))

  options(omicloupe.preloaded = list(
    handoff_source = "NormalyzerDE",
    handoff_token = paste(rep("a", 20), collapse = ""),
    data1 = data1,
    data2 = NULL,
    design1 = design1,
    design2 = NULL,
    feature_col1 = "Feature",
    feature_col2 = NULL,
    sample_col1 = "sample",
    sample_col2 = "sample",
    two_datasets = FALSE,
    matched_samples = FALSE,
    skip_correlation = FALSE,
    discard_duplicates = FALSE,
    auto_load = TRUE
  ))

  expect_warning(
    shiny::testServer(
      function(input, output, session) {
        session$userData$rv <- module_setup_server("Setup", "Setup")
      },
      {
        rv <- session$userData$rv

        session$setInputs(
          `Setup-automatic_sample_detect` = TRUE,
          `Setup-two_datasets` = FALSE,
          `Setup-matched_samples` = FALSE,
          `Setup-two_datasets_random_discard` = FALSE,
          `Setup-skip_correlation` = FALSE,
          `Setup-data_table_tabs` = "Design1"
        )
        session$flushReact()
        session$flushReact()

        expect_equal(rv$filename_1(), "NormalyzerDE")
        expect_true("NormalyzerDE" %in% names(rv$selected_cols_obj()))
        expect_true(is.data.frame(rv$filedata_1()))
        expect_true(is.data.frame(rv$design_1()))
        expect_false(is.null(rv$mapping_obj()))

        session$setInputs(`Setup-clear_handoff` = 1)
        session$flushReact()
        session$flushReact()

        expect_null(rv$filedata_1())
        expect_null(rv$design_1())
        expect_null(rv$filename_1())
        expect_null(rv$mapping_obj())
        expect_equal(rv$selected_cols_obj(), list())
      }
    ),
    "session\\$request doesn't currently simulate a realistic request on MockShinySession"
  )
})

