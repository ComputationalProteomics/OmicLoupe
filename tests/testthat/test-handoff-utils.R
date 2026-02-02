
test_that("get_handoff_token extracts token from request query string", {
  token <- paste(rep("a", 20), collapse = "")
  session <- list(
    request = list(QUERY_STRING = paste0("token=", token)),
    clientData = shiny::reactiveValues(url_search = paste0("?token=", token))
  )

  expect_equal(get_handoff_token(session), token)

  session_empty <- list(request = list(QUERY_STRING = ""))
  expect_null(get_handoff_token(session_empty))
})

test_that("get_handoff_token extracts token from url_search when request is missing", {
  token <- paste(rep("b", 20), collapse = "")
  session <- list(clientData = list(url_search = paste0("?token=", token)))

  expect_equal(get_handoff_token(session), token)
})

test_that("get_handoff_token extracts token from parent request query string", {
  token <- paste(rep("c", 20), collapse = "")
  session <- structure(
    list(parent = list(request = list(QUERY_STRING = paste0("token=", token)))),
    class = "session_proxy"
  )

  expect_equal(get_handoff_token(session), token)
})

test_that("validate_handoff_token enforces allowed format", {
  expect_error(validate_handoff_token("short"), "Invalid token format")
  expect_error(validate_handoff_token("not/allowed________________"), "Invalid token format")

  ok_token <- paste(rep("b", 20), collapse = "")
  expect_silent(validate_handoff_token(ok_token))
})

test_that("load_handoff_preloaded loads data.tsv and design.tsv", {
  base_dir <- tempfile("omicloupe-handoff-")
  dir.create(base_dir, recursive = TRUE)

  token <- paste(rep("c", 20), collapse = "")
  token_dir <- file.path(base_dir, token)
  dir.create(token_dir, recursive = TRUE)

  data <- data.frame(Feature = c("A", "B"), s1 = c(1, 2), s2 = c(3, 4))
  design <- data.frame(sample = c("s1", "s2"), condition = c("G1", "G2"))

  readr::write_tsv(data, file.path(token_dir, "data.tsv"))
  readr::write_tsv(design, file.path(token_dir, "design.tsv"))

  preloaded <- load_handoff_preloaded(token, base_dir = base_dir)

  expect_true(is.data.frame(preloaded$data1))
  expect_true(is.data.frame(preloaded$design1))
  expect_equal(preloaded$handoff_source, "NormalyzerDE")
  expect_equal(preloaded$handoff_token, token)
  expect_equal(preloaded$sample_col1, "sample")
  expect_true(isTRUE(preloaded$auto_load))
  expect_false(isTRUE(preloaded$two_datasets))
})

test_that("load_handoff_preloaded respects manifest.json and .ready requirement", {
  base_dir <- tempfile("omicloupe-handoff-")
  dir.create(base_dir, recursive = TRUE)

  token <- paste(rep("d", 20), collapse = "")
  token_dir <- file.path(base_dir, token)
  dir.create(token_dir, recursive = TRUE)

  data <- data.frame(Feature = c("A", "B"), s1 = c(1, 2), s2 = c(3, 4))
  design <- data.frame(sample = c("s1", "s2"), condition = c("G1", "G2"))

  readr::write_tsv(data, file.path(token_dir, "data.tsv"))
  readr::write_tsv(design, file.path(token_dir, "design.tsv"))

  jsonlite::write_json(
    list(feature_col = "Feature", sample_col = "sample", auto_load = FALSE),
    path = file.path(token_dir, "manifest.json"),
    auto_unbox = TRUE
  )

  old_env <- Sys.getenv("OMICLOUPE_HANDOFF_REQUIRE_READY", unset = NA)
  on.exit({
    if (is.na(old_env)) Sys.unsetenv("OMICLOUPE_HANDOFF_REQUIRE_READY") else Sys.setenv(OMICLOUPE_HANDOFF_REQUIRE_READY = old_env)
  }, add = TRUE)

  Sys.setenv(OMICLOUPE_HANDOFF_REQUIRE_READY = "true")
  expect_error(load_handoff_preloaded(token, base_dir = base_dir), "missing \\.ready")

  file.create(file.path(token_dir, ".ready"))
  preloaded <- load_handoff_preloaded(token, base_dir = base_dir)
  expect_equal(preloaded$feature_col1, "Feature")
  expect_equal(preloaded$sample_col1, "sample")
  expect_false(isTRUE(preloaded$auto_load))
})

test_that("load_handoff_preloaded supports token=test demo dataset", {
  preloaded <- load_handoff_preloaded("test")

  expect_equal(preloaded$handoff_source, "NormalyzerDE")
  expect_equal(preloaded$handoff_token, "test")
  expect_true(is.data.frame(preloaded$data1))
  expect_true(is.data.frame(preloaded$design1))
  expect_true("Feature" %in% colnames(preloaded$data1))
  expect_true("sample" %in% colnames(preloaded$design1))
  expect_true(isTRUE(preloaded$auto_load))
})
