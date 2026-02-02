
is_truthy_string <- function(value) {
    if (is.null(value) || length(value) == 0 || is.na(value)) {
        return(FALSE)
    }
    tolower(as.character(value)) %in% c("1", "true", "yes", "y", "on")
}

get_handoff_base_dir <- function() {
    Sys.getenv("OMICLOUPE_HANDOFF_DIR", unset = "/home/data/omicloupe-handoff")
}

make_test_handoff_preloaded <- function() {
    set.seed(1)

    feature_ids <- paste0("Prot", sprintf("%03d", seq_len(50)))
    sample_names <- paste0("s", seq_len(6))

    design1 <- data.frame(
        sample = sample_names,
        condition = c(rep("A", 3), rep("B", 3)),
        stringsAsFactors = FALSE
    )

    base <- stats::rnorm(length(feature_ids), mean = 10, sd = 1)
    effect <- stats::rnorm(length(feature_ids), mean = 0, sd = 0.6)
    noise_sd <- 0.2

    A_mat <- vapply(
        seq_len(3),
        function(i) base + stats::rnorm(length(feature_ids), 0, noise_sd),
        numeric(length(feature_ids))
    )
    B_mat <- vapply(
        seq_len(3),
        function(i) base + effect + stats::rnorm(length(feature_ids), 0, noise_sd),
        numeric(length(feature_ids))
    )
    colnames(A_mat) <- sample_names[1:3]
    colnames(B_mat) <- sample_names[4:6]

    sample_df <- as.data.frame(cbind(A_mat, B_mat))
    logFC <- rowMeans(B_mat) - rowMeans(A_mat)
    AveExpr <- rowMeans(sample_df)
    pvals <- apply(sample_df, 1, function(row) stats::t.test(row[1:3], row[4:6])$p.value)
    adj <- stats::p.adjust(pvals, method = "BH")

    data1 <- data.frame(
        Feature = feature_ids,
        sample_df,
        `A_vs_B.logFC` = logFC,
        `A_vs_B.P.Value` = pvals,
        `A_vs_B.adj.P.Val` = adj,
        `A_vs_B.AveExpr` = AveExpr,
        check.names = FALSE
    )

    validate_preloaded_data(
        data1 = data1,
        data2 = NULL,
        design1 = design1,
        design2 = NULL,
        feature_col1 = "Feature",
        feature_col2 = NULL,
        sample_col1 = "sample",
        sample_col2 = "sample",
        two_datasets = FALSE
    )

    list(
        handoff_source = "NormalyzerDE",
        handoff_token = "test",
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
    )
}

validate_handoff_token <- function(token) {
    if (!is.character(token) || length(token) != 1 || is.na(token) || token == "") {
        stop("Missing or invalid handoff token")
    }

    if (!grepl("^[A-Za-z0-9_-]{20,128}$", token)) {
        stop("Invalid token format (expected [A-Za-z0-9_-], length 20-128)")
    }

    invisible(TRUE)
}

get_handoff_token <- function(session) {
    if (is.null(session)) {
        return(NULL)
    }

    url_search <- NULL

    get_request_query_string <- function(request) {
        if (is.null(request)) {
            return(NULL)
        }

        query <- request$QUERY_STRING
        if (is.null(query) || length(query) == 0 || identical(query, "")) {
            query <- request$queryString
        }

        if (is.null(query) || length(query) == 0 || identical(query, "")) {
            return(NULL)
        }

        query
    }

    request_candidates <- list(
        tryCatch(session$request, error = function(e) NULL),
        tryCatch(session$parent$request, error = function(e) NULL),
        tryCatch(session$session$request, error = function(e) NULL)
    )

    for (request in request_candidates) {
        url_search <- get_request_query_string(request)
        if (!is.null(url_search)) {
            break
        }
    }

    if (is.null(url_search)) {
        url_search <- tryCatch(
            session$clientData$url_search,
            error = function(e) NULL
        )
    }

    if (is.null(url_search) || length(url_search) == 0) {
        return(NULL)
    }

    url_search <- as.character(url_search[[1]])
    if (is.na(url_search) || url_search == "") {
        return(NULL)
    }

    params <- shiny::parseQueryString(url_search)
    token <- params[["token"]]
    if (is.null(token) || length(token) == 0 || is.na(token) || token == "") {
        return(NULL)
    }

    as.character(token[[1]])
}

resolve_handoff_token_dir <- function(base_dir, token) {
    if (!dir.exists(base_dir)) {
        stop(sprintf("Handoff directory does not exist: %s", base_dir))
    }

    token_dir <- file.path(base_dir, token)
    if (!dir.exists(token_dir)) {
        stop(sprintf("Handoff token directory not found: %s", token_dir))
    }

    norm_base <- normalizePath(base_dir, winslash = "/", mustWork = TRUE)
    norm_token <- normalizePath(token_dir, winslash = "/", mustWork = TRUE)

    base_prefix <- paste0(norm_base, "/")
    if (!(startsWith(norm_token, base_prefix) || identical(norm_token, norm_base))) {
        stop("Resolved token directory escapes the configured handoff base directory")
    }

    norm_token
}

load_handoff_preloaded <- function(token, base_dir = get_handoff_base_dir()) {
    if (isTRUE(identical(token, "test"))) {
        return(make_test_handoff_preloaded())
    }

    validate_handoff_token(token)

    token_dir <- resolve_handoff_token_dir(base_dir, token)

    require_ready <- is_truthy_string(Sys.getenv("OMICLOUPE_HANDOFF_REQUIRE_READY", unset = "false"))
    ready_path <- file.path(token_dir, ".ready")
    if (require_ready && !file.exists(ready_path)) {
        stop("Handoff bundle is not marked ready (missing .ready)")
    }

    data_path <- file.path(token_dir, "data.tsv")
    design_path <- file.path(token_dir, "design.tsv")
    manifest_path <- file.path(token_dir, "manifest.json")

    if (!file.exists(data_path)) {
        stop(sprintf("Missing required file: %s", data_path))
    }
    if (!file.exists(design_path)) {
        stop(sprintf("Missing required file: %s", design_path))
    }

    data1 <- readr::read_tsv(data_path, col_types = readr::cols())
    design1 <- readr::read_tsv(design_path, col_types = readr::cols())

    if (!is.data.frame(design1) || ncol(design1) == 0) {
        stop("design.tsv must have at least one column")
    }

    manifest <- NULL
    if (file.exists(manifest_path)) {
        manifest <- jsonlite::fromJSON(manifest_path)
    }

    feature_col1 <- NULL
    sample_col1 <- NULL
    auto_load <- TRUE

    if (!is.null(manifest)) {
        if (!is.null(manifest$feature_col)) feature_col1 <- as.character(manifest$feature_col)
        if (!is.null(manifest$sample_col)) sample_col1 <- as.character(manifest$sample_col)
        if (!is.null(manifest$auto_load)) auto_load <- isTRUE(manifest$auto_load)
    }

    if (!is.null(feature_col1) && !(feature_col1 %in% colnames(data1))) {
        stop(sprintf("manifest feature_col '%s' not found in data.tsv columns", feature_col1))
    }

    if (!is.null(sample_col1) && !(sample_col1 %in% colnames(design1))) {
        stop(sprintf("manifest sample_col '%s' not found in design.tsv columns", sample_col1))
    }

    if (is.null(sample_col1)) {
        if ("sample" %in% colnames(design1)) {
            sample_col1 <- "sample"
        } else {
            sample_col1 <- colnames(design1)[1]
        }
    }

    validate_preloaded_data(
        data1 = data1,
        data2 = NULL,
        design1 = design1,
        design2 = NULL,
        feature_col1 = feature_col1,
        feature_col2 = NULL,
        sample_col1 = sample_col1,
        sample_col2 = "sample",
        two_datasets = FALSE
    )

    list(
        handoff_source = "NormalyzerDE",
        handoff_token = token,
        data1 = data1,
        data2 = NULL,
        design1 = design1,
        design2 = NULL,
        feature_col1 = feature_col1,
        feature_col2 = NULL,
        sample_col1 = sample_col1,
        sample_col2 = "sample",
        two_datasets = FALSE,
        matched_samples = FALSE,
        skip_correlation = FALSE,
        discard_duplicates = FALSE,
        auto_load = auto_load
    )
}
