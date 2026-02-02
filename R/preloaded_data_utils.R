validate_preloaded_data <- function(
    data1,
    data2,
    design1,
    design2,
    feature_col1,
    feature_col2,
    sample_col1,
    sample_col2,
    two_datasets
) {
    if (!is.data.frame(data1)) {
        stop("data1 must be a data.frame")
    }
    if (nrow(data1) == 0) {
        stop("data1 has no rows")
    }
    if (ncol(data1) < 2) {
        stop("data1 must have at least 2 columns (feature column + at least one sample)")
    }

    if (!is.null(feature_col1)) {
        if (!feature_col1 %in% colnames(data1)) {
            stop(sprintf("feature_col1 '%s' not found in data1 columns: %s",
                        feature_col1, paste(colnames(data1), collapse=", ")))
        }
    }

    if (!is.null(design1)) {
        if (!is.data.frame(design1)) {
            stop("design1 must be a data.frame")
        }
        if (!sample_col1 %in% colnames(design1)) {
            stop(sprintf("sample_col1 '%s' not found in design1 columns: %s",
                        sample_col1, paste(colnames(design1), collapse=", ")))
        }

        if (!is.null(feature_col1)) {
            design_samples <- design1[[sample_col1]]
            data_cols <- setdiff(colnames(data1), feature_col1)
            missing <- setdiff(design_samples, data_cols)

            if (length(missing) > 0) {
                warning(sprintf(
                    "Design1 samples not in data1: %s",
                    paste(head(missing, 5), collapse=", ")
                ))
            }
        }
    }

    if (two_datasets) {
        if (is.null(data2)) {
            stop("two_datasets=TRUE but data2 is NULL")
        }
        if (!is.data.frame(data2)) {
            stop("data2 must be a data.frame")
        }
        if (nrow(data2) == 0) {
            stop("data2 has no rows")
        }
        if (ncol(data2) < 2) {
            stop("data2 must have at least 2 columns")
        }

        if (!is.null(feature_col2)) {
            if (!feature_col2 %in% colnames(data2)) {
                stop(sprintf("feature_col2 '%s' not found in data2 columns: %s",
                            feature_col2, paste(colnames(data2), collapse=", ")))
            }
        }

        if (!is.null(design2)) {
            if (!is.data.frame(design2)) {
                stop("design2 must be a data.frame")
            }
            if (!sample_col2 %in% colnames(design2)) {
                stop(sprintf("sample_col2 '%s' not found in design2 columns: %s",
                            sample_col2, paste(colnames(design2), collapse=", ")))
            }
        }
    }

    invisible(NULL)
}

detect_feature_column <- function(data) {
    if (!is.data.frame(data)) {
        stop("data must be a data.frame")
    }
    if (ncol(data) == 0) {
        stop("data has no columns")
    }

    colnames(data)[1]
}

detect_sample_columns <- function(data, feature_col, design = NULL) {
    all_cols <- colnames(data)

    non_sample <- feature_col

    stat_cols <- detect_stat_columns(data)
    non_sample <- c(non_sample, stat_cols)

    sample_cols <- setdiff(all_cols, non_sample)

    if (!is.null(design)) {
    }

    sample_cols
}

detect_stat_columns <- function(data) {
    stat_patterns <- c(
        "P\\.Value", "adj\\.P\\.Val", "p\\.value", "pvalue",
        "logFC", "log2FC", "FoldChange",
        "AveExpr", "t\\.value", "B\\.value"
    )

    pattern <- paste(stat_patterns, collapse = "|")
    grep(pattern, colnames(data), value = TRUE, ignore.case = TRUE)
}

has_preloaded_data <- function() {
    preloaded <- getOption("omicloupe.preloaded", default = NULL)
    !is.null(preloaded) && !is.null(preloaded$data1)
}

get_preloaded_data <- function() {
    getOption("omicloupe.preloaded", default = NULL)
}
