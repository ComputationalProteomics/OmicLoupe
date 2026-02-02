pca_calculation <- function(data, center = TRUE, scale = TRUE) {
    if (any(is.na(data))) {
        stop("Data contains missing values (NA). Please remove or impute them before PCA.")
    }

    data_t <- t(data)

    pca_result <- prcomp(data_t, center = center, scale. = scale)

    return(pca_result)
}

calculate_pca_obj <- function(rdf, samples, do_scale, do_center, var_cut, return_df=FALSE, col_prefix=NULL) {
    
    if (length(samples) < 2) {
        stop("PCA requires at least 2 samples.")
    }
    if (!is.numeric(var_cut) || length(var_cut) != 1 || is.na(var_cut) || var_cut < 0 || var_cut > 1) {
        stop("var_cut must be a single number between 0 and 1.")
    }

    missing_samples <- setdiff(samples, colnames(rdf))
    if (length(missing_samples) > 0) {
        stop(sprintf(
            "Sample columns not found in data: %s",
            paste(missing_samples, collapse = ", ")
        ))
    }

    sdf <- rdf[, samples, drop = FALSE]
    non_numeric_samples <- names(sdf)[!vapply(sdf, is.numeric, logical(1))]
    if (length(non_numeric_samples) > 0) {
        stop(sprintf(
            "PCA sample columns must be numeric. Non-numeric columns: %s",
            paste(non_numeric_samples, collapse = ", ")
        ))
    }

    sdf_matrix <- as.matrix(sdf)

    has_inf_rows <- apply(sdf_matrix, 1, function(elem) { any(is.infinite(elem)) })
    has_nan_rows <- apply(sdf_matrix, 1, function(elem) { any(is.nan(elem)) })

    valid_pca_rows <- complete.cases(sdf_matrix) & !has_inf_rows & !has_nan_rows
    sdf_complete <- sdf_matrix[valid_pca_rows, , drop = FALSE]

    if (nrow(sdf_complete) == 0) {
        stop("No features remain after removing rows with missing/NaN/Inf values.")
    }

    row_vars <- apply(sdf_complete, 1, stats::var)
    nonzero_var <- !is.na(row_vars) & row_vars > 0
    sdf_nonzero <- sdf_complete[nonzero_var, , drop = FALSE]

    if (nrow(sdf_nonzero) == 0) {
        stop("No features with non-zero variance remain for PCA after filtering.")
    }

    if (var_cut > 0) {
        cutoff_value <- as.numeric(stats::quantile(row_vars[nonzero_var], var_cut))
        var_filter_contrast <- row_vars[nonzero_var] >= cutoff_value
    } else {
        var_filter_contrast <- rep(TRUE, nrow(sdf_nonzero))
    }

    sdf_complete_varfilt <- sdf_nonzero[var_filter_contrast, , drop = FALSE]

    if (nrow(sdf_complete_varfilt) == 0) {
        stop(sprintf(
            "Variance filter removed all features (cutoff=%s). Lower the cutoff or disable variance filtering.",
            var_cut
        ))
    }

    pca_obj <- prcomp(t(sdf_complete_varfilt), scale. = do_scale, center = do_center)
    
    if (!return_df) {
        pca_obj
    }
    else {
        idx_complete <- which(valid_pca_rows)
        idx_nonzero <- which(nonzero_var)
        idx_varfilt <- which(var_filter_contrast)
        idx_keep <- idx_complete[idx_nonzero[idx_varfilt]]

        rdf_target <- rdf[idx_keep, , drop = FALSE]
        rot_df <- pca_obj$rotation
        if (!is.null(col_prefix)) {
            colnames(rot_df) <- paste0(col_prefix, colnames(rot_df))
        }
        cbind(rdf_target, rot_df)
    }
}
