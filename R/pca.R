calculate_pca_obj <- function(rdf, samples, do_scale, do_center, var_cut, return_df=FALSE, col_prefix=NULL) {
    
    sdf <- rdf[, samples]
    variance_filter <- function(m, cutoff) {
        vars <- apply(m, 1, var)
        vars > quantile(vars, cutoff)
    }
    
    has_inf_rows <- apply(sdf, 1, function(elem) { any(is.infinite(elem)) } )
    
    valid_pca_rows <- complete.cases(sdf) & !has_inf_rows
    
    sdf_complete <- sdf[valid_pca_rows, ] 
    
    var_filter_contrast <- variance_filter(sdf_complete, cutoff=var_cut)
    sdf_complete_varfilt <- sdf_complete[var_filter_contrast, ]
    pca_obj <- prcomp(t(sdf_complete_varfilt), scale. = do_scale, center=do_center)
    
    if (!return_df) {
        pca_obj
    }
    else {
        rdf_target <- rdf %>% 
            dplyr::filter(valid_pca_rows) %>%
            dplyr::filter(var_filter_contrast)
        rot_df <- pca_obj$rotation
        if (!is.null(col_prefix)) {
            colnames(rot_df) <- paste0(col_prefix, colnames(rot_df))
        }
        cbind(rdf_target, rot_df)
    }
}

