calculate_pca_obj <- function(rdf, samples, do_scale, do_center, var_cut, return_df=FALSE) {
    
    sdf <- rdf[, samples]
    variance_filter <- function(m, cutoff) {
        vars <- apply(m, 1, var)
        vars > quantile(vars, cutoff)
    }
    
    sdf_complete <- sdf[complete.cases(sdf), ] 
    var_filter_contrast <- variance_filter(sdf_complete, cutoff=var_cut)
    sdf_complete_varfilt <- sdf_complete[var_filter_contrast, ]
    pca_obj <- prcomp(t(sdf_complete_varfilt), scale. = do_scale, center=do_center)
    
    if (!return_df) {
        pca_obj
    }
    else {
        rdf_target <- rdf %>% filter(complete.cases(sdf)) %>% filter(var_filter_contrast)
        cbind(rdf_target, pca_obj$rotation)
    }
}

