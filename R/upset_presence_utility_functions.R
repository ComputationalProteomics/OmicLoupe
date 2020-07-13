cond_cols_uppres <- function(rv, input, dataset, target, cond_col, cond_level) {
    
    ddf <- rv[[sprintf("ddf_%s", target)]](rv, dataset)
    # cond_col <- rv[[sprintf("ddf_condcol_%s", target)]](rv, dataset)
    sample_col <- rv[[sprintf("ddf_samplecol_%s", target)]](rv, input$dataset1)
    
    match_cond_filter <- ddf[[cond_col]] == cond_level
    match_cond_samples <- ddf %>% dplyr::filter(match_cond_filter) %>% pull(sample_col)
    match_cond_samples
}

get_na_nbrs_uppres <- function(rv, input, comb_data, selected_cond, selected_levels, dataset_nbr, target) {
    
    samples_per_cond <- map(
        selected_levels,
        ~cond_cols_uppres(rv, input, input[[sprintf("dataset%s", dataset_nbr)]], target, selected_cond, .x)
    ) %>% `names<-`(selected_levels)
    
    d_prefix <- sprintf("d%s", dataset_nbr)
    
    # map_obj <- rv$mapping_obj()$get_combined_dataset(include_non_matching=TRUE)
    
    non_missing_per_cond_df <- map(
        samples_per_cond,
        ~comb_data %>%
            dplyr::select(paste(d_prefix, .x, sep=".")) %>%
            mutate(nbr_na=(!is.na(.)) %>% rowSums()) %>%
            mutate(comb_id=comb_data$comb_id)
    ) %>%
        map(~dplyr::select(.x, nbr_na)) %>% 
        do.call("cbind", .) %>%
        `colnames<-`(paste(selected_levels, "nbr_na", sep=".")) %>%
        cbind(comb_id=comb_data$comb_id, .)
    
    non_missing_per_cond_df
}

parse_na_nbrs_to_upset_table <- function(nbr_nas_df, dataset, ddf, selected_cond, selected_levels, presence_fraction_thres) {
    
    tot_counts <- table(ddf[[selected_cond]]) %>% as.list()
    parsed <- nbr_nas_df[, -1, drop=FALSE] %>% rename_all(~gsub("\\.nbr_na", "", .))
    if (presence_fraction_thres == 0) {
        upset_table <- data.frame(ifelse(parsed > 0, 1, 0))
    }
    else {
        upset_table <- data.frame(ifelse(parsed >= (tot_counts[selected_levels] %>% map(~.*presence_fraction_thres)), 1, 0))
    }
    upset_table$comb_id <- nbr_nas_df$comb_id
    upset_table
}