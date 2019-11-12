# Parses out the set of four statistical columns from the total set of
# statistical columns given a specific base
# For example: condA.P.Value, condA.adj.P.Val, condA.logFC, condA.AveExpr
parse_stat_cols <- function(raw_stat_cols, base) {
    
    get_target_column <- function(columns, base, statistic, accept_as_is=FALSE) {
        
        desired_column <- paste0(base, statistic)
        if (accept_as_is && statistic %in% columns) {
            statistic
        }
        else if (desired_column %in% columns) {
            desired_column
        }
        else {
            stop(sprintf(
                "No match for desired column: %s among: %s", 
                desired_column, paste(columns, collapse=", ")
            )
            )
        }
    }
    
    stat_cols <- list()
    stat_cols$P.Value <- get_target_column(raw_stat_cols, base, "P.Value")
    stat_cols$adj.P.Val <- get_target_column(raw_stat_cols, base, "adj.P.Val")
    stat_cols$logFC <- get_target_column(raw_stat_cols, base, "logFC")
    stat_cols$AveExpr <- get_target_column(raw_stat_cols, base, "AveExpr", accept_as_is = TRUE)
    stat_cols
}

# Retrieves a subset of a dataframe where it is checked what points passes both
# a statistical threshold and a fold threshold
get_pass_thres_annot_data_old <- function(df, stat_cols, pvalue_cut, fold_cut, stat_pattern) {
    
    pass_threshold_data <- df[[stat_cols[[stat_pattern]]]] < pvalue_cut & abs(df[[stat_cols$logFC]]) > fold_cut
    plot_df <- cbind(df, pass_threshold_data) %>% arrange(desc(UQ(as.name(stat_cols[[stat_pattern]]))))
    plot_df
}

get_pass_thres_annot_data <- function(df, stat_cols1, stat_cols2, pvalue_cut, fold_cut, stat_pattern) {
    
    pass_threshold_data1 <- df[[stat_cols1[[stat_pattern]]]] < pvalue_cut & abs(df[[stat_cols1$logFC]]) > fold_cut
    pass_threshold_data2 <- df[[stat_cols2[[stat_pattern]]]] < pvalue_cut & abs(df[[stat_cols2$logFC]]) > fold_cut
    pass_both <- pass_threshold_data1 & pass_threshold_data2
    # pass_neither <- !pass_threshold_data1 & !pass_threshold_data2
    
    pass_type <- rep("NONE", length(pass_both))
    pass_type[pass_threshold_data1] <- "D1"
    pass_type[pass_threshold_data2] <- "D2"
    pass_type[pass_both] <- "BOTH"
    pass_type <- factor(pass_type, levels = c("NONE", "BOTH", "D1", "D2"))
    
    # pass_type <- cbind(d1=pass_threshold_data1, d2=pass_threshold_data2) %>%
    # pass_type <- map2(
    #     pass_threshold_data1, 
    #     pass_threshold_data2,
    #     function(val1, val2) {
    #         
    #         if (is.na(val1)) val1 <- FALSE
    #         if (is.na(val2)) val2 <- FALSE
    #         
    #         if (val1 && val2) "BOTH"
    #         else if (val1) "D1"
    #         else if (val2) "D2"
    #         else "NONE"
    #         
    #     }) %>% unlist()
    
    plot_df <- cbind(df, pass_threshold_data=pass_type) %>% arrange(desc(UQ(as.name(stat_cols1[[stat_pattern]]))))
    plot_df
}


get_curr_selected_cols_pattern <- function(chosen_dataset, filenames, pattern1="selected_cols_1", pattern2="selected_cols_2s") {
    if (chosen_dataset == filenames[1]) {
        pattern1
    }
    else if (length(filenames) > 1 && chosen_dataset == filenames[2]) {
        pattern2
    }
    else {
        stop("Unknown situation for input$dataset1: ", dataset)
    }
}












