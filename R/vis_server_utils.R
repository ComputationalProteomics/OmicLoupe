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

get_pass_thres_annot_data <- function(df, stat_cols1, stat_cols2, pvalue_cut, fold_cut, stat_pattern) {
    
    pass_threshold_data1 <- df[[stat_cols1[[stat_pattern]]]] < pvalue_cut & abs(df[[stat_cols1$logFC]]) > fold_cut
    pass_threshold_data2 <- df[[stat_cols2[[stat_pattern]]]] < pvalue_cut & abs(df[[stat_cols2$logFC]]) > fold_cut
    pass_both <- pass_threshold_data1 & pass_threshold_data2

    pass_type <- rep("None", length(pass_both))
    pass_type[pass_threshold_data1] <- "Ref"
    pass_type[pass_threshold_data2] <- "Comp"
    pass_type[pass_both] <- "Both"
    pass_type <- factor(pass_type, levels = c("None", "Both", "Ref", "Comp"))
    
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


di_new <- function(rv, input_field, field) {
    
    if (is.null(rv$filename_1()) || rv$filename_1() == "") {
        NULL
    }
    else if (input_field == rv$filename_1()) {
        1
    }
    else if (!is.null(rv$filename_2()) && input_field == rv$filename_2()) {
        2
    }
    else {
        NULL
    }
}

# di <- function(rv, input, field) {
#     
#     if (is.null(rv$filename_1()) || rv$filename_1() == "") {
#         NULL
#     }
#     else if (input[[sprintf("dataset%s", field)]] == rv$filename_1()) {
#         1
#     }
#     else if (!is.null(rv$filename_2()) && input[[sprintf("dataset%s", field)]] == rv$filename_2()) {
#         2
#     }
#     else {
#         NULL
#     }
# }







