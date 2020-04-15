# Parses out the set of four statistical columns from the total set of
# statistical columns given a specific base
# For example: condA.P.Value, condA.adj.P.Val, condA.logFC, condA.AveExpr
parse_stat_cols <- function(raw_stat_cols, base, stat_patterns) {
    
    get_target_column <- function(columns, base, statistic, accept_as_is=FALSE) {
        
        desired_columns <- paste0(base, statistic)
        if (accept_as_is && any(statistic %in% columns)) {
            statistic[statistic %in% columns]
        }
        else if (any(desired_columns %in% columns)) {
            desired_columns[desired_columns %in% columns]
        }
        else {
            stop(sprintf(
                "No match for desired columns: %s among: %s", 
                paste(desired_columns, collapse=", "), paste(columns, collapse=", ")
            )
            )
        }
    }
    
    stat_cols <- list()
    stat_cols$P.Value <- get_target_column(raw_stat_cols, base, stat_patterns$P.Value)
    stat_cols$adj.P.Val <- get_target_column(raw_stat_cols, base, stat_patterns$adj.P.Val)
    stat_cols$logFC <- get_target_column(raw_stat_cols, base, stat_patterns$logFC)
    stat_cols$AveExpr <- get_target_column(raw_stat_cols, base, stat_patterns$AveExpr, accept_as_is = TRUE)
    
    stat_cols
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


di_new <- function(rv, input_field, dummy=NULL) {
    
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

di <- di_new

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







