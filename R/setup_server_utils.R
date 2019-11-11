column_selection_action <- function(selected_columns, curr_select_cols, is_deselect=FALSE) {

    if (!is_deselect) {
        joint_selected_samples <- c(curr_select_cols, selected_columns)
    }
    else {
        joint_selected_samples <- setdiff(curr_select_cols, selected_columns)
    }
    joint_selected_samples
}

sync_select_inputs <- function(session, source_id, selection_id, filedata, joint_selected_samples) {

    all_headers <- colnames(filedata())
    updateSelectInput(
        session, 
        selection_id, 
        choices = all_headers[all_headers %in% joint_selected_samples]
    )
    updateSelectInput(
        session, 
        source_id, 
        choices = setdiff(all_headers, joint_selected_samples)
    )
}

autoselect_statpatterns <- function(dataset_cols, stat_patterns=c("P.Value", "adj.P.Val", "logFC", "AveExpr")) {

    grep_string <- sprintf("%s%s", paste(stat_patterns, collapse="$|"), "$")
    dataset_cols[grepl(grep_string, dataset_cols)]
    # rv$selected_cols_2$selected_statcols <- column_selection_action(input$data_selected_columns_2, rv$selected_cols_2, "selected_statcols")
}

reset_reactive_cols <- function(rv) {

    rv$selected_cols_obj(list())
    rv
}

clear_file_fields <- function(session, filedata, field_ids) {
    field_ids %>% walk(~updateSelectInput(session, .x, choices=colnames(filedata())))
}


clear_fields <- function(session, filedata, field_ids) {
    field_ids %>% walk(~updateSelectInput(session, .x, choices=c("")))
}


