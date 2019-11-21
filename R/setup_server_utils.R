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

do_dataset_mapping <- function(rv, feature_col_1, feature_col_2, output, sample_cols1, sample_cols2) {

    if (is.null(rv$filedata_1()) && is.null(rv$filedata_2())) {
        output$perform_map_status <- renderText({
            sprintf("Both datasets needs to be present, missing both")
        })
    }
    else if (is.null(rv$filedata_2())) {
        rv$mapping_obj(MapObject$new(
            rv$filedata_1(), 
            feature_col_1, 
            samples1=sample_cols1
        ))
        
        # To function 
        out_text <- sprintf(
            "Dataset1 present and mapped, %s entries matched", 
            nrow(rv$mapping_obj()$get_combined_dataset())
        )
        if (rv$mapping_obj()$has_full_entries()) {
            out_text <- sprintf("%s (%s full)", out_text, nrow(rv$mapping_obj()$get_combined_dataset(full_entries=TRUE)))
        }
        # 
        
        output$perform_map_status <- renderText({ out_text })
    }
    else if (is.null(rv$filedata_1())) {
        rv$mapping_obj(MapObject$new(
            rv$filedata_2(), 
            feature_col_2, 
            samples2=sample_cols2
        ))
        
        # To function
        out_text <- sprintf(
            "Dataset2 present and mapped, %s entries matched", 
            nrow(rv$mapping_obj()$get_combined_dataset())
        )
        if (rv$mapping_obj()$has_full_entries()) {
            out_text <- sprintf("%s (%s full)", out_text, nrow(rv$mapping_obj()$get_combined_dataset(full_entries=TRUE)))
        }
        #
        
        output$perform_map_status <- renderText({ out_text })
    }
    else {
        rv$mapping_obj(MapObject$new(
            rv$filedata_1(), 
            feature_col_1, 
            rv$filedata_2(), 
            feature_col_2,
            samples1=sample_cols1,
            samples2=sample_cols2
        ))
        
        # To function
        out_text <- sprintf(
            "Both datasets present and mapped, %s entries matched", 
            nrow(rv$mapping_obj()$get_combined_dataset())
        )
        if (rv$mapping_obj()$has_full_entries()) {
            out_text <- sprintf("%s (%s full)", out_text, nrow(rv$mapping_obj()$get_combined_dataset(full_entries=TRUE)))
        }
        #
        
        output$perform_map_status <- renderText({ out_text })
    }
    rv
}


