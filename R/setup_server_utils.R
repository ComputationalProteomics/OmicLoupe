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

# autoselect_statpatterns <- function(dataset_cols, stat_patterns=c("P.Value", "PValue", "adj.P.Val", "AdjPVal", "logFC", "log2FoldChange", "AveExpr", "featureAvg")) {
# 
#     # grep_string <- sprintf("%s%s", paste(stat_patterns, collapse="$|"), "$")
#     dataset_cols[grepl(grep_string, dataset_cols)]
# }

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

do_dataset_mapping <- function(rv, feature_col_1, feature_col_2, output, sample_cols1, sample_cols2, matched_samples) {
    
    get_output_text <- function(rv, type) {
        if (type == "Dataset1" || type == "Dataset2" || type == "Both") {
            
            # browser()
            
            out_text <- sprintf(
                "%s loaded, %s entries matched", 
                type, nrow(rv$mapping_obj()$get_combined_dataset())
            )
            
            if (type == "Both") {
                out_text <- c(out_text, sprintf(" (original number of rows: %s and %s)", rv$mapping_obj()$get_dataset1_nrow(), rv$mapping_obj()$get_dataset2_nrow()))
                if (rv$mapping_obj()$has_full_entries()) {
                    out_text <- sprintf("%s (%s with no missing values)", out_text, nrow(rv$mapping_obj()$get_combined_dataset(full_entries=TRUE)))
                }
            }
            sprintf("%s\n%s", out_text, "You can now explore your dataset using the top bar menu")
        }
        # else if (type == "Combined") {
        #     
        # }
        else {
            stop(sprintf("Unknown type state: %s", type))
        }
    }
    
    if (is.null(rv$filedata_1()) && is.null(rv$filedata_2())) {
        output$load_status <- renderText({
            sprintf("Both datasets needs to be present, missing both")
        })
    }
    else if (is.null(rv$filedata_2())) {
        rv$mapping_obj(MapObject$new(
            rv$filedata_1(), 
            feature_col_1, 
            samples1=sample_cols1
        ))
        
        out_text <- get_output_text(rv, "Dataset1")
        output$load_status <- renderText({ out_text })
    }
    else if (is.null(rv$filedata_1())) {
        rv$mapping_obj(MapObject$new(
            rv$filedata_2(), 
            feature_col_2, 
            samples2=sample_cols2
        ))
        
        out_text <- get_output_text(rv, "Dataset2")
        output$load_status <- renderText({ out_text })
    }
    else {
        rv$mapping_obj(MapObject$new(
            rv$filedata_1(), 
            feature_col_1, 
            rv$filedata_2(), 
            feature_col_2,
            samples1=sample_cols1,
            samples2=sample_cols2,
            matched=matched_samples
        ))
        
        if (!is.null(rv$mapping_obj()$get_combined_dataset())) {
            out_text <- get_output_text(rv, "Both")
        }
        else {
            out_text <- "No samples mapped, check your data and your Feature columns!"
        }

        output$load_status <- renderText({ out_text })
    }
    rv
}


