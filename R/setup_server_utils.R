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

do_dataset_mapping <- function(rv, feature_col_1, feature_col_2, output, sample_cols1, sample_cols2, matched_samples, duplicates_method="stop") {
    
    get_output_text <- function(rv, type) {
        
        mo <- rv$mapping_obj()
        
        valid_types <- c("Dataset1", "Dataset2", "Both")
        if (type %in% valid_types) {
            out_text <- sprintf(
                "%s loaded, %s entries matched", 
                type, nrow(mo$get_combined_dataset())
            )
            
            if (type == "Both") {
                out_text <- c(out_text, sprintf(" (original number of rows: %s and %s)", mo$get_dataset1_nrow(), mo$get_dataset2_nrow()))
                if (mo$has_full_entries()) {
                    out_text <- sprintf("%s (%s with no missing values)", out_text, nrow(mo$get_combined_dataset(full_entries=TRUE)))
                }
            }
            sprintf("%s\n%s", out_text, "You can now explore your dataset using the top bar menu")
        }
        else {
            stop(sprintf("Unknown type state: %s", type))
        }
    }
    
    get_mapped_output_text <- function(rv, duplicates_method) {
        mo <- rv$mapping_obj()
        
        if (mo$has_combined()) {
            if (!mo$has_same_number_entries()) {
                if (duplicates_method == "stop") {
                    out_text <- "Datasets mapped, but not equal number of entries. Either fix, or use option 'Discard dups.' to proceed."
                }
                else if (duplicates_method == "discard") {
                    out_text <- "One or both had duplicate entries, discarding duplicates as 'discard' is assigned"
                }
                else {
                    stop("Unknown duplicates_method setting: ", duplicates_method)
                }
            }
            else {
                out_text <- get_output_text(rv, "Both")
            }
        }
        else {
            out_text <- "No samples mapped, check your data and your Feature columns!"
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
        output$load_status <- renderText({ get_output_text(rv, "Dataset1") })
    }
    else if (is.null(rv$filedata_1())) {
        rv$mapping_obj(MapObject$new(
            rv$filedata_2(), 
            feature_col_2, 
            samples2=sample_cols2
        ))
        output$load_status <- renderText({ get_output_text(rv, "Dataset2") })
    }
    else {
        
        mo <- MapObject$new(
            rv$filedata_1(), 
            feature_col_1, 
            rv$filedata_2(), 
            feature_col_2,
            samples1=sample_cols1,
            samples2=sample_cols2,
            matched=matched_samples,
            discard_dups=ifelse(duplicates_method=="discard", TRUE, FALSE)
        )
        
        rv$mapping_obj(mo)
        out_text <- get_mapped_output_text(rv, duplicates_method)
        
        # if (mo$has_combined()) {
        #     if (!mo$has_same_number_entries()) {
        #         if (duplicates_method == "stop") {
        #             out_text <- "Datasets mapped, but not equal number of entries. Either fix, or use option 'Discard dups.' to proceed."
        #         }
        #         else if (duplicates_method == "discard") {
        #             out_text <- "One or both had duplicate entries, discarding duplicates as 'discard' is assigned"
        #         }
        #         else {
        #             stop("Unknown duplicates_method setting: ", duplicates_method)
        #         }
        #     }
        #     else {
        #         out_text <- get_output_text(rv, "Both")
        #     }
        # }
        # else {
        #     out_text <- "No samples mapped, check your data and your Feature columns!"
        # }
        
        output$load_status <- renderText({ out_text })
    }
    rv
}


