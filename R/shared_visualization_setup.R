# parse_stat_cols_for_visuals <- function(rv_filename1, rv_filename2, rv_selected_cols_obj, selected_dataset, selected_statbase) {
#     
#     warning("Can I omit this one?")
#     
#     filenames <- c(
#         rv_filename1,
#         rv_filename2
#     )
#     
#     selection_index <- which(filenames %in% selected_dataset)
#     
#     statcols <- rv_selected_cols_obj[[selected_dataset]]$statcols
#     parsed_cols <- parse_stat_cols(statcols, selected_statbase)
#     d_parsed_cols <- lapply(parsed_cols, function(elem) { sprintf("d%s.%s", selection_index, elem) })
#     d_parsed_cols
# }

get_dataset_choices <- function(reactive_vals) {
    choices <- c()
    if (!is.null(reactive_vals$filedata_1())) {
        choices <- c(choices, reactive_vals$filename_1())
    }
    if (!is.null(reactive_vals$filedata_2())) {
        choices <- c(choices, reactive_vals$filename_2())
    }
    choices
}

update_stat_inputs <- function(session, reactive_vals, dataset1_select, dataset2_select) {
    if (is.null(reactive_vals$filename_1()) && is.null(reactive_vals$filename_2())) {
        return()
    }
    
    choices_1 <- reactive_vals$selected_cols_obj()[[dataset1_select]]$statpatterns
    choices_2 <- reactive_vals$selected_cols_obj()[[dataset2_select]]$statpatterns
    
    updateSelectInput(session, "stat_base1", choices=choices_1, selected=choices_1[1])
    updateSelectInput(session, "stat_base2", choices=choices_2, selected=choices_2[1])
}

















