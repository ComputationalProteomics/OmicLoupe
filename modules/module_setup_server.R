source("R/setup_server_utils.R")


module_setup_server <- function(input, output, session) {
    
    rv <- list()
    rv$filedata_1 <- reactive({
        infile <- input$data_file_1
        if (is.null(infile)) {
            return(NULL)
        }
        read_tsv(infile$datapath, col_types = cols())
    })
    rv$filedata_2 <- reactive({
        infile <- input$data_file_2
        if (is.null(infile)) {
            return(NULL)
        }
        read_tsv(infile$datapath, col_types = cols())
    })
    
    rv$selected_cols_obj <- reactiveVal({
        list()
    })
    
    rv$filename_1 <- reactive({
        infile <- input$data_file_1
        if (is.null(infile)) {
            return(NULL)
        }
        stringi::stri_extract_first(str = infile$name, regex = ".*")
    })
    
    rv$filename_2 <- reactive({
        infile <- input$data_file_2
        if (is.null(infile)) {
            return(NULL)
        }
        stringi::stri_extract_first(str = infile$name, regex = ".*")
    })
    
    rv$mapping_obj <- reactiveVal(NULL)
    rv$test_react <- reactiveVal(NULL)
    
    update_selcol_obj <- function(rv, dataset, colname, new_value, sync_stat_patterns=FALSE, stat_pattern="P.Value") {
        selcol_obj <- rv$selected_cols_obj()
        selcol_obj[[dataset]][[colname]] <- new_value
        
        if (sync_stat_patterns) {
            target_statcols <- selcol_obj[[dataset]][["statcols"]]
            match_bases <- target_statcols[grep(stat_pattern, target_statcols)]
            trimmed_patterns <- gsub(stat_pattern, "", match_bases)
            selcol_obj[[dataset]][["statpatterns"]] <- trimmed_patterns
        }
        
        rv$selected_cols_obj(selcol_obj)
        rv
    }
    
    selcol_obj_has_statpatterns <- function(selcol_obj, dataset) {
        if (!is.null(selcol_obj) && !is.null(selcol_obj[[dataset]]) && !is.null(selcol_obj[[dataset]][["statpatterns"]])) {
            TRUE
        }
        else {
            FALSE
        }
    }
    
    # ------------------- Sample 1 Management --------------------

    observeEvent(input$sample_select_button_1, {
        
        selected_samples <- column_selection_action(
            input$data_selected_columns_1,
            rv$selected_cols_obj()[[rv$filename_1()]]$samples
        )

        rv <- update_selcol_obj(rv, rv$filename_1(), "samples", selected_samples)

        sync_select_inputs(
            session, 
            "data_selected_columns_1", 
            "sample_selected_1", 
            rv$filedata_1, 
            selected_samples
        )
    })

    observeEvent(input$sample_deselect_button_1, {
        selected_samples <- column_selection_action(
            input$sample_selected_1,
            rv$selected_cols_obj()[[rv$filename_1()]]$samples, 
            is_deselect = TRUE
        )
        rv <- update_selcol_obj(rv, rv$filename_1(), "samples", selected_samples)
        sync_select_inputs(
            session, 
            "data_selected_columns_1", 
            "sample_selected_1", 
            rv$filedata_1, 
            selected_samples
        )
    })

    update_statpatterns_display <- function(statpatterns, target_out) {
        # found_stat_patterns_1/2
        if (!is.null(statpatterns)) {
            out_text <- paste(statpatterns, collapse=", ")
        }
        else {
            out_text <- "No patterns found"
        }
        output[[target_out]] <- renderText({
            out_text
        })
    }
    
    observeEvent(input$stat_select_button_1, {
        selected_statcols <- column_selection_action(
            input$data_selected_columns_1, 
            rv$selected_cols_obj()[[rv$filename_1()]]$statcols 
        )
        rv <- update_selcol_obj(rv, rv$filename_1(), "statcols", selected_statcols, sync_stat_patterns = TRUE)
        sync_select_inputs(
            session, 
            "data_selected_columns_1", 
            "statcols_selected_1", 
            rv$filedata_1, 
            selected_statcols
        )
        update_statpatterns_display(rv$selected_cols_obj()[[rv$filename_1()]]$statpatterns, "found_stat_patterns_1")
    })

    observeEvent(input$stat_deselect_button_1, {
        selected_statcols <- column_selection_action(
            input$statcols_selected_1, 
            rv$selected_cols_obj()[[rv$filename_1()]]$statcols,
            is_deselect = TRUE
        )
        rv <- update_selcol_obj(rv, rv$filename_1(), "statcols", selected_statcols, sync_stat_patterns = TRUE)
        sync_select_inputs(
            session, 
            "data_selected_columns_1", 
            "statcols_selected_1", 
            rv$filedata_1, 
            selected_statcols
        )
        update_statpatterns_display(rv$selected_cols_obj()[[rv$filename_1()]]$statpatterns, "found_stat_patterns_1")
    })

    observeEvent(input$feature_col_1, {
        if (!is.null(rv$filename_1()) && !is.null(rv$mapping_obj())) {
            rv <- update_selcol_obj(rv, rv$filename_1(), "feature_col", input$feature_col_1)
        }
    })
    # --------------------------- End ----------------------------
    
    # ------------------- Sample 2 Management --------------------
    observeEvent(input$sample_select_button_2, {
        selected_samples <- column_selection_action(
            input$data_selected_columns_2, 
            rv$selected_cols_obj()[[rv$filename_2()]]$samples
        )
        rv <- update_selcol_obj(rv, rv$filename_2(), "samples", selected_samples)
        sync_select_inputs(
            session, 
            "data_selected_columns_2", 
            "sample_selected_2", 
            rv$filedata_2, 
            selected_samples
        )
    })
    
    observeEvent(input$sample_deselect_button_2, {
        selected_samples <- column_selection_action(
            input$sample_selected_2, 
            rv$selected_cols_obj()[[rv$filename_2()]]$samples,
            is_deselect = TRUE
        )
        rv <- update_selcol_obj(rv, rv$filename_2(), "samples", selected_samples)
        sync_select_inputs(
            session, 
            "data_selected_columns_2", 
            "sample_selected_2", 
            rv$filedata_2, 
            selected_samples
        )
    })
    
    observeEvent(input$stat_select_button_2, {
        selected_statcols <- column_selection_action(
            input$data_selected_columns_2, 
            rv$selected_cols_obj()[[rv$filename_2()]]$statcols
        )
        rv <- update_selcol_obj(rv, rv$filename_2(), "statcols", selected_statcols, sync_stat_patterns = TRUE)
        sync_select_inputs(
            session, 
            "data_selected_columns_2", 
            "statcols_selected_2", 
            rv$filedata_2, 
            selected_statcols
        )
        update_statpatterns_display(rv$selected_cols_obj()[[rv$filename_2()]]$statpatterns, "found_stat_patterns_2")
    })
    
    observeEvent(input$stat_deselect_button_2, {
        selected_statcols <- column_selection_action(
            input$statcols_selected_2,
            rv$selected_cols_obj()[[rv$filename_2()]]$statcols,
            is_deselect = TRUE
        )
        rv <- update_selcol_obj(rv, rv$filename_2(), "statcols", selected_statcols, sync_stat_patterns = TRUE)
        sync_select_inputs(
            session, 
            "data_selected_columns_2", 
            "statcols_selected_2", 
            rv$filedata_2, 
            selected_statcols
        )
        update_statpatterns_display(rv$selected_cols_obj()[[rv$filename_2()]]$statpatterns, "found_stat_patterns_2")
    })
    
    observeEvent(input$feature_col_2, {
        if (!is.null(input$dataset_2) && !is.null(rv$mapping_obj())) {
            rv$selected_cols_2$feature_col <- input$feature_col_2
            rv <- update_selcol_obj(rv, rv$filename_2(), "feature_col", input$feature_col_2)
        }
    })
    # --------------------------- End ----------------------------
    
    observeEvent(input$selection_clear_button, {
        clear_fields(session, rv$filedata_1, c("sample_selected", "statcols_selected"))
        clear_file_fields(session, rv$filedata_1, c("data1_selected_columns", "feature_col"))
        clear_fields(session, rv$filedata_1, c("sample_selected", "statcols_selected"))
        clear_file_fields(session, rv$filedata_1, c("data1_selected_columns", "feature_col"))
        rv <- reset_reactive_cols(rv)
    })
    
    autodetect_stat_cols <- function() {
        selected_statcols <- autoselect_statpatterns(colnames(rv$filedata_1()))
        if (!is.null(rv$filename_1())) {
            rv <- update_selcol_obj(rv, rv$filename_1(), "statcols", selected_statcols, sync_stat_patterns = TRUE)
            sync_select_inputs(session, "data_selected_columns_1", "statcols_selected_1", rv$filedata_1, selected_statcols)
            update_statpatterns_display(rv$selected_cols_obj()[[rv$filename_1()]]$statpatterns, "found_stat_patterns_1")
            # rv$selected_cols_obj()[[rv$filename_1()]][["statcols"]] <- selected_statcols
        }
        
        selected_statcols_2 <- autoselect_statpatterns(colnames(rv$filedata_2()))
        if (!is.null(rv$filename_2())) {
            rv <- update_selcol_obj(rv, rv$filename_2(), "statcols", selected_statcols_2, sync_stat_patterns = TRUE)
            # rv$selected_cols_obj()[[rv$filename_1()]][["statcols"]] <- selected_statcols_2
            sync_select_inputs(session, "data_selected_columns_2", "statcols_selected_2", rv$filedata_2, selected_statcols_2)
            update_statpatterns_display(rv$selected_cols_obj()[[rv$filename_2()]]$statpatterns, "found_stat_patterns_2")
        }
    }
    
    observeEvent(input$autodetect_stat_cols, {
        autodetect_stat_cols()
    })
    
    output$found_stat_patterns_2 <- renderText({

        if (selcol_obj_has_statpatterns(rv$selected_col_obj, input$dataset_2)) {
            stat_patterns <- rv$selected_cols_obj()[[rv$filename_2()]]$statpatterns
            if (length(statpatterns) > 0) {
                found_patterns_text <- paste(stat_patterns, collapse=", ")
            }
            else {
                found_patterns_text <- "None"
            }
            paste("Found base patterns:", found_patterns_text)
        }
        else {
            "Nothing found"
        }
    })
    
        
    observeEvent(rv$filedata_1(), {
        clear_fields(session, rv$filedata_1, c("sample_selected_1", "statcols_selected_1"))
        clear_file_fields(session, rv$filedata_1, c("data_selected_columns_1", "feature_col_1"))
        rv$selected_cols_obj(
            c(rv$selected_cols_obj(), setNames(list(list()), rv$filename_1()))
        )
        if (input$autodetect_statcols_toggle) {
            autodetect_stat_cols()
        }
    })
    
    observeEvent(rv$filedata_2(), {
        clear_fields(session, rv$filedata_2, c("sample_selected_2", "statcols_selected_2"))
        clear_file_fields(session, rv$filedata_2, c("data_selected_columns_2", "feature_col_2"))
        rv$selected_cols_obj(
            c(rv$selected_cols_obj(), setNames(list(list()), rv$filename_2()))
        )
        if (input$autodetect_statcols_toggle) {
            autodetect_stat_cols()
        }
    })
    
    do_dataset_mapping <- function() {
        if (is.null(rv$filedata_1()) && is.null(rv$filedata_2())) {
            output$perform_map_status <- renderText({
                sprintf("Both datasets needs to be present, missing both")
            })
        }
        else if (is.null(rv$filedata_2())) {
            print("Performing new map")
            rv$mapping_obj(MapObject$new(rv$filedata_1(), input$feature_col_1))
            output$perform_map_status <- renderText({
                sprintf("Dataset1 present and mapped, %s entries matched", nrow(rv$mapping_obj()$get_combined_dataset()))
            })
        }
        else if (is.null(rv$filedata_1())) {
            rv$mapping_obj(MapObject$new(rv$filedata_2(), input$feature_col_2))
            output$perform_map_status <- renderText({
                sprintf("Dataset2 present and mapped, %s entries matched", nrow(rv$mapping_obj()$get_combined_dataset()))
            })
        }
        else {
            rv$mapping_obj(MapObject$new(rv$filedata_1(), input$feature_col_1, rv$filedata_2(), input$feature_col_2))
            output$perform_map_status <- renderText({
                sprintf("Both datasets present and mapped! %s entries matched", nrow(rv$mapping_obj()$get_combined_dataset()))
            })
        }
    }
    
    observeEvent({
        input$perform_map_button
        input$feature_col_1
        input$feature_col_2
    }, {
        do_dataset_mapping()
    })
    
    observeEvent(rv$mapping_obj, {
        message("Mapping object obtained!")
    })
    
    return(rv)
}

