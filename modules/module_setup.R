source("R/setup_server_utils.R")
source("R/setup_ui_utils.R")

setup_panel_ui <- function(id) {
    
    ns <- NS(id)
    tabPanel(
        id,
        fluidPage(
            id = "outer_area",
            tags$style(
                type = "text/css",
                ".button_row { padding: 5px; }",
                "#column_select_noselectize { height: 500px; }"
            ),
            fluidRow(
                column(4,
                       # sample_input_well(ns("data_file_1"), ns("data_selected_columns_1")),
                       conditionalPanel(
                           sprintf("input['%s'] == 1", ns("select_dataset")),
                           h3("Dataset 1"),
                           sample_input_well(ns("data_file_1"), ns("data_selected_columns_1"))
                       ),
                       conditionalPanel(
                           sprintf("input['%s'] == 2", ns("select_dataset")),
                           h3("Dataset 2"),
                           sample_input_well(ns("data_file_2"), ns("data_selected_columns_2"))
                       )
                ),  
                column(3,
                       align="center",
                       wellPanel(
                           select_button_row("Select samples", ns("sample_select_button_1"), ns("sample_deselect_button_1")),
                           select_button_row("Select stat groups", ns("stat_select_button_1"), ns("stat_deselect_button_1")),
                           # select_button_row("Select samples 2", ns("sample_select_button_2"), ns("sample_deselect_button_2")),
                           # select_button_row("Select stat groups 2", ns("stat_select_button_2"), ns("stat_deselect_button_2")),
                           action_button_row(ns("autodetect_stat_cols"), "Autodetect"),
                           textInput(ns("sample_pattern"), "Sample pattern"),
                           action_button_row(ns("autodetect_sample_cols"), "Autodetect samples"),
                           action_button_row(ns("selection_clear_button"), "Clear selection"),
                           fluidRow(
                               class = "button_row",
                               column(6,
                                      checkboxInput(ns("toggle_dataset2"), label = "Toggle dataset 2", value = FALSE),
                                      selectInput(ns("select_dataset"), label = "Select dataset", choices = c("Sample 1"=1,"Sample 2"=2), selected = 1)
                               ),
                               column(6,
                                      checkboxInput(ns("autodetect_statcols_toggle"), label = "Autodetect", value = FALSE)
                               )
                           )
                       ),
                       wellPanel(
                           textOutput(ns("perform_map_status"))
                       ),
                       informative_text()
                ),
                column(5,
                       conditionalPanel(
                           sprintf("input['%s'] == 1", ns("select_dataset")),
                           h3("Dataset 1"),
                           selected_sample_well(
                               ns("sample_selected_1"),
                               ns("statcols_selected_1"),
                               ns("feature_col_1"),
                               ns("found_stat_patterns_1")
                           )
                       ),
                       conditionalPanel(
                           sprintf("input['%s'] == 2", ns("select_dataset")),
                           h3("Dataset 2"),
                           selected_sample_well(
                               ns("sample_selected_2"),
                               ns("statcols_selected_2"),
                               ns("feature_col_2"),
                               ns("found_stat_patterns_2")
                           )
                       )
                )
            )
        )
    )
}

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
    
    # ------------------- Sample Management --------------------
    
    observeEvent(input$sample_select_button_1, {
        
        data_nbr <- input$select_dataset
        filename <- rv[[sprintf("filename_%s", data_nbr)]]()
        
        selected_samples <- column_selection_action(
            input[[sprintf("data_selected_columns_%s", data_nbr)]],
            rv$selected_cols_obj()[[filename]]$samples
        )

        rv <- update_selcol_obj(rv, filename, "samples", selected_samples)

        sync_select_inputs(
            session,
            sprintf("data_selected_columns_%s", data_nbr),
            sprintf("sample_selected_%s", data_nbr),
            rv[[sprintf("filedata_%s", data_nbr)]],
            selected_samples
        )
    })
    
    observeEvent(input$sample_deselect_button_1, {
        
        data_nbr <- input$select_dataset
        filename <- rv[[sprintf("filename_%s", data_nbr)]]()
        
        selected_samples <- column_selection_action(
            input[[sprintf("sample_selected_%s", data_nbr)]],            
            rv$selected_cols_obj()[[rv$filename_1()]]$samples, 
            is_deselect = TRUE
        )
        rv <- update_selcol_obj(rv, rv$filename_1(), "samples", selected_samples)
        sync_select_inputs(
            session, 
            sprintf("data_selected_columns_%s", data_nbr),
            sprintf("sample_selected_%s", data_nbr),
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
        
        data_nbr <- input$select_dataset
        filename <- rv[[sprintf("filename_%s", data_nbr)]]()
        
        selected_statcols <- column_selection_action(
            input[[sprintf("data_selected_columns_%s", data_nbr)]],
            rv$selected_cols_obj()[[filename]]$statcols
        )
        
        rv <- update_selcol_obj(rv, filename, "statcols", selected_statcols, sync_stat_patterns = TRUE)
        sync_select_inputs(
            session, 
            sprintf("data_selected_columns_%s", data_nbr),
            sprintf("statcols_selected_%s", data_nbr),
            rv[[sprintf("filedata_%s", data_nbr)]],
            selected_statcols
        )
        update_statpatterns_display(
            rv$selected_cols_obj()[[filename]]$statpatterns, 
            sprintf("found_stat_patterns_%s", data_nbr)
        )
    })
    
    observeEvent(input$stat_deselect_button_1, {
        data_nbr <- input$select_dataset
        filename <- rv[[sprintf("filename_%s", data_nbr)]]()
        
        selected_statcols <- column_selection_action(
            input[[sprintf("statcols_selected_%s", data_nbr)]],
            rv$selected_cols_obj()[[filename]]$statcols,
            is_deselect = TRUE
        )
        
        rv <- update_selcol_obj(rv, filename, "statcols", selected_statcols, sync_stat_patterns = TRUE)
        sync_select_inputs(
            session, 
            sprintf("data_selected_columns_%s", data_nbr),
            sprintf("statcols_selected_%s", data_nbr),
            rv[[sprintf("filedata_%s", data_nbr)]],
            selected_statcols
        )
        update_statpatterns_display(
            rv$selected_cols_obj()[[filename]]$statpatterns, 
            sprintf("found_stat_patterns_%s", data_nbr)
        )
    })
    
    observeEvent(input$feature_col_1, {
        if (!is.null(rv$filename_1()) && !is.null(rv$mapping_obj())) {
            rv <- update_selcol_obj(rv, rv$filename_1(), "feature_col", input$feature_col_1)
        }
    })
    # --------------------------- End ----------------------------
    
    # ------------------- Sample 2 Management --------------------
    # observeEvent(input$sample_select_button_2, {
    #     selected_samples <- column_selection_action(
    #         input$data_selected_columns_2, 
    #         rv$selected_cols_obj()[[rv$filename_2()]]$samples
    #     )
    #     rv <- update_selcol_obj(rv, rv$filename_2(), "samples", selected_samples)
    #     sync_select_inputs(
    #         session, 
    #         "data_selected_columns_2", 
    #         "sample_selected_2", 
    #         rv$filedata_2, 
    #         selected_samples
    #     )
    # })
    # 
    # observeEvent(input$sample_deselect_button_2, {
    #     selected_samples <- column_selection_action(
    #         input$sample_selected_2,
    #         rv$selected_cols_obj()[[rv$filename_2()]]$samples,
    #         is_deselect = TRUE
    #     )
    #     rv <- update_selcol_obj(rv, rv$filename_2(), "samples", selected_samples)
    #     sync_select_inputs(
    #         session,
    #         "data_selected_columns_2",
    #         "sample_selected_2",
    #         rv$filedata_2,
    #         selected_samples
    #     )
    # })

    # observeEvent(input$stat_select_button_2, {
    #     selected_statcols <- column_selection_action(
    #         input$data_selected_columns_2,
    #         rv$selected_cols_obj()[[rv$filename_2()]]$statcols
    #     )
    #     rv <- update_selcol_obj(rv, rv$filename_2(), "statcols", selected_statcols, sync_stat_patterns = TRUE)
    #     sync_select_inputs(
    #         session,
    #         "data_selected_columns_2",
    #         "statcols_selected_2",
    #         rv$filedata_2,
    #         selected_statcols
    #     )
    #     update_statpatterns_display(rv$selected_cols_obj()[[rv$filename_2()]]$statpatterns, "found_stat_patterns_2")
    # })
    # 
    # observeEvent(input$stat_deselect_button_2, {
    #     selected_statcols <- column_selection_action(
    #         input$statcols_selected_2,
    #         rv$selected_cols_obj()[[rv$filename_2()]]$statcols,
    #         is_deselect = TRUE
    #     )
    #     rv <- update_selcol_obj(rv, rv$filename_2(), "statcols", selected_statcols, sync_stat_patterns = TRUE)
    #     sync_select_inputs(
    #         session,
    #         "data_selected_columns_2",
    #         "statcols_selected_2",
    #         rv$filedata_2,
    #         selected_statcols
    #     )
    #     update_statpatterns_display(rv$selected_cols_obj()[[rv$filename_2()]]$statpatterns, "found_stat_patterns_2")
    # })
    
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

