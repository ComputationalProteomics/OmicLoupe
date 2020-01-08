source("R/setup_server_utils.R")
source("R/setup_ui_utils.R")

setup_panel_ui <- function(id) {
    
    ns <- NS(id)
    tabPanel(
        id,
        fluidPage(
            id = "outer_area",
            useShinyjs(),
            useShinyalert(),
            tags$head(
                tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap")
            ),
            tags$style(
                type = "text/css",
                
                ".recolor_button { color: #fff; background-color: #337ab7; border-color: #2e6da4; }",
                ".recolor_button:hover { color: #fff; background-color: #2269a6; border-color: #2e6da4; }",
                ".recolor_button:active:focus { color: #fff; background-color: #115895; border-color: #115895; }",
                ".recolor_button:focus { color: #fff; background-color: #2269a6; border-color: #2e6da4; }",
                
                ".recolor_button_red { color: #fff; background-color: #aa0000; border-color: #aa0000; }",
                ".recolor_button_red:hover { color: #fff; background-color: #aa0000; border-color: #aa0000; }",
                ".recolor_button_red:active:focus { color: #fff; background-color: #aa0000; border-color: #aa0000; }",
                ".recolor_button_red:focus { color: #fff; background-color: #aa0000; border-color: #aa0000; }",
                
                ".recolor_button_gray { color: #fff; background-color: #ccc; border-color: #ccc; }",
                ".recolor_button_gray:hover { color: #fff; background-color: #bbb; border-color: #bbb; }",
                ".recolor_button_gray:active:focus { color: #fff; background-color: #aaa; border-color: #aaa; }",
                ".recolor_button_gray:focus { color: #fff; background-color: #bbb; border-color: #bbb; }",
                
                ".recolor_button_yellow { color: #fff; background-color: #aa0; border-color: #aa0; }",
                ".recolor_button_yellow:hover { color: #fff; background-color: #aa0; border-color: #aa0; }",
                ".recolor_button_yellow:active:focus { color: #fff; background-color: #aa0; border-color: #aa0; }",
                ".recolor_button_yellow:focus { color: #fff; background-color: #aa0; border-color: #aa0; }",
                
                ".recolor_button { color: #fff; background-color: #337ab7; border-color: #2e6da4; }",
                ".recolor_button:hover { color: #fff; background-color: #2269a6; border-color: #2e6da4; }",
                ".recolor_button:active:focus { color: #fff; background-color: #115895; border-color: #115895; }",
                ".recolor_button:focus { color: #fff; background-color: #2269a6; border-color: #2e6da4; }",
                
                ".btn-file { color: #fff; background-color: #337ab7; border-color: #2e6da4; }",
                ".btn-file:hover { color: #fff; background-color: #2269a6; border-color: #2e6da4; }",
                ".btn-file:active:focus { color: #fff; background-color: #115895; border-color: #115895; }",
                ".btn-file:focus { color: #fff; background-color: #2269a6; border-color: #2e6da4; }",
                
                ".help { color: #000; background-color: #fff; border-color: #ccc; }",
                ".help:hover { color: #000; background-color: #fff; border-color: #ccc; }",
                ".help:active:focus { color: #000; background-color: #fff; border-color: #ccc; }",
                ".help:focus { color: #000; background-color: #fff; border-color: #ccc; }",
                
                ".well { background-color: #F3F3F3; border-color: #aaa; border-width: 1px; box-shadow: 2px 2px grey; }"
            ),
            tags$style(
                type = "text/css",
                ".button_row { padding: 5px; }",
                "#column_select_noselectize { height: 500px; }"
            ),
            
            tabsetPanel(
                id = ns("setup_panels"),
                type = "tabs",
                tabPanel("LoadData", 
                         top_bar_w_help("Load data", ns("help")),
                         fluidRow(
                             column(2, selectInput(ns("select_dataset"), label = "Select dataset", choices = c("Dataset 1"=1,"Dataset 2"=2), selected = 1)),
                             column(2),
                             column(5,
                                    p(HTML("<b>Status:</b>")),
                                    p("No columns assigned"),
                                    p("No datasets loaded"),
                                    textOutput(ns("column_status")),
                                    textOutput(ns("load_status")),
                                    textOutput(ns("perform_map_status"))
                             )
                         ),
                         fluidRow(
                             column(4,
                                    conditionalPanel(
                                        sprintf("input['%s'] == 1", ns("select_dataset")),
                                        sample_input_well(ns("data_file_1"), ns("data_selected_columns_1"), ns("feature_col_1"))
                                    ),
                                    conditionalPanel(
                                        sprintf("input['%s'] == 2", ns("select_dataset")),
                                        sample_input_well(ns("data_file_2"), ns("data_selected_columns_2"), ns("feature_col_2"))
                                    ),
                                    conditionalPanel(
                                        sprintf("input['%s'] == 1 || input['%s'] == 1", ns("select_dataset"), ns("matched_samples")),
                                        design_input_well(ns("design_file_1"), ns("design_sample_col_1"), ns("design_cond_col_1"))
                                    ),
                                    conditionalPanel(
                                        sprintf("input['%s'] == 2 && input['%s'] != 1", ns("select_dataset"), ns("matched_samples")),
                                        design_input_well(ns("design_file_2"), ns("design_sample_col_2"), ns("design_cond_col_2"))
                                    )
                             ),
                             column(3,
                                    align="center",
                                    wellPanel(
                                        fluidRow(
                                            class = "button_row",
                                            actionButton(
                                                ns("autodetect_cols"),
                                                class = "recolor_button",
                                                width = "80%",
                                                "Identify columns"
                                            )
                                        ),
                                        fluidRow(
                                            class = "button_row",
                                            actionButton(
                                                ns("perform_map_button"),
                                                class = "recolor_button",
                                                width = "80%",
                                                "Load data"
                                            )
                                        ),
                                        checkboxInput(ns("matched_samples"), label = "Matched samples", value = FALSE),
                                        checkboxInput(ns("toggle_extra_settings"), "Toggle extra settings", value = FALSE),
                                        conditionalPanel(
                                            sprintf("input['%s'] == 1", ns("toggle_extra_settings")),
                                            select_button_row("Select samples", ns("sample_select_button_1"), ns("sample_deselect_button_1")),
                                            select_button_row("Select stat groups", ns("stat_select_button_1"), ns("stat_deselect_button_1"))
                                        )
                                    )
                             ),
                             column(5,
                                    conditionalPanel(
                                        sprintf("input['%s'] == 1", ns("select_dataset")),
                                        wellPanel(
                                            selectInput(
                                                ns("sample_selected_1"),
                                                "Assigned sample columns (dataset 1)",
                                                choices = c(""),
                                                multiple = TRUE,
                                                selectize = FALSE,
                                                size = 12
                                            ),
                                            selectInput(
                                                ns("statcols_selected_1"),
                                                "Assigned statistics columns (dataset 1)",
                                                choices = c(""),
                                                multiple = TRUE,
                                                selectize = FALSE,
                                                size = 12
                                            ),
                                            textOutput(ns("found_stat_patterns_1"))
                                        )
                                    ),
                                    conditionalPanel(
                                        sprintf("input['%s'] == 2", ns("select_dataset")),
                                        wellPanel(
                                            selectInput(
                                                ns("sample_selected_2"),
                                                "Assigned sample columns (dataset 2)",
                                                choices = c(""),
                                                multiple = TRUE,
                                                selectize = FALSE,
                                                size = 12
                                            ),
                                            selectInput(
                                                ns("statcols_selected_2"),
                                                "Assigned statistics columns (dataset 2)",
                                                choices = c(""),
                                                multiple = TRUE,
                                                selectize = FALSE,
                                                size = 12
                                            ),
                                            textOutput(ns("found_stat_patterns_2"))
                                        )
                                    )
                             )
                         )
                ),
                tabPanel("TableSetup", 
                         top_bar_w_help("Table Setup", ns("help_table_setup")),
                         wellPanel(
                             fluidRow(
                                 column(
                                     12,
                                     column(6, numericInput(ns("trunc_length"), "Truncate strings longer than", value = 20)),
                                     column(6, numericInput(ns("round_digits"), "Round numbers digits", value = 3)),
                                     selectInput(
                                         ns("shown_fields"), 
                                         "Display fields", 
                                         choices=c("[Unassigned]"), 
                                         selected="[Unassigned]",
                                         multiple=TRUE
                                     )
                                 )
                             )
                         ),
                         tabsetPanel(
                             type = "tabs",
                             tabPanel("Raw data 1", DT::DTOutput(ns("raw_data1"))),
                             tabPanel("Raw data 2", DT::DTOutput(ns("raw_data2"))),
                             tabPanel("Design 1", DT::DTOutput(ns("dt_design1"))),
                             tabPanel("Design 2", DT::DTOutput(ns("dt_design2"))),
                             tabPanel("Mapped data", DT::DTOutput(ns("table_display")))
                         )
                )
            )
        )
    )
}



module_setup_server <- function(input, output, session, module_name) {
    
    # document.querySelectorAll("#navbar li a[data-value=Correlation]")
    
    observeEvent(input$help, {
        shinyalert(
            title = "Help: Setup page",
            text = help_setup_setup, 
            html = TRUE
        )
    })
    
    observeEvent(input$help_table_setup, {
        shinyalert(
            title = "Help: Table setup",
            text = help_table_setup,
            html = TRUE
        )
    })
    
    observeEvent(rv$mapping_obj(), {
        req(rv$mapping_obj()$get_combined_dataset())
        
        comb_data_cols <- rv$mapping_obj()$get_combined_dataset() %>% colnames()
        samples_ref <- NULL
        if (!is.null(input$data_file_1)) {
            samples_ref <- rv$samples(rv, input$data_file_1$name, prefix="d1.")
        }
        
        samples_comp <- NULL
        if (!is.null(input$data_file_2)) {
            samples_comp <- rv$samples(rv, input$data_file_2$name, prefix="d2.")
        }
        start_select <- comb_data_cols[!comb_data_cols %in% c(samples_ref, samples_comp)]
        updateSelectInput(session, "shown_fields", choices = comb_data_cols, selected=start_select)
    })
    
    observeEvent({
        input$trunc_length
        input$round_digits
        input$shown_fields
    }, {
        rv$table_settings(list(
            trunc_length = input$trunc_length,
            round_digits = input$round_digits,
            shown_fields = input$shown_fields
        ))
    })
    
    selected_id_reactive <- reactive({
        rv$mapping_obj()$get_combined_dataset()[input$table_display_rows_selected, ]$comb_id %>% as.character()
    })
    
    observeEvent(input$table_display_rows_selected, {
        rv$set_selected_feature(selected_id_reactive(), module_name)
    })
    
    output$table_display <- DT::renderDataTable({
        
        req(rv$mapping_obj())
        req(rv$mapping_obj()$get_combined_dataset())
        rv$dt_parsed_data(rv, rv$mapping_obj()$get_combined_dataset())
    })
    
    
    output$raw_data1 <- DT::renderDataTable({
        req(rv$filedata_1())
        rv$filedata_1()
    })
    
    output$raw_data2 <- DT::renderDataTable({
        req(rv$filedata_2())
        rv$filedata_2()
    })
    
    output$dt_design1 <- DT::renderDataTable({
        req(rv$design_1)
        rv$design_1()
    })
    
    output$dt_design2 <- DT::renderDataTable({
        req(rv$design_2)
        rv$design_2()
    })
    
    rv <- setup_reactive_values_obj(input)
    
    statcols <- function(rv, data_field, contrast_field, prefix_index=NULL) { 
        
        dataset_stat_cols <- rv$selected_cols_obj()[[data_field]]$statcols
        parsed_cols <- parse_stat_cols(dataset_stat_cols, contrast_field)
        if (!is.null(prefix_index)) {
            parsed_cols <- lapply(parsed_cols, function(elem) { sprintf("d%s.%s", prefix_index, elem) })
        }
        parsed_cols
    }
    
    rv$statcols_ref <- function(rv, data_field, contrast_field) {
        data_ind <- di_new(rv, data_field, 1)
        statcols(rv, data_field, contrast_field, prefix_index=data_ind)
    }
    
    rv$statcols_comp <- function(rv, data_field, contrast_field) {
        data_ind <- di_new(rv, data_field, 2)
        statcols(rv, data_field, contrast_field, prefix_index=data_ind)
    }
    
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
    
    # ------------------- Sample Management --------------------
    
    observeEvent(input$sample_select_button_1, {
        
        data_nbr <- input$select_dataset
        filename <- rv[[sprintf("filename_%s", data_nbr)]]()
        
        if (!is.null(input[[sprintf("data_selected_columns_%s", data_nbr)]])) {
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
        }
        else {
            warning("Trying to select with nothing marked")
        }
    })
    
    observeEvent(input$sample_deselect_button_1, {
        
        data_nbr <- input$select_dataset
        filename <- rv[[sprintf("filename_%s", data_nbr)]]()
        
        if (!is.null(input[[sprintf("statcols_selected_%s", data_nbr)]])) {
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
        }
        else {
            warning("Trying to deselect with nothing marked, ignoring")
        }
        
    })
    
    update_statpatterns_display <- function(statpatterns, target_out) {
        
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
        
        if (!is.null(input[[sprintf("data_selected_columns_%s", data_nbr)]])) {
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
        }
        else {
            warning("Trying to select with nothing marked, ignoring...")
        }
    })
    
    observeEvent(input$stat_deselect_button_1, {
        data_nbr <- input$select_dataset
        filename <- rv[[sprintf("filename_%s", data_nbr)]]()
        
        if (!is.null(input[[sprintf("statcols_selected_%s", data_nbr)]])) {
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
        }
        else {
            warning("Trying to deselect with nothing marked, ignoring")
        }
    })
    
    observeEvent(input$feature_col_1, {
        data_nbr <- input$select_dataset
        if (!is.null(rv[[sprintf("filename_%s", data_nbr)]]()) && !is.null(rv$mapping_obj())) {
            rv[[sprintf("selected_cols_", data_nbr)]]$feature_col <- input[[sprintf("feature_col_%s", data_nbr)]]
            rv <- update_selcol_obj(
                rv, 
                rv[[sprintf("filename_%s", data_nbr)]](), 
                "feature_col", 
                input[[sprintf("feature_col_%s", data_nbr)]]
            )
        }
    })
    # --------------------------- End ----------------------------
    
    autodetect_stat_cols <- function() {
        selected_statcols <- autoselect_statpatterns(colnames(rv$filedata_1()))
        if (!is.null(rv$filename_1())) {
            rv <- update_selcol_obj(rv, rv$filename_1(), "statcols", selected_statcols, sync_stat_patterns = TRUE)
            sync_select_inputs(session, "data_selected_columns_1", "statcols_selected_1", rv$filedata_1, selected_statcols)
            update_statpatterns_display(rv$selected_cols_obj()[[rv$filename_1()]]$statpatterns, "found_stat_patterns_1")
        }
        
        selected_statcols_2 <- autoselect_statpatterns(colnames(rv$filedata_2()))
        if (!is.null(rv$filename_2())) {
            rv <- update_selcol_obj(rv, rv$filename_2(), "statcols", selected_statcols_2, sync_stat_patterns = TRUE)
            sync_select_inputs(session, "data_selected_columns_2", "statcols_selected_2", rv$filedata_2, selected_statcols_2)
            update_statpatterns_display(rv$selected_cols_obj()[[rv$filename_2()]]$statpatterns, "found_stat_patterns_2")
        }
    }
    
    autodetect_sample_cols <- function(ddf, rdf, data_nbr, rv, filename) {
        
        full_match <- lapply(ddf, function(col) { as.character(col) %in% colnames(rdf) %>% all() } ) %>% unlist()
        sample_col <- colnames(ddf)[which(full_match)[1]]
        
        updateSelectInput(
            session, 
            sprintf("design_sample_col_%s", data_nbr), 
            choices=colnames(rv[[sprintf("design_%s", data_nbr)]]()), 
            selected = sample_col
        )
        
        if (is.null(ddf)) {
            stop("Data frame is NULL, invalid input provided!")
        }
        
        samples_from_ddf <- ddf[[sample_col]]
        if (all(samples_from_ddf %in% colnames(rdf))) {
            sync_select_inputs(
                session, 
                sprintf("data_selected_columns_%s", data_nbr),
                sprintf("sample_selected_%s", data_nbr),
                rv[[sprintf("filedata_%s", data_nbr)]], 
                samples_from_ddf
            )
            rv <- update_selcol_obj(rv, filename, "samples", samples_from_ddf)
        }
        else {
            if (length(which(samples_from_ddf %in% colnames(rdf))) == 0) {
                message("No samples from design matched to data, something is wrong!")
            }
            else {
                missing <- colnames(rdf)[!samples_from_ddf %in% colnames(rdf)]
                message("Not all samples matched, non-missing: ", paste(missing, collapse=", "))
            }
        }
    }
    
    observeEvent(input$autodetect_cols, {
        autodetect_stat_cols()
        if (!is.null(input$design_sample_col_1) && input$design_sample_col_1 != "" && !is.null(rv$filedata_1())) {
            autodetect_sample_cols(
                rv$design_1(),
                rv$filedata_1(),
                data_nbr = 1, 
                rv, 
                rv$filename_1()
            )
        }
        if (!is.null(input$design_sample_col_2) && input$design_sample_col_2 != "" && !is.null(rv$filedata_2())) {
            autodetect_sample_cols(
                rv$design_2(),
                rv$filedata_2(),
                data_nbr = 2,
                rv, 
                rv$filename_2()
            )
        }
        
        # if has entry with columns in one case - perform the mapping
        # Otherwise - be ready to recolor the Map datasets button
        # rv <- perform_mapping(rv, output, input$data_file_1, input$data_file_2, input$feature_col_1, input$feature_col_2)
        
    })
    
    observeEvent(rv$filedata_1(), {
        clear_fields(session, rv$filedata_1, c("sample_selected_1", "statcols_selected_1"))
        clear_file_fields(session, rv$filedata_1, c("data_selected_columns_1", "feature_col_1"))
        rv$selected_cols_obj(
            c(rv$selected_cols_obj(), setNames(list(list()), rv$filename_1()))
        )
    })
    
    observeEvent(rv$filedata_2(), {
        clear_fields(session, rv$filedata_2, c("sample_selected_2", "statcols_selected_2"))
        clear_file_fields(session, rv$filedata_2, c("data_selected_columns_2", "feature_col_2"))
        rv$selected_cols_obj(
            c(rv$selected_cols_obj(), setNames(list(list()), rv$filename_2()))
        )
    })
    
    observeEvent(rv$design_1(), {
        updateSelectInput(session, "design_sample_col_1", choices=colnames(rv$design_1()))
        if (length(colnames(rv$design_1())) > 1) start_cond <- colnames(rv$design_1())[2]
        else start_cond <- colnames(rv$design_1())[1]
        updateSelectInput(session, "design_cond_col_1", choices=colnames(rv$design_1()), selected = start_cond)
    })
    
    observeEvent(rv$design_2(), {
        updateSelectInput(session, "design_sample_col_2", choices=colnames(rv$design_2()))
        if (length(colnames(rv$design_2())) > 1) start_cond <- colnames(rv$design_2())[2]
        else start_cond <- colnames(rv$design_2())[1]
        updateSelectInput(session, "design_cond_col_2", choices=colnames(rv$design_2()), selected = start_cond)
    })
    
    perform_mapping <- function(rv, output, data_file_1, data_file_2, feature_col_1, feature_col_2) {
        selcol1 <- NULL
        if (!is.null(data_file_1)) {
            selcol_list <- rv$selected_cols_obj()[[data_file_1$name]]
            if ("samples" %in% names(selcol_list)) {
                selcol1 <- selcol_list$samples
            }
        }
        
        selcol2 <- NULL
        if (!is.null(data_file_2)) {
            selcol2_list <- rv$selected_cols_obj()[[data_file_2$name]]
            if ("samples" %in% names(selcol2_list)) {
                selcol2 <- selcol2_list$samples
            }
        }
        
        rv <- do_dataset_mapping(
            rv, 
            feature_col_1, 
            feature_col_2, 
            output, 
            selcol1,
            selcol2,
            input$matched_samples
        )
        rv
    }
    
    observeEvent({
        input$perform_map_button
    }, {
        rv <- perform_mapping(rv, output, input$data_file_1, input$data_file_2, input$feature_col_1, input$feature_col_2)
    })
    
    observeEvent(rv$mapping_obj(), {
        req(!is.null(rv$mapping_obj()))
        number_files <- length(which(c(!is.null(input$data_file_1), !is.null(input$data_file_2))))
        
        if (number_files == 2) {
            message("Dual found")
            
            shinyjs::addClass("perform_map_button", "recolor_button_blue")
        }
        else if (number_files == 1) {
            message("Single found")
            shinyjs::addClass("perform_map_button", "recolor_button_gray")
        }
        else {
            message("Number identified files: %s", number_files)
        }
    })
    
    observeEvent({
        input$data_file_1
        input$data_file_2
        input$design_file_1
        input$design_file_2
    }, {
        shinyjs::removeClass("perform_map_button", "recolor_button")
        shinyjs::removeClass("perform_map_button", "recolor_button_red")
        shinyjs::addClass("perform_map_button", "recolor_button_yellow")
    })
    
    return(rv)
}

