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
                             column(6, 
                                    wellPanel(
                                        fluidRow(
                                            column(5, 
                                                   align="center",
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
                                                   )
                                                   
                                            ),
                                            column(6, 
                                                   checkboxInput(ns("two_datasets"), label = "Two datasets", value = FALSE),
                                                   checkboxInput(ns("matched_samples"), label = "Matched samples", value = FALSE),
                                                   checkboxInput(ns("automatic_sample_detect"), label = "Detect sample col.", value = TRUE)
                                            )
                                        )
                                    )
                             ),
                             column(4, 
                                    p(HTML("<b>Status:</b>")),
                                    textOutput(ns("column_status")),
                                    textOutput(ns("load_status"))
                             ),
                             column(2)
                         ),
                         fluidRow(
                             column(4,
                                    bar_w_help("Dataset", ns("dataset_help")),
                                    sample_input_well(ns("data_file_1"), ns("data_selected_columns_1"), ns("feature_col_1"), ns("annot_col_1"), select_size=5)
                             ),
                             column(4,
                                    bar_w_help("Design", ns("design_help")),
                                    design_input_well(ns("design_file_1"), ns("design_sample_col_1"), ns("design_cond_col_1"))
                             ),
                             column(4,
                                    bar_w_help("Assigned columns", ns("assign_cols_help")),
                                    wellPanel(
                                        selectInput(
                                            ns("sample_selected_1"),
                                            "Assigned sample columns (dataset 1)",
                                            choices = c(""),
                                            multiple = TRUE,
                                            selectize = FALSE,
                                            size = 5
                                        ),
                                        selectInput(
                                            ns("statcols_selected_1"),
                                            "Assigned statistics columns (dataset 1)",
                                            choices = c(""),
                                            multiple = TRUE,
                                            selectize = FALSE,
                                            size = 5
                                        ),
                                        textOutput(ns("found_stat_patterns_1"))
                                    )
                             )
                         ),
                         fluidRow(
                             column(4,
                                    conditionalPanel(
                                        sprintf("input['%s'] == 1", ns("two_datasets")),
                                        sample_input_well(ns("data_file_2"), ns("data_selected_columns_2"), ns("feature_col_2"), ns("annot_col_2"), select_size=5)
                                    )
                             ),
                             column(4,
                                    conditionalPanel(
                                        sprintf("input['%s'] == 1 && input['%s'] == 0", ns("two_datasets"), ns("matched_samples")),
                                        design_input_well(ns("design_file_2"), ns("design_sample_col_2"), ns("design_cond_col_2"))
                                    )
                             ),
                             column(4,
                                    conditionalPanel(
                                        sprintf("input['%s'] == 1", ns("two_datasets")),
                                        wellPanel(
                                            selectInput(
                                                ns("sample_selected_2"),
                                                "Assigned sample columns (dataset 2)",
                                                choices = c(""),
                                                multiple = TRUE,
                                                selectize = FALSE,
                                                size = 5
                                            ),
                                            selectInput(
                                                ns("statcols_selected_2"),
                                                "Assigned statistics columns (dataset 2)",
                                                choices = c(""),
                                                multiple = TRUE,
                                                selectize = FALSE,
                                                size = 5
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
                             tabPanel("Mapped data", DT::DTOutput(ns("table_display"))),
                             tabPanel("Raw data 1", DT::DTOutput(ns("raw_data1"))),
                             tabPanel("Raw data 2", DT::DTOutput(ns("raw_data2"))),
                             tabPanel("Design 1", DT::DTOutput(ns("dt_design1"))),
                             tabPanel("Design 2", DT::DTOutput(ns("dt_design2")))
                         )
                )
            )))
}

# How to access navbar element (from outside module)
# document.querySelectorAll("#navbar li a[data-value=Correlation]")

module_setup_server <- function(input, output, session, module_name) {
    
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

    observeEvent(input$dataset_help, {
        shinyalert(
            title = "Help: Setup dataset",
            text = "How the data matrix should look",
            html = TRUE
        )
    })

    observeEvent(input$design_help, {
        shinyalert(
            title = "Help: Setup design",
            text = "How the design matrix should look",
            html = TRUE
        )
    })
    
    observeEvent(input$assign_cols_help, {
        shinyalert(
            title = "Help: Assign columns",
            text = "How to do the column assignment",
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
        rv$dt_parsed_data(rv, rv$filedata_1(), with_row_selection=FALSE)
    })
    
    output$raw_data2 <- DT::renderDataTable({
        req(rv$filedata_2())
        rv$dt_parsed_data(rv, rv$filedata_2(), with_row_selection=FALSE)
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
    
    autodetect_sample_cols <- function(ddf, rdf, data_nbr, rv, filename, sample_col=NULL) {
        
        if (is.null(sample_col)) {
            full_match <- lapply(ddf, function(col) { as.character(col) %in% colnames(rdf) %>% all() } ) %>% unlist()
            sample_col <- colnames(ddf)[which(full_match)[1]]
            updateSelectInput(
                session, 
                sprintf("design_sample_col_%s", data_nbr), 
                choices=colnames(rv[[sprintf("design_%s", data_nbr)]]()), 
                selected = sample_col
            )
        }
        
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
    
    
    detect_sample_cols <- function(rv, design, design_sample_col, filedata, filename, data_nbr, autodetect=TRUE) {
        
        if (!is.null(design_sample_col) && design_sample_col != "" && !is.null(filedata)) {
            
            if (autodetect) {
                sample_col <- NULL
            }
            else {
                sample_col <- design_sample_col
            }
            
            autodetect_sample_cols(
                design,
                filedata,
                data_nbr = data_nbr, 
                rv, 
                filename,
                sample_col = sample_col
            )
        }
    }
    
    observeEvent(input$autodetect_cols, {
        
        autodetect_stat_cols()
        detect_sample_cols(
            rv,
            rv$design_1(),
            input$design_sample_col_1,
            filedata=rv$filedata_1(),
            filename=rv$filename_1(),
            data_nbr=1,
            autodetect=input$automatic_sample_detect
        )
        detect_sample_cols(
            rv,
            rv$design_2(),
            input$design_sample_col_2,
            filedata=rv$filedata_2(),
            filename=rv$filename_2(),
            data_nbr=2,
            autodetect=input$automatic_sample_detect
        )
        
    })
    
    # Clear/reset fildata 1 related fields
    observeEvent(rv$filedata_1(), {
        clear_fields(session, rv$filedata_1, c("sample_selected_1", "statcols_selected_1"))
        clear_file_fields(session, rv$filedata_1, c("data_selected_columns_1", "feature_col_1", "annot_col_1"))
        rv$selected_cols_obj(
            c(rv$selected_cols_obj(), setNames(list(list()), rv$filename_1()))
        )
    })
    
    # Clear/reset filedata 2 related fields
    observeEvent(rv$filedata_2(), {
        clear_fields(session, rv$filedata_2, c("sample_selected_2", "statcols_selected_2"))
        clear_file_fields(session, rv$filedata_2, c("data_selected_columns_2", "feature_col_2", "annot_col_2"))
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
    
    observeEvent(input$perform_map_button, {
        
        if (is.null(input$data_file_1) && is.null(input$design_file_1)) {
            output$load_status <- renderText({ "Neither data file or design file detected, please upload and assign columns before loading data" })
        }
        else if (!is.null(input$data_file_1) && is.null(input$design_file_1)) {
            output$load_status <- renderText({ "No design file detected, please upload and assign columns before loading data" })
        }
        else if (is.null(input$data_file_1) && !is.null(input$design_file_1)) {
            output$load_status <- renderText({ "No data file detected, please upload and assign columns before loading data" })
        }
        else if (length(rv$selected_cols_obj()[[input$data_file_1$name]]) == 0) {
            output$load_status <- renderText({ "Data present but no columns assigned, please identify columns before loading" })
        }
        else if (input$matched_samples && 
                 (is.null(rv$selected_cols_obj()[[input$data_file_1$name]]$samples) || 
                  is.null(rv$selected_cols_obj()[[input$data_file_2$name]]$samples))) {
            output$load_status <- renderText({ "Matched samples requires identified assigned sample columns for both datasets, now at least one is missing" })
        }
        else {
            rv <- perform_mapping(rv, output, input$data_file_1, input$data_file_2, input$feature_col_1, input$feature_col_2)
        }
    })
    
    observeEvent(rv$mapping_obj(), {
        req(!is.null(rv$mapping_obj()))
        number_files <- length(which(c(!is.null(input$data_file_1), !is.null(input$data_file_2))))
        
        if (number_files == 2) {
            message("Dual found")
            # output$load_status <- renderText({ "Two datasets detected as assigned" })
        }
        else if (number_files == 1) {
            message("Single found")
            # output$load_status <- renderText({ "One dataset detected as assigned" })
        }
        else {
            message("Number identified files: %s", number_files)
        }
    })
    
    return(rv)
}

