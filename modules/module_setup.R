source("R/setup_server_utils.R")
source("R/setup_ui_utils.R")

setup_panel_ui <- function(id) {
    
    ns <- NS(id)
    tabPanel(
        id,
        fluidPage(
            id = "outer_area",
            tags$head(
                tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap")
            ),
            tags$style(
                type = "text/css",
                ".recolor_button { color: #fff; background-color: #337ab7; border-color: #2e6da4 }",
                ".recolor_button:hover { color: #fff; background-color: #2269a6; border-color: #2e6da4 }",
                ".well { background-color: #F3F3F3; border-color: #AAAAAA; border-width: 1px; box-shadow: 2px 2px grey; }"
            ),
            tags$style(
                type = "text/css",
                ".button_row { padding: 5px; }",
                "#column_select_noselectize { height: 500px; }"
            ),
            fluidRow(
                column(4,
                       selectInput(ns("select_dataset"), label = "Select dataset", choices = c("Dataset 1"=1,"Dataset 2"=2), selected = 1),
                       conditionalPanel(
                           sprintf("input['%s'] == 1", ns("select_dataset")),
                           h3("Dataset 1"),
                           sample_input_well(ns("data_file_1"), ns("data_selected_columns_1"), ns("feature_col_1")),
                           design_input_well(ns("design_file_1"), ns("design_sample_col_1"))
                       ),
                       conditionalPanel(
                           sprintf("input['%s'] == 2", ns("select_dataset")),
                           h3("Dataset 2"),
                           sample_input_well(ns("data_file_2"), ns("data_selected_columns_2"), ns("feature_col_2")),
                           design_input_well(ns("design_file_2"), ns("design_sample_col_2"))
                       )
                ),
                column(3,
                       align="center",
                       wellPanel(
                           select_button_row("Select samples", ns("sample_select_button_1"), ns("sample_deselect_button_1")),
                           select_button_row("Select stat groups", ns("stat_select_button_1"), ns("stat_deselect_button_1")),
                           # action_button_row(ns("autodetect_cols"), "Autodetect"),
                           fluidRow(
                               class = "button_row",
                               actionButton(
                                   ns("autodetect_cols"),
                                   class = "recolor_button",
                                   width = "80%",
                                   "Autodetect"
                               )
                           ),
                           fluidRow(
                               class = "button_row",
                               actionButton(
                                   ns("perform_map_button"),
                                   class = "recolor_button",
                                   width = "80%",
                                   "Map datasets"
                               )
                           )
                           # action_button_row(ns("perform_map_button"), "Map datasets")
                       ),
                       wellPanel(
                           p("If using two datasets, assign matching feature column. If wanting PCA measures, assign sample columns."),
                           textOutput(ns("perform_map_status"))
                       )
                ),
                column(5,
                       conditionalPanel(
                           sprintf("input['%s'] == 1", ns("select_dataset")),
                           h3("Dataset 1"),
                           wellPanel(
                               selectInput(
                                   ns("sample_selected_1"),
                                   "Sample columns",
                                   choices = c(""),
                                   multiple = TRUE,
                                   selectize = FALSE,
                                   size = 12
                               ),
                               selectInput(
                                   ns("statcols_selected_1"),
                                   "Stat cols",
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
                           h3("Dataset 2"),
                           wellPanel(
                               selectInput(
                                   ns("sample_selected_2"),
                                   "Sample columns",
                                   choices = c(""),
                                   multiple = TRUE,
                                   selectize = FALSE,
                                   size = 12
                               ),
                               selectInput(
                                   ns("statcols_selected_2"),
                                   "Stat cols",
                                   choices = c(""),
                                   multiple = TRUE,
                                   selectize = FALSE,
                                   size = 12
                               ),
                               textOutput(ns("found_stat_patterns_2"))
                           )
                       )
                )
            ),
            h3("Table views"),
            tabsetPanel(
                type = "tabs",
                tabPanel("Data 1", DT::DTOutput(ns("dt_data1"))),
                tabPanel("Design 1", DT::DTOutput(ns("dt_design1"))),
                tabPanel("Data 2", DT::DTOutput(ns("dt_data2"))),
                tabPanel("Design 2", DT::DTOutput(ns("dt_design2")))
            )
        )
    )
}

module_setup_server <- function(input, output, session) {

    output$dt_data1 = DT::renderDataTable({
        req(rv$filedata_1)
        rv$filedata_1()
    })
        
    output$dt_design1 = DT::renderDataTable({
        req(rv$design_1)
        rv$design_1()
    })
    
    output$dt_data2 = DT::renderDataTable({
        req(rv$filedata_2)
        rv$filedata_2()
    })
    
    output$dt_design2 = DT::renderDataTable({
        req(rv$design_2)
        rv$design_2()
    })
    
    load_data <- function(in_file) {
        infile <- in_file
        if (is.null(infile)) {
            return(NULL)
        }
        read_tsv(infile$datapath, col_types = cols())
    }
    
    get_filename <- function(in_file) {
        infile <- in_file
        if (is.null(infile)) {
            return(NULL)
        }
        stringi::stri_extract_first(str = infile$name, regex = ".*")
    }
    
    rv <- list()
    rv$filedata_1 <- reactive(load_data(input$data_file_1))
    rv$filedata_2 <- reactive(load_data(input$data_file_2))
    rv$design_1 <- reactive(load_data(input$design_file_1))
    rv$design_2 <- reactive(load_data(input$design_file_2))
    rv$design_samplecol_1 <- reactive(input$design_sample_col_1)
    rv$design_samplecol_2 <- reactive(input$design_sample_col_2)
    rv$selected_cols_obj <- reactiveVal(list())
    rv$filename_1 <- reactive(get_filename(input$data_file_1))
    rv$filename_2 <- reactive(get_filename(input$data_file_2))
    rv$mapping_obj <- reactiveVal(NULL)

    retrieve_data <- function(rv, input, ind, data_pat) {
        if (!is.null(di(rv, input, 1))) rv[[sprintf("%s_%s", data_pat, di(rv, input, ind))]]()
        else NULL
    }
    
    # retrieve_data_specific_input <- function(rv, input_vals, ind) {
    # 
    #     print(sprintf("Received fields: %s and %s", input_vals[1], input_vals[2]))
    #             
    #     NULL
    #     # if (!is.null())
    #     # 
    #     # if (!is.null(di(rv, input, 1))) rv[[sprintf("%s_%s", data_pat, di(rv, input, 1))]]()
    #     # else NULL
    # }
    
    rv$rdf_ref <- function(rv, input) retrieve_data(rv, input, 1, "filedata")
    rv$rdf_comp <- function(rv, input) retrieve_data(rv, input, 2, "filedata")
    # rv$rdf_comp_test <- function(rv, input_vector) retrieve_data_specific_input(rv, input_vector, 2)
    rv$ddf_ref <- function(rv, input) retrieve_data(rv, input, 1, "design")
    rv$ddf_comp <- function(rv, input) retrieve_data(rv, input, 2, "design")
    rv$rdf_cols_ref <- function(rv, input) colnames(retrieve_data(rv, input, 1, "filedata"))
    rv$rdf_cols_comp <- function(rv, input) colnames(retrieve_data(rv, input, 2, "filedata"))
    rv$ddf_cols_ref <- function(rv, input) colnames(retrieve_data(rv, input, 1, "design"))
    rv$ddf_cols_comp <- function(rv, input) colnames(retrieve_data(rv, input, 2, "design"))

    rv$samples_ref <- function(rv, input) {
        rv$selected_cols_obj()[[input[[sprintf("dataset%s", dataset_ind(rv, input, 1))]]]]$samples 
    }
    
    rv$samples_comp <- function(rv, input) {
        rv$selected_cols_obj()[[input[[sprintf("dataset%s", dataset_ind(rv, input, 2))]]]]$samples 
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
        
        samples_from_ddf <- ddf[[sample_col]]
        if (all(samples_from_ddf %in% colnames(rdf))) {
            message("All samples found!")
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
        if (input$design_sample_col_1 != "") {
            autodetect_sample_cols(
                rv$design_1(),
                rv$filedata_1(),
                data_nbr = 1, 
                rv, 
                rv$filename_1()
            )
        }
        if (input$design_sample_col_2 != "") {
            autodetect_sample_cols(
                rv$design_2(),
                rv$filedata_2(),
                data_nbr = 2,
                rv, 
                rv$filename_2()
            )
        }
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
    })
    
    observeEvent(rv$design_2(), {
        updateSelectInput(session, "design_sample_col_2", choices=colnames(rv$design_2()))
    })
    
    observeEvent({
        input$perform_map_button
    }, {
        print("Doing dataset mapping")

        selcol1 <- NULL
        if (length(rv$selected_cols_obj()) > 0) {
            selcol_list <- rv$selected_cols_obj()[[1]]
            if ("samples" %in% names(selcol_list)) {
                selcol1 <- selcol_list$samples
            }
        }

        selcol2 <- NULL
        if (length(rv$selected_cols_obj()) > 1) {
            selcol2_list <- rv$selected_cols_obj()[[2]]
            if ("samples" %in% names(selcol2_list)) {
                selcol2 <- selcol2_list$samples
            }
        }
        
        rv <- do_dataset_mapping(
            rv, 
            input$feature_col_1, 
            input$feature_col_2, 
            output, 
            selcol1,
            selcol2
        )
    })
        
    observeEvent(rv$mapping_obj, {
        message("Mapping object obtained!")
    })
    
    return(rv)
}

