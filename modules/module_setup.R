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
                         bar_w_help("Load data", ns("help")),
                         fluidRow(
                             column(6,
                                    wellPanel(
                                        fluidRow(
                                            column(5,
                                                   align="center",
                                                   fluidRow(
                                                       class = "button_row",
                                                       actionButton(
                                                           ns("identify_columns"),
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
                                                   fluidRow(
                                                       column(6, checkboxInput(ns("two_datasets"), label = "Two datasets", value = FALSE)),
                                                       conditionalPanel(
                                                           sprintf("input['%s'] == 1", ns("two_datasets")),
                                                           column(6, checkboxInput(ns("two_datasets_random_discard"), label = "Discard dups.", value = FALSE))
                                                       )
                                                   ),
                                                   checkboxInput(ns("matched_samples"), label = "Matched samples", value = FALSE),
                                                   checkboxInput(ns("automatic_sample_detect"), label = "Detect sample col.", value = TRUE)
                                            )
                                        )
                                    )
                             ),
                             column(6,
                                    p(HTML("<b>Status:</b>")),
                                    textOutput(ns("column_status")),
                                    br(),
                                    textOutput(ns("load_status"))
                             )
                         ),
                         fluidRow(
                             column(4,
                                    bar_w_help("Dataset", ns("dataset_help")),
                                    sample_input_well(
                                        ns("data_file_1"), 
                                        ns("data_selected_columns_1"), 
                                        ns("feature_col_1"), 
                                        ns("annot_col_1"), 
                                        ns("parse_err_data_1"), select_size=5)
                             ),
                             column(4,
                                    bar_w_help("Design", ns("design_help")),
                                    design_input_well(
                                        ns("design_file_1"), 
                                        ns("design_sample_col_1"), 
                                        ns("design_cond_col_1"),
                                        ns("parse_err_design_1")
                                    )
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
                                        sample_input_well(
                                            ns("data_file_2"), 
                                            ns("data_selected_columns_2"), 
                                            ns("feature_col_2"), 
                                            ns("annot_col_2"), 
                                            ns("parse_err_data_2"), 
                                            select_size=5)
                                    )
                             ),
                             column(4,
                                    conditionalPanel(
                                        sprintf("input['%s'] == 1 && input['%s'] == 0", ns("two_datasets"), ns("matched_samples")),
                                        design_input_well(
                                            ns("design_file_2"), 
                                            ns("design_sample_col_2"), 
                                            ns("design_cond_col_2"),
                                            ns("parse_err_design_2")
                                        )
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
                         bar_w_help("Table Setup", ns("help_table_setup")),
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
                             ),
                             fluidRow(
                                 column(3, textInput(ns("statpat_pval"), label="Stat pattern, P-value", value="P.Value")),
                                 column(3, textInput(ns("statpat_fdr"), label="Stat pattern, FDR", value="adj.P.Val")),
                                 column(3, textInput(ns("statpat_fold"), label="Stat pattern, log fold", value="logFC")),
                                 column(3, textInput(ns("statpat_expr"), label="Stat pattern, expression", value="AvgExpr"))
                             )
                         ),
                         tabsetPanel(
                             id=ns("data_table_tabs"),
                             type = "tabs",
                             tabPanel("MappedData"),
                             tabPanel("RawData1"),
                             tabPanel("RawData2"),
                             tabPanel("Design1"),
                             tabPanel("Design2")
                         ),
                         downloadButton(ns("download_table"), "Download table"),
                         DT::DTOutput(ns("table_output"))
                ),
                tabPanel("InputHelp",
                         h3("Input help"),
                         htmlOutput(ns("input_help_text_design")),
                         plotOutput(ns("input_help_image_design"), height = 200),
                         htmlOutput(ns("input_help_text_data")),
                         plotOutput(ns("input_help_image_data"), height = 600)
                )
            )
        )
    )
}

# How to access navbar element (from outside module)
# document.querySelectorAll("#navbar li a[data-value=Correlation]")
# Issue: download all data in displayed DT table buttom
# https://github.com/rstudio/DT/issues/267

module_setup_server <- function(input, output, session, module_name) {
    
    output$download_table <- downloadHandler(
        filename = function() {
            paste(input$data_table_tabs, "-", Sys.Date(), ".tsv", sep="")
        },
        content = function(file) {
            write_tsv(get_target_data(input$data_table_tabs, get_raw=TRUE), file)
        }
    )
    
    output$parse_err_data_1 <- renderUI({
        req(rv$filedata_1(), readr::problems(rv$filedata_1()) %>% nrow() > 1)
        downloadButton(sprintf("%s-%s", module_name, "parse_err_data_1_handler"), "Check parsing issues")
    })
    
    output$parse_err_data_2 <- renderUI({
        req(rv$filedata_2(), readr::problems(rv$filedata_2()) %>% nrow() > 1)
        downloadButton(sprintf("%s-%s", module_name, "parse_err_data_2_handler"), "Check parsing issues")
    })
    
    output$parse_err_design_1 <- renderUI({
        req(rv$design_1(), readr::problems(rv$design_1()) %>% nrow() > 1)
        downloadButton(sprintf("%s-%s", module_name, "parse_err_design_1_handler"), "Check parsing issues")
    })
    
    output$parse_err_design_2 <- renderUI({
        req(rv$design_2(), readr::problems(rv$design_2()) %>% nrow() > 1)
        downloadButton(sprintf("%s-%s", module_name, "parse_err_design_2_handler"), "Check parsing issues")
    })
    
    output$input_help_text_design <- renderUI({
        "The design matrix specify sample-specific information, and is used to color different sample groupings.
        One column (here 'sample') should contain all involved sample IDs. These IDs should all be present in
        the data matrix header line. Beyond this, any number of additional columns with sample conditions
        could be included. In this case, samples are divided into two groups, with three samples belonging to
        group 'A' and three to group 'B'. This matrix should be saved in tab separated format (Save as -> .csv
        and choose 'tab' as delimitor)."
    })
    
    output$input_help_image_design <- renderImage({
        filename <- normalizePath(file.path("./doc", "design_help.png"))
        list(src = filename)
    }, deleteFile = FALSE)
    
    output$input_help_text_data <- renderUI({
        "The data matrix contains feature information, and measured values across different samples.
        It can contain any number of annotation columns (here: first four), information from statistical 
        comparisons and the sample values.
        
        Statistical comparisons should include the information 'average expression', 'log fold change', 'p-value'
        and 'adjusted p-value'. For each, the same prefix should be used (here 'comp'). By default, 'logFC', 'AveExpr', 'P.Value'
        and 'adj.P.Val' suffixes are expected, but this could be adjusted under the 'TableSetup' page. The contrast
        prefix and the suffixes should each be separated by a dot (.).
        
        Beyond this are the sample columns. These should match the ones specified in the design matrix (if not matching,
        OmicLoupe will not understand that the non-missing columns are sample columns, and will be handled as normal
        annotation columns).
        
        Numbers should be separated by dot, not comma. Missing values denoted as 'NA'. Similarly to the design matrix,
        the data matrix should be saved in tab-separated format."
    })
    
    output$input_help_image_data <- renderImage({
        filename <- normalizePath(file.path("./doc", "data_help.png"))
        list(src = filename)
    }, deleteFile = FALSE)
    
    err_log_download_handler <- function(rv, rv_tag) {
        downloadHandler(
            filename = function() {sprintf("parsing_errors_%s.txt", rv_tag)},
            content = function(file) {write_tsv(readr::problems(rv[[rv_tag]]()), path = file)}
        )
    }

    output$parse_err_data_1_handler <- err_log_download_handler(rv, "filedata_1")
    output$parse_err_data_2_handler <- err_log_download_handler(rv, "filedata_2")
    output$parse_err_design_1_handler <- err_log_download_handler(rv, "design_1")
    output$parse_err_design_2_handler <- err_log_download_handler(rv, "design_2")
    
    observeEvent(rv$mapping_obj(), {
        # req(rv$mapping_obj()$get_combined_dataset())
        validate(need(!is.null(rv$mapping_obj()$get_combined_dataset()), "No combined dataset found, something wrong with the mapping?"))
        
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
    
    get_target_data <- function(target, get_raw=FALSE) {
        switch(
            target,
            "MappedData"={
                # req(rv$mapping_obj())
                # req(rv$mapping_obj()$get_combined_dataset())
                validate(need(!is.null(rv$mapping_obj()), "No mapped data found in get_target_data, something wrong with the mapping?"))
                validate(need(!is.null(rv$mapping_obj()$get_combined_dataset()), "No combined dataset found in get_target_data, something wrong with the mapping?"))
                if (get_raw) {
                    rv$dt_parsed_data_raw(rv, rv$mapping_obj()$get_combined_dataset())
                }
                else {
                    rv$dt_parsed_data(rv, rv$mapping_obj()$get_combined_dataset())
                }
            },
            "Design1"={
                # req(rv$design_1)
                validate(need(!is.null(rv$design_1), "No design 1 found in get_target_data, something wrong with the design matrix?"))
                rv$design_1()
            },
            "Design2"={
                validate(need(!is.null(rv$design_2), "No design 2 found in get_target_data, something wrong with the design matrix?"))
                # req(rv$design_2)
                rv$design_2()
            },
            "RawData1"={
                # req(rv$filedata_1())
                validate(need(!is.null(rv$filedata_1()), "No filedata 1 found in get_target_data, something wrong with loading the file?"))
                rv$dt_parsed_data(
                    rv, 
                    rv$filedata_1(), 
                    with_row_selection=FALSE
                )
            },
            "RawData2"={
                # req(rv$filedata_2())
                validate(need(!is.null(rv$filedata_2()), "No filedata 2 found in get_target_data, something wrong with loading the file?"))
                rv$dt_parsed_data(
                    rv, 
                    rv$filedata_2(), 
                    with_row_selection=FALSE
                )
            }
        )
    }
    
    output$table_output <- DT::renderDataTable({
        get_target_data(input$data_table_tabs)
    })
    
    rv <- setup_reactive_values_obj(input)
    
    # stat_patterns <- list(
    #     P.Value = "P.Value",
    #     adj.P.Val = "adj.P.Val",
    #     logFC = "logFC",
    #     avgExpr = "avgExpr"
    # )
    
    statcols <- function(rv, data_field, contrast_field, stat_patterns, prefix_index=NULL) {
        # statcols <- function(rv, data_field, contrast_field, prefix_index=NULL) {
        dataset_stat_cols <- rv$selected_cols_obj()[[data_field]]$statcols
        parsed_cols <- parse_stat_cols(dataset_stat_cols, contrast_field, stat_patterns)
        if (!is.null(prefix_index)) {
            parsed_cols <- lapply(parsed_cols, function(elem) { sprintf("d%s.%s", prefix_index, elem) })
        }
        parsed_cols
    }
    
    rv$stat_patterns <- reactive({
        list(
            P.Value=c("P.Value", "PValue", input$statpat_pval) %>% unique(),
            adj.P.Val=c("adj.P.Val", "AdjPVal", input$statpat_fdr) %>% unique(),
            logFC=c("logFC", "log2FoldChange", input$statpat_fold) %>% unique(),
            AveExpr=c("AveExpr", "featureAvg", input$statpat_expr) %>% unique()
        )
    })
    
    rv$stat_patterns_parsed <- reactive({
        map(rv$stat_patterns(), ~paste0(paste(., collapse="$|"), "$"))
    })

    rv$statcols_ref <- function(rv, data_field, contrast_field) {
        data_ind <- di_new(rv, data_field, 1)
        stat_patterns <- rv$stat_patterns()
        statcols(rv, data_field, contrast_field, stat_patterns, prefix_index=data_ind)
    }
    
    rv$statcols_comp <- function(rv, data_field, contrast_field) {
        data_ind <- di_new(rv, data_field, 2)
        stat_patterns <- rv$stat_patterns()
        statcols(rv, data_field, contrast_field, stat_patterns, prefix_index=data_ind)
    }
    
    # stat_pattern - A stat suffix used to assess what contrasts are present
    update_selcol_obj <- function(rv, dataset, colname, new_value, stat_pattern, sync_stat_patterns=FALSE) {
        
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
        
        autoselect_statpatterns <- function(parsed_stat_patterns, dataset_cols) {
            combined_stat_pattern <- parsed_stat_patterns %>% unlist() %>% paste(collapse="|")
            dataset_cols[grepl(combined_stat_pattern, dataset_cols)]
        }
        
        selected_statcols <- autoselect_statpatterns(rv$stat_patterns_parsed(), colnames(rv$filedata_1()))
        if (!is.null(rv$filename_1())) {
            rv <- update_selcol_obj(rv, rv$filename_1(), "statcols", selected_statcols, sync_stat_patterns = TRUE, stat_pattern = rv$stat_patterns_parsed()$P.Value)
            sync_select_inputs(session, "data_selected_columns_1", "statcols_selected_1", rv$filedata_1, selected_statcols)
            update_statpatterns_display(rv$selected_cols_obj()[[rv$filename_1()]]$statpatterns, "found_stat_patterns_1")
        }
        
        selected_statcols_2 <- autoselect_statpatterns(rv$stat_patterns_parsed(), colnames(rv$filedata_2()))
        if (!is.null(rv$filename_2())) {
            rv <- update_selcol_obj(rv, rv$filename_2(), "statcols", selected_statcols_2, sync_stat_patterns = TRUE, stat_pattern = rv$stat_patterns_parsed()$P.Value)
            sync_select_inputs(session, "data_selected_columns_2", "statcols_selected_2", rv$filedata_2, selected_statcols_2)
            update_statpatterns_display(rv$selected_cols_obj()[[rv$filename_2()]]$statpatterns, "found_stat_patterns_2")
        }
    }
    
    detect_sample_column <- function(ddf, rdf) {
        full_match <- lapply(ddf, function(col) { as.character(col) %in% colnames(rdf) %>% all() } ) %>% unlist()
        full_matches <- colnames(ddf)[which(full_match)]
        if (length(full_matches) > 1) {
            warning(sprintf("More than one column fully matches (%s), picking the first one", paste(full_matches, collapse=", ")))
            full_matches[1]
        }
        else {
            full_matches
        }
    }
    
    assign_sample_cols <- function(rv, data_nbr, ddf, rdf, sample_col, filename) {
        
        samples_from_ddf <- ddf[[sample_col]]
        if (all(samples_from_ddf %in% colnames(rdf))) {
            sync_select_inputs(
                session,
                sprintf("data_selected_columns_%s", data_nbr),
                sprintf("sample_selected_%s", data_nbr),
                rv[[sprintf("filedata_%s", data_nbr)]],
                samples_from_ddf
            )
            rv <- update_selcol_obj(rv, filename, "samples", samples_from_ddf, stat_pattern = rv$stat_patterns()$P.Value)
            status_message <- sprintf("%s sample columns identified for dataset %s.", length(samples_from_ddf), data_nbr)
            status_val <- 0
        }
        else {
            if (length(which(samples_from_ddf %in% colnames(rdf))) == 0) {
                # status_message <- "No samples from design matched to data, something is wrong!"
                shinyalert(
                    "Input error", 
                    "No samples from design matched to data. 
                
                    Please carefully inspect your inputs. You can use the 'TableSetup' tab to inspect
                    what is currently loaded into OmicLoupe and 'InputHelp' for further instructions
                    on input format.
                    
                    If neither helps, please send a message to the developer.", 
                    type="error")
                status_message <- ""
                status_val <- 1
            }
            else {
                missing <- colnames(rdf)[!samples_from_ddf %in% colnames(rdf)]
                shinyalert(
                    "Input error", 
                    sprintf("%s
                
                    Please carefully inspect your inputs. You can use the 'TableSetup' tab to inspect
                    what is currently loaded into OmicLoupe and 'InputHelp' for further instructions
                    on input format.
                    
                    If neither helps, please send a message to the developer.", paste0("Not all samples matched, non-missing: ", paste(missing, collapse=", "))), 
                    type="error")
                
                status_message <- paste0("Not all samples matched, non-missing: ", paste(missing, collapse=", "))
                status_val <- 1
            }
        }
        list(message=status_message, status=status_val)
    }
    
    observeEvent(input$identify_columns, {
        
        output$column_status <- renderText("A dataset and a design matrix need to be assigned before being able to detect sample columns")
        
        # req(!is.null(input$design_sample_col_1), input$design_sample_col_1 != "", !is.null(rv$filedata_1()))
        validate(need(!is.null(input$design_sample_col_1), "Autodetect columns didn't find any sample column in the design matrix"))
        validate(need(input$design_sample_col_1 != "", "Autodetect columns didn't find any non-empty sample column in the design matrix"))
        validate(need(!is.null(rv$filedata_1()), "Autodetect columns didn't find any filedata 1"))
        
        if (input$automatic_sample_detect) {
            sample_col_1 <- detect_sample_column(rv$design_1(), rv$filedata_1())
            updateSelectInput(session, "design_sample_col_1", selected = sample_col_1)
        }
        else {
            sample_col_1 <- input$design_sample_col_1
        }
        
        if (length(sample_col_1) == 0) {
            shinyalert(
                "Input error", 
                "No column in design matrix matches column names in the data matrix. 
                
                Please carefully inspect your inputs. You can use the 'TableSetup' tab to inspect
                what is currently loaded into OmicLoupe and 'InputHelp' for further instructions
                on input format.
                
                If neither helps, please send a message to the developer.", 
                type="error")
            return()
        }
        
        
        
        autodetect_stat_cols()
        status_data1 <- assign_sample_cols(
            rv,
            data_nbr=1,
            rv$design_1(),
            rv$filedata_1(),
            sample_col_1,
            rv$filename_1()
        )
        
        status_data2 <- list(message=NULL, status=0)
        if (!is.null(rv$design_2()) && !is.null(rv$filedata_2())) {
            
            if (input$automatic_sample_detect) {
                sample_col_2 <- detect_sample_column(rv$design_2(), rv$filedata_2())
                updateSelectInput(session, "design_sample_col_2", selected = sample_col_2)
            }
            else {
                sample_col_2 <- input$design_sample_col_2
            }
            
            status_data2 <- assign_sample_cols(
                rv,
                data_nbr=2,
                rv$design_2(),
                rv$filedata_2(),
                sample_col_2,
                rv$filename_2()
            )
        }
        
        info_text <- paste(c(status_data1$message, status_data2$message), sep="\n")
        if (status_data1$status == 0 && status_data2$status == 0) {
            info_text <- sprintf("%s\n%s", info_text, "Proceed to load the data using 'Load data'.")
        }
        output$column_status <- renderText(info_text)
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
        if (is.null(selcol1)) {
            output$load_status <- renderText("No columns loaded for dataset 1 which is required for analysis, you can inspect your matrices under 'TableSetup'")
        }
        
        req(selcol1)
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
            input$matched_samples,
            duplicates_method=ifelse(input$two_datasets_random_discard, "discard", "stop")
        )
        
        rv
    }
    
    observeEvent(input$perform_map_button, {
        
        if (is.null(input$data_file_1) && is.null(input$design_file_1)) {
            shinyalert("Input error", "Neither data file or design file detected, please upload and assign columns before loading data", type = "error")
            return()
        }
        
        if (!is.null(input$data_file_1) && is.null(input$design_file_1)) {
            shinyalert(
                "Input error", 
                "No design file detected, please upload in the 'Choose design file' field and assign columns using 'Identify columns' before loading data
                
                For further help, please check the 'InputHelp' tab. If still stuck, please send a message to the developer.", 
                type = "error")
            return()
        }
        
        if (is.null(input$data_file_1) && !is.null(input$design_file_1)) {
            shinyalert(
                "Input error", 
                "No data file detected, please upload in the 'Choose data file' field and assign columns using 'Identify columns' before loading data
                
                For further help, please check the 'InputHelp' tab. If still stuck, please send a message to the developer.", 
                type = "error")
            return()
        }
        
        if (length(rv$selected_cols_obj()[[input$data_file_1$name]]) == 0) {
            shinyalert(
                "Input error", 
                "Data present but no sample columns assigned, please identify columns using 'Identify columns' before loading
                
                For further help, please check the 'InputHelp' tab. If still stuck, please send a message to the developer.", 
                type = "error")
            return()
        }
        
        if (input$matched_samples &&
                 (is.null(rv$selected_cols_obj()[[input$data_file_1$name]]$samples) ||
                  is.null(rv$selected_cols_obj()[[input$data_file_2$name]]$samples))) {
            
            shinyalert(
                "Input error", 
                "Matched samples requires identified assigned sample columns for both datasets, now at least one is missing. Either run a single sample, or make sure columns for the second dataset are properly assigned.
                
                For further help, please check the 'InputHelp' tab. If still stuck, please send a message to the developer.", 
                type = "error")
            return()
        }
        
        rv <- perform_mapping(rv, output, input$data_file_1, input$data_file_2, input$feature_col_1, input$feature_col_2)
    })
    
    observeEvent(rv$mapping_obj(), {
        # req(!is.null(rv$mapping_obj()))
        
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
            text = "The data file should contain columns with measurements for each samples with a column name specifying the sample name. These names should match to a sample
            column found in the design file. This file can contain additional columns with feature information. Further information about the format can be found at
            http://quantitativeproteomics.org/normalyzerde/help",
            html = TRUE
        )
    })
    
    observeEvent(input$design_help, {
        shinyalert(
            title = "Help: Setup design",
            text = "The design file should contain one column with all sample names (also present among the data file columns). Further, it could contain columns with 
            conditions which can be used for coloring groups. More information can be found at: http://quantitativeproteomics.org/normalyzerde/help",
            html = TRUE
        )
    })
    
    observeEvent(input$assign_cols_help, {
        shinyalert(
            title = "Help: Assign columns",
            text = "Looking for suffixes P.Value/PValue, adj.P.Val/adjPVal, logFC/log2FoldChange, AvgExpr/featureAvg or what is specified under the 'TableSetup' tab.
            The contrasts are expected to follow the pattern contrastname.adj.P.Val, contrastname.P.Value, contrastname.logFC, AvgExpr where AvgExpr specify the average of the
            feature across all conditions",
            html = TRUE
        )
    })
    
    return(rv)
}

