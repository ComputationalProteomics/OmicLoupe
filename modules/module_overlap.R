setup_overlap_ui <- function(id) {
    ns <- NS(id)
    tabPanel(
        id,
        fluidPage(
            fluidRow(
                column(8,
                       h4("Some ideas for development"),
                       htmlOutput(ns("html"))
                )
            ),
            p("Left and right: Venns or upsets"),
            fluidRow(
                column(
                    12,
                    wellPanel(
                        fluidRow(
                            column(6,
                                   selectInput(ns("dataset1"), "Reference dataset", choices = c("Dev"), selected = "Dev"),
                                   selectInput(ns("dataset2"), "Comp. dataset", choices = c("Dev"), selected = "Dev"),
                                   sliderInput(ns("threshold"), "Threshold", min=0, max=1, step=0.01, value=0.05),
                                   selectInput(ns("select_target"), "Select target", choices=c("A", "B", "A&B", "A|B"), selected = "A&B")
                            ),
                            column(6,
                                   selectInput(ns("ref_contrast"), "Ref. contr.", choices = c("Dev"), selected = "Dev"),
                                   selectInput(ns("comp_contrast"), "Comp. contr.", choices = c("Dev"), selected = "Dev"),
                                   selectInput(ns("contrast_type"), "Contrast type", choices=c("P.Value", "adj.P.Val"))
                            )
                        )
                    ),
                    htmlOutput(ns("warnings")),
                    tabsetPanel(
                        id = ns("plot_tabs"),
                        type = "tabs",
                        tabPanel("Venn",
                                 plotOutput(ns("venn")),
                                 DT::DTOutput(ns("table_display"))
                                 # plotOutput(ns("venn_comp"))
                        ),
                        tabPanel("Upset",
                                plotOutput(ns("upset_ref")),
                                plotOutput(ns("upset_comp"))
                        )
                    )
                )
            )
        )
    )
}

parse_vector_to_bullets <- function(vect, number=TRUE) {
    html_string <- paste0(
        "<li>",
        paste(vect, collapse="</li><li>"),
        "</li>"
    )
    
    if (!number) {
        list_style <- "ul"
    }
    else {
        list_style <- "ol"
    }
    
    sprintf("<%s>%s</%s>", list_style, html_string, list_style)
}

module_overlap_server <- function(input, output, session, rv) {
    
    selected_id_reactive <- reactive({
        output_table_reactive()[input$table_display_rows_selected, ]$comb_id %>% as.character()
    })
    
    observeEvent(input$table_display_rows_selected, {
        
        message("Observed!")
        rv$selected_feature(selected_id_reactive())
        message("Now the value is: ", rv$selected_feature())
    })
    
    ref_pass_reactive <- reactive({
        
        combined_dataset <- rv$mapping_obj()$get_combined_dataset(full_entries=FALSE)
        ref_sig_field <- rv$statcols_ref(rv, input$dataset1, input$ref_contrast)[[input$contrast_type]]
        ref_fold_field <- rv$statcols_ref(rv, input$dataset1, input$ref_contrast)$logFC

        ref_pass_tbl <- combined_dataset %>% 
            filter(UQ(as.name(ref_sig_field)) < input$threshold) %>% 
            dplyr::select(c("comb_id", ref_fold_field)) %>%
            rename(fold=ref_fold_field) %>%
            mutate(comb_id=as.character(comb_id))
        ref_pass_list <- setNames(as.list(ref_pass_tbl$fold), ref_pass_tbl$comb_id)
        ref_pass_list
    })
    
    comp_pass_reactive <- reactive({
        
        combined_dataset <- rv$mapping_obj()$get_combined_dataset(full_entries=FALSE)
        comp_sig_field <- rv$statcols_comp(rv, input$dataset2, input$comp_contrast)[[input$contrast_type]]
        comp_fold_field <- rv$statcols_comp(rv, input$dataset2, input$comp_contrast)$logFC
        
        comp_pass_tbl <- combined_dataset %>% 
            filter(UQ(as.name(comp_sig_field)) < input$threshold) %>% 
            dplyr::select(c("comb_id", comp_fold_field)) %>%
            rename(fold=comp_fold_field) %>%
            mutate(comb_id=as.character(comb_id))
        comp_pass_list <- setNames(as.list(comp_pass_tbl$fold), comp_pass_tbl$comb_id)
        comp_pass_list
    })
    
    output_table_reactive <- reactive({
        ref_pass <- names(ref_pass_reactive())
        comp_pass <- names(comp_pass_reactive())
        
        if (input$select_target == "A&B") {
            target_ids <- union(ref_pass, comp_pass)
        }
        else if (input$select_target == "A|B") {
            target_ids <- intersect(ref_pass, comp_pass)
        }
        else if (input$select_target == "A") {
            target_ids <- setdiff(ref_pass, comp_pass)
        }
        else if (input$select_target == "B") {
            target_ids <- setdiff(comp_pass, ref_pass)
        }
        else {
            stop(sprintf("Unknown input$select_target: %s", input$select_target))
        }
        
        rv$mapping_obj()$get_combined_dataset() %>%
            filter(comb_id %in% target_ids)
    })
    
    output$table_display <- DT::renderDataTable({

        output_table_reactive() %>%
            DT::datatable(
                data=.,
                selection=list(mode='single', selected=c(1)),
                options=list(pageLength=10)
            )
        
        # ref_pass <- names(ref_pass_reactive())
        # comp_pass <- names(comp_pass_reactive())
        # 
        # if (input$select_target == "A&B") {
        #     target_ids <- union(ref_pass, comp_pass)
        # }
        # else if (input$select_target == "A|B") {
        #     target_ids <- intersect(ref_pass, comp_pass)
        # }
        # else if (input$select_target == "A") {
        #     target_ids <- setdiff(ref_pass, comp_pass)
        # }
        # else if (input$select_target == "B") {
        #     target_ids <- setdiff(comp_pass, ref_pass)
        # }
        # else {
        #     stop(sprintf("Unknown input$select_target: %s", input$select_target))
        # }
        # 
        # rv$mapping_obj()$get_combined_dataset() %>%
        #     filter(comb_id %in% target_ids) %>%
        #     DT::datatable(
        #         data=.,
        #         selection=list(mode='single', selected=c(1)),
        #         options=list(pageLength=10)
        #     )
    })
    
    output$venn <- renderPlot({

        venn$do_paired_expression_venn(
            ref_pass_reactive(), 
            comp_pass_reactive(), 
            title="Comp", 
            highlight = input$select_target)
    })
    
    observeEvent(rv$filedata_1(), {
        choices <- get_dataset_choices(rv)
        updateSelectInput(session, "dataset1", choices=choices, selected=choices[1])
        updateSelectInput(session, "dataset2", choices=choices, selected=choices[1])
    })
    
    observeEvent(rv$filedata_2(), {
        choices <- get_dataset_choices(rv)
        updateSelectInput(session, "dataset1", choices=choices, selected=choices[1])
        updateSelectInput(session, "dataset2", choices=choices, selected=choices[1])
    })
    
    sync_param_choices <- function() {
        warning("Empty overlap parameter sync for now")
        # ref_choices <- c("None", rv$ddf_cols_ref(rv, input$dataset1))
        # comp_choices <- c("None", rv$ddf_cols_comp(rv, input$dataset2))
        # # updateSelectInput(session, "color_data_ref", choices = ref_choices, selected=ref_choices[1])
        # # updateSelectInput(session, "sample_data1", choices = ref_choices, selected=ref_choices[1])
        # # updateSelectInput(session, "color_data_comp", choices = comp_choices, selected=comp_choices[1])
        # # updateSelectInput(session, "sample_data2", choices = comp_choices, selected=comp_choices[1])
        # 
        # ref_data_choices <- c("None", rv$rdf_cols_ref(rv, input$dataset1))
        # comp_data_choices <- c("None", rv$rdf_cols_comp(rv, input$dataset2))
        # updateSelectInput(session, "data_num_col_ref", choices = ref_data_choices, selected=ref_data_choices[1])
        # updateSelectInput(session, "data_cat_col_ref", choices = ref_data_choices, selected=ref_data_choices[1])
        # updateSelectInput(session, "data_num_col_comp", choices = comp_data_choices, selected=comp_data_choices[1])
        # updateSelectInput(session, "data_cat_col_comp", choices = comp_data_choices, selected=comp_data_choices[1])
    }

    observeEvent(rv$ddf_ref(rv, input$dataset1), {
        sync_param_choices()
    })

    observeEvent(rv$ddf_comp(rv, input$dataset2), {
        sync_param_choices()
    })

    
    observeEvent({
        rv$selected_cols_obj() 
        input$dataset1 
        input$dataset2}, {
            # update_stat_inputs(session, rv, input$dataset1, input$dataset2)
            if (is.null(rv$filename_1()) && is.null(rv$filename_2())) {
                return()
            }
            
            choices_1 <- rv$selected_cols_obj()[[input$dataset1]]$statpatterns
            choices_2 <- rv$selected_cols_obj()[[input$dataset2]]$statpatterns
            
            updateSelectInput(session, "ref_contrast", choices=choices_1, selected=choices_1[1])
            updateSelectInput(session, "comp_contrast", choices=choices_2, selected=choices_2[1])
        })
    
    output$warnings <- renderUI({
        
        error_vect <- c()
        if (is.null(rv$filename_1())) {
            error_vect <- c(error_vect, "No filename_1 found, upload dataset at Setup page")
        }
        
        if (is.null(rv$design_1())) {
            error_vect <- c(error_vect, "No design_1 found, upload dataset at Setup page")
        }
        
        total_text <- paste(error_vect, collapse="<br>")
        HTML(sprintf("<b><font size='5' color='red'>%s</font></b>", total_text))
    })
    
    output$html <- renderUI({
        
        HTML(parse_vector_to_bullets(c(
            "Illustration of presence of certain features across the two datasets",
            "Illustration of presence of certain features in certain statistical comparisons",
            "Illustration of overlap across features passing certain statistical thresholds",
            "Venn diagrams and upsets are alternatives"
        )))
    })
}

