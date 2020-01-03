setup_spotcheck_ui <- function(id) {
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
            p("Spotchecking here, allow jumping here from any screen"),
            fluidRow(
                column(
                    12,
                    wellPanel(
                        fluidRow(
                            column(6,
                                   selectInput(ns("dataset1"), "Reference dataset", choices = c("[Unassigned]"), selected = "[Unassigned]"),
                                   selectInput(ns("ref_cond"), "Ref. cond.", choices = c("[Unassigned]"), selected = "[Unassigned]")
                            ),
                            column(6,
                                   selectInput(ns("dataset2"), "Comp. dataset", choices = c("[Unassigned]"), selected = "[Unassigned]"),
                                   selectInput(ns("comp_cond"), "Comp. cond.", choices = c("[Unassigned]"), selected = "[Unassigned]")
                            )
                        ),
                        checkboxInput(ns("display_show_fields"), "Display column selects", value = FALSE),
                        conditionalPanel(
                            sprintf("input['%s'] == 1", ns("display_show_fields")),
                            fluidRow(
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
                            column(4, checkboxInput(ns("show_boxplot"), "Show boxplot", value=TRUE)),
                            column(4, checkboxInput(ns("show_scatter"), "Show scatter", value=TRUE)),
                            column(4, checkboxInput(ns("show_violin"), "Show violin", value=FALSE))
                        )
                    ),
                    htmlOutput(ns("warnings")),
                    tabsetPanel(
                        type = "tabs",
                        tabPanel("Combined view", plotOutput(ns("spot_display_combined")))
                    ),
                    tabsetPanel(
                        type = "tabs",
                        tabPanel("Combined data", DT::DTOutput(ns("table_display_combined")))
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

module_spotcheck_server <- function(input, output, session, rv) {
    
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
    
    observeEvent({ rv$mapping_obj() }, {
        
        req(rv$mapping_obj()$get_combined_dataset())
        comb_data_cols <- rv$mapping_obj()$get_combined_dataset() %>% colnames()
        samples_ref <- rv$samples(rv, input$dataset1, prefix="d1.")
        samples_comp <- rv$samples(rv, input$dataset2, prefix="d2.")
        start_select <- comb_data_cols[!comb_data_cols %in% c(samples_ref, samples_comp)]
        updateSelectInput(session, "shown_fields", choices = comb_data_cols, selected=start_select) 
    })
    
    # observeEvent(rv$selected_feature(), {
    #     if (input$table_display_combined_rows_selected != rv$selected_feature()) {
    #         
    #     }
    #     # rv$selected_feature(input$table_display_rows_selected)
    # })

    selected_id_reactive <- reactive({
        rv$mapping_obj()$get_combined_dataset()[input$table_display_combined_rows_selected, ]$comb_id %>% as.character()
    })
    
    observeEvent(input$table_display_combined_rows_selected, {
        message("Observed!")
        rv$selected_feature(selected_id_reactive())
        message("Now the value is: ", rv$selected_feature())
    })
    
    sync_param_choices <- function() {
        
        req(rv$ddf_ref(rv, input$dataset1))
        req(rv$ddf_comp(rv, input$dataset2))

        ref_choices <- c("None", rv$ddf_cols_ref(rv, input$dataset1))
        comp_choices <- c("None", rv$ddf_cols_comp(rv, input$dataset2))
        updateSelectInput(session, "ref_cond", choices = ref_choices, selected=rv$ddf_condcol_ref(rv, input$dataset1))
        updateSelectInput(session, "comp_cond", choices = comp_choices, selected=rv$ddf_condcol_comp(rv, input$dataset2))
    }
    
    observeEvent({
        rv$ddf_ref(rv, input$dataset1)
        rv$ddf_comp(rv, input$dataset2)
        rv$design_condcol_1()
        rv$design_condcol_2()
        input$dataset1
        input$dataset2}, {
            sync_param_choices()
    })
    
    output$table_display_combined <- DT::renderDataTable({
        req(rv$mapping_obj())
        req(rv$mapping_obj()$get_combined_dataset())
        
        shown_data <- rv$mapping_obj()$get_combined_dataset()
        
        if (is.null(rv$selected_feature())) {
            selected_row_nbr <- 1
        }
        else {
            selected_row_nbr <- which(shown_data$comb_id %>% as.character() %in% rv$selected_feature())
        }

        round_digits <- 3
        trunc_length <- 20
                
        shown_data %>%
            dplyr::select(input$shown_fields) %>%
            mutate_if(
                is.character,
                ~str_trunc(., trunc_length)
            ) %>%
            mutate_if(
                is.numeric,
                ~round(., round_digits)
            ) %>%
            DT::datatable(
                data=., 
                selection=list(mode='single', selected=c(selected_row_nbr)), 
                options=list(pageLength=10))
    })
    
    plot_df_ref <- reactive({
        req(rv$rdf_ref(rv, input$dataset1))
        req(rv$ddf_ref(rv, input$dataset1))
        req(rv$samples(rv, input$dataset1))
        
        rdf_ref <- rv$rdf_ref(rv, input$dataset1)
        ddf_ref <- rv$ddf_ref(rv, input$dataset1)
        samples_ref <- rv$samples(rv, input$dataset1)
        cond_ref <- input$ref_cond
        
        plt_df_ref <- tibble(
            sample=samples_ref,
            value=rdf_ref[input$table_display_combined_rows_selected, samples_ref] %>% unlist(),
            cond=ddf_ref[[cond_ref]] %>% as.factor()
        )
        plt_df_ref
    })
    
    plot_df_comp <- reactive({
        req(rv$rdf_comp(rv, input$dataset2))
        req(rv$ddf_comp(rv, input$dataset2))
        req(rv$samples(rv, input$dataset2))
        
        rdf_comp <- rv$rdf_comp(rv, input$dataset2)
        ddf_comp <- rv$ddf_comp(rv, input$dataset2)
        samples_comp <- rv$samples(rv, input$dataset2)
        cond_comp <- input$comp_cond
        
        plt_df_comp <- tibble(
            sample=samples_comp,
            value=rdf_comp[input$table_display_combined_rows_selected, samples_comp] %>% unlist(),
            cond=ddf_comp[[cond_comp]] %>% as.factor()
        )
        plt_df_comp
    })
    
    output$spot_display_combined <- renderPlot({
        
        target_row <- input$table_display_combined_rows_selected

        add_geoms <- function(plt, show_box, show_scatter, show_violin) {
            if (show_violin) {
                plt <- plt + geom_violin(na.rm = TRUE)
            }
            if (show_box) {
                plt <- plt + geom_boxplot(na.rm = TRUE)
            }
            if (show_scatter) {
                plt <- plt + geom_point(na.rm = TRUE)
            }
            plt
        }        
        
        if (!is.null(plot_df_ref())) {
            plt_ref_base <- ggplot(plot_df_ref(), aes(x=cond, y=value, color=cond)) + 
                ggtitle(sprintf("Spot check feature: %s", target_row))
            plt_ref <- add_geoms(plt_ref_base, input$show_boxplot, input$show_scatter, input$show_violin)
        }
        
        if (!is.null(plot_df_comp())) {
            plt_comp_base <- ggplot(plot_df_comp(), aes(x=cond, y=value, color=cond)) + 
                ggtitle(sprintf("Spot check feature: %s", target_row))
            plt_comp <- add_geoms(plt_comp_base, input$show_boxplot, input$show_scatter, input$show_violin)
        }

        if (!is.null(plot_df_ref()) && !is.null(plot_df_comp())) {
            ggarrange(plt_ref, plt_comp, nrow=1, ncol=2)
        }
        else if (!is.null(plot_df_ref())) {
            ggarrange(plt_ref, nrow=1, ncol=2)
        }
        else {
            ggarrange(plt_comp, nrow=1, ncol=2)
        }
    })
    
    output$html <- renderUI({
        HTML(parse_vector_to_bullets(c(
            "Allow rapidly switch here after identifying features in other tab",
            "Intensity illustration across characteristic from design (boxplot / scatter)",
            "Profile illustrations allowing display of multiple features (sorted on design condition)"
        )))
    })
}










