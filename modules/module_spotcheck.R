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
                                   selectInput(ns("ref_cond"), "Ref. cond.", choices = c("[Unassigned]"), selected = "[Unassigned]"),
                                   selectInput(ns("shown_fields_ref"), "Ref. shown fields", choices=c("[Unassigned]"), selected="[Unassigned]")
                            ),
                            column(6,
                                   selectInput(ns("dataset2"), "Comp. dataset", choices = c("[Unassigned]"), selected = "[Unassigned]"),
                                   selectInput(ns("comp_cond"), "Comp. cond.", choices = c("[Unassigned]"), selected = "[Unassigned]"),
                                   selectInput(ns("shown_fields_comp"), "Comp. shown fields", choices=c("[Unassigned]"), selected="[Unassigned]")
                            )
                        ),
                        fluidRow(
                            checkboxInput(ns("show_boxplot"), "Show boxplot", value=TRUE),
                            checkboxInput(ns("show_scatter"), "Show scatter", value=TRUE),
                            checkboxInput(ns("show_violin"), "Show violin", value=FALSE)
                        )
                    ),
                    htmlOutput(ns("warnings")),
                    textOutput(ns("test_table_select")),
                    tabsetPanel(
                        type = "tabs",
                        tabPanel("Combined data", DT::DTOutput(ns("table_display_combined")))
                        # tabPanel("Ref. data", DT::DTOutput(ns("table_display_ref"))),
                        # tabPanel("Comp. data", DT::DTOutput(ns("table_display_comp")))
                    ),
                    tabsetPanel(
                        type = "tabs",
                        tabPanel("Combined view", plotOutput(ns("spot_display_combined")))
                        # tabPanel("Single views", 
                        #          fluidRow(
                        #              column(6, plotOutput(ns("spot_display_ref"))),
                        #              column(6, plotOutput(ns("spot_display_comp")))
                        #          )
                        # )
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
    
    output$test_table_select <- renderText({      
        
        target_row_ref <- "[unassigned]"
        if ("table_display_ref_rows_selected" %in% names(input) && !is.null(input$table_display_ref_rows_selected)) {
            target_row_ref <- input$table_display_ref_rows_selected[1]
        }
        
        target_row_comp <- "[unassigned]"
        if ("table_display_comp_rows_selected" %in% names(input) && !is.null(input$table_display_comp_rows_selected)) {
            target_row_comp <- input$table_display_comp_rows_selected[1]
        }
        
        sprintf("Target rows: %s and %s", target_row_ref, target_row_comp)
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
        ref_choices <- c("None", rv$ddf_cols_ref(rv, input$dataset1))
        comp_choices <- c("None", rv$ddf_cols_comp(rv, input$dataset2))
        updateSelectInput(session, "ref_cond", choices = ref_choices, selected=ref_choices[1])
        # updateSelectInput(session, "sample_data1", choices = ref_choices, selected=ref_choices[1])
        updateSelectInput(session, "comp_cond", choices = comp_choices, selected=comp_choices[1])
        # updateSelectInput(session, "sample_data2", choices = comp_choices, selected=comp_choices[1])

        # ref_data_choices <- c("None", rv$rdf_cols_ref(rv, input$dataset1))
        # comp_data_choices <- c("None", rv$rdf_cols_comp(rv, input$dataset2))
        # updateSelectInput(session, "data_num_col_ref", choices = ref_data_choices, selected=ref_data_choices[1])
        # updateSelectInput(session, "data_cat_col_ref", choices = ref_data_choices, selected=ref_data_choices[1])
        # updateSelectInput(session, "data_num_col_comp", choices = comp_data_choices, selected=comp_data_choices[1])
        # updateSelectInput(session, "data_cat_col_comp", choices = comp_data_choices, selected=comp_data_choices[1])
    }
    
    observeEvent({
        rv$ddf_ref(rv, input$dataset1)
        rv$ddf_comp(rv, input$dataset2) }, {
        sync_param_choices()
    })
    
    output$table_display_combined <- DT::renderDataTable({
        req(rv$mapping_obj())
        req(rv$mapping_obj()$get_combined_dataset())
        rv$mapping_obj()$get_combined_dataset() %>%
            DT::datatable(
                data=., 
                selection=list(mode='single', selected=c(1)), 
                options=list(pageLength=10))
    })
    
    # output$table_display_ref <- DT::renderDataTable({
    #     req(rv$rdf_ref(rv, input$dataset1))
    #     rv$rdf_ref(rv, input$dataset1) %>%
    #         DT::datatable(
    #             data=., 
    #             selection=list(mode='single', selected=c(1)), 
    #             options=list(pageLength=10))
    # })
    # 
    # output$table_display_comp <- DT::renderDataTable({
    #     req(rv$rdf_ref(rv, input$dataset2))
    #     rv$rdf_ref(rv, input$dataset2) %>%
    #         DT::datatable(
    #             data=., 
    #             selection=list(mode='single', selected=c(1)), 
    #             options=list(pageLength=10))
    # })
    
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
            cond=ddf_ref[[cond_ref]]
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
            cond=ddf_comp[[cond_comp]]
        )
        plt_df_comp
    })
    
    output$spot_display_combined <- renderPlot({
        
        target_row <- input$table_display_combined_rows_selected

        add_geoms <- function(plt, show_box, show_scatter, show_violin) {
            if (show_box) {
                plt <- plt + geom_boxplot(na.rm = TRUE)
            }
            if (show_scatter) {
                plt <- plt + geom_point(na.rm = TRUE)
            }
            if (show_violin) {
                plt <- plt + geom_violin(na.rm = TRUE)
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
    
    output$spot_display_ref <- renderPlot({
        ggplot() + ggtitle("Spot check figure (ref)")
    })
    
    output$spot_display_comp <- renderPlot({
        ggplot() + ggtitle("Spot check figure (comp)")
    })
    
    output$html <- renderUI({
        HTML(parse_vector_to_bullets(c(
            "Allow rapidly switch here after identifying features in other tab",
            "Intensity illustration across characteristic from design (boxplot / scatter)",
            "Profile illustrations allowing display of multiple features (sorted on design condition)"
        )))
    })
}










