setup_spotcheck_ui <- function(id) {
    ns <- NS(id)
    tabPanel(
        id,
        fluidPage(
            top_bar_w_help("Spotcheck", ns("help")),
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
                        fluidRow(
                            column(3, checkboxInput(ns("show_boxplot"), "Show boxplot", value=TRUE)),
                            column(3, checkboxInput(ns("show_scatter"), "Show scatter", value=TRUE)),
                            column(3, checkboxInput(ns("show_violin"), "Show violin", value=FALSE)),
                            column(3, checkboxInput(ns("show_labels"), "Show labels", value=FALSE))
                        )
                    ),
                    htmlOutput(ns("warnings")),
                    tabsetPanel(
                        type = "tabs",
                        tabPanel("Combined view", plotOutput(ns("spot_display_combined")) %>% withSpinner())
                    ),
                    tabsetPanel(
                        type = "tabs",
                        tabPanel("Combined data", DT::DTOutput(ns("table_display")))
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

module_spotcheck_server <- function(input, output, session, rv, module_name) {
    
    observeEvent(input$help, {
        shinyalert(
            title = "Help: Spot-check visuals",
            text = help_spotcheck, 
            html = TRUE
        )
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
    
    selected_id_reactive <- reactive({
        rv$mapping_obj()$get_combined_dataset()[input$table_display_rows_selected, ]$comb_id %>% 
            as.character()
    })
    
    observeEvent(input$table_display_rows_selected, {
        rv$set_selected_feature(selected_id_reactive(), module_name)
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
    
    output$table_display <- DT::renderDataTable({
        req(rv$mapping_obj())
        req(rv$mapping_obj()$get_combined_dataset())
        rv$dt_parsed_data(rv, rv$mapping_obj()$get_combined_dataset())
    })
    
    plot_df_ref <- reactive({
        req(rv$rdf_ref(rv, input$dataset1))
        req(rv$ddf_ref(rv, input$dataset1))
        req(rv$samples(rv, input$dataset1))
        req(input$table_display_rows_selected)
        
        rdf_ref <- rv$rdf_ref(rv, input$dataset1)
        ddf_ref <- rv$ddf_ref(rv, input$dataset1)
        samples_ref <- rv$samples(rv, input$dataset1)
        cond_ref <- input$ref_cond
        
        plt_df_ref <- tibble(
            sample=samples_ref,
            value=rdf_ref[input$table_display_rows_selected, samples_ref] %>% unlist(),
            cond=ddf_ref[[cond_ref]] %>% as.factor()
        )
        plt_df_ref
    })
    
    plot_df_comp <- reactive({
        req(rv$rdf_comp(rv, input$dataset2))
        req(rv$ddf_comp(rv, input$dataset2))
        req(rv$samples(rv, input$dataset2))
        req(input$table_display_rows_selected)
        
        rdf_comp <- rv$rdf_comp(rv, input$dataset2)
        ddf_comp <- rv$ddf_comp(rv, input$dataset2)
        samples_comp <- rv$samples(rv, input$dataset2)
        cond_comp <- input$comp_cond
        
        plt_df_comp <- tibble(
            sample=samples_comp,
            value=rdf_comp[input$table_display_rows_selected, samples_comp] %>% unlist(),
            cond=ddf_comp[[cond_comp]] %>% as.factor()
        )
        plt_df_comp
    })
    
    output$spot_display_combined <- renderPlot({
        
        req(plot_df_ref())
        req(plot_df_comp())
        
        target_row <- input$table_display_rows_selected
        add_geoms <- function(plt, show_box, show_scatter, show_violin, show_labels) {
            if (show_violin) {
                plt <- plt + geom_violin(na.rm = TRUE)
            }
            if (show_box) {
                plt <- plt + geom_boxplot(na.rm = TRUE)
            }
            if (show_scatter) {
                plt <- plt + geom_point(na.rm = TRUE)
            }
            if (show_labels) {
                plt <- plt + geom_text_repel(na.rm = TRUE)
            }
            plt
        }
        
        plt_ref_base <- ggplot(plot_df_ref(), aes(x=cond, y=value, color=cond, label=sample)) + 
            ggtitle(sprintf("Spot check feature: %s", target_row))
        plt_ref <- add_geoms(plt_ref_base, input$show_boxplot, input$show_scatter, input$show_violin, input$show_labels)

        plt_comp_base <- ggplot(plot_df_comp(), aes(x=cond, y=value, color=cond, label=sample)) + 
            ggtitle(sprintf("Spot check feature: %s", target_row))
        plt_comp <- add_geoms(plt_comp_base, input$show_boxplot, input$show_scatter, input$show_violin, input$show_labels)

        ggarrange(plt_ref, plt_comp, nrow=1, ncol=2)
    })
}










