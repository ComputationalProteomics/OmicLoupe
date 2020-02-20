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
                            column(4, checkboxInput(ns("show_boxplot"), "Show boxplot", value=TRUE)),
                            column(4, checkboxInput(ns("show_scatter"), "Show scatter", value=TRUE)),
                            column(4, checkboxInput(ns("show_violin"), "Show violin", value=FALSE))
                        ),
                        fluidRow(
                            column(6, checkboxInput(ns("assign_numeric_as_factor"), "Numeric as factor", value=TRUE))
                        )
                    ),
                    htmlOutput(ns("warnings")),
                    tabsetPanel(
                        type = "tabs",
                        tabPanel(
                            "Combined view", 
                            fluidRow(
                                column(6, plotlyOutput(ns("spot_display_ref")) %>% withSpinner()),
                                column(6, plotlyOutput(ns("spot_display_comp")) %>% withSpinner())
                            )
                        )
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

        set_if_new <- function(prev_val, new_values, new_val_selected) {
            if (is.null(prev_val)) new_val_selected
            else if (prev_val %in% new_values) prev_val
            else new_val_selected
        }
        
        ref_choices <- c("None", rv$ddf_cols_ref(rv, input$dataset1))
        comp_choices <- c("None", rv$ddf_cols_comp(rv, input$dataset2))
        updateSelectInput(session, "ref_cond", choices = ref_choices, selected=set_if_new(input$ref_cond, ref_choices, ref_choices[1]))
        updateSelectInput(session, "comp_cond", choices = comp_choices, selected=set_if_new(input$comp_cond, comp_choices, comp_choices[1]))
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
        
        map_df <- rv$mapping_obj()$get_combined_dataset()
        ddf_ref <- rv$ddf_ref(rv, input$dataset1)
        ddf_ref$None <- "None"
        samples_ref <- rv$samples(rv, input$dataset1)
        cond_ref <- input$ref_cond
        ref_ind <- di_new(rv, input$dataset1)
        samples_names <- paste0(sprintf("d%s.", ref_ind), samples_ref)
        
        if (input$assign_numeric_as_factor) parsed_cond <- ddf_ref[[cond_ref]] %>% as.factor()
        else parsed_cond <- ddf_ref[[cond_ref]]

        plt_df_ref <- tibble(
            sample=samples_names,
            value=map_df %>% filter(comb_id == sprintf("C%s", input$table_display_rows_selected)) %>% dplyr::select(all_of(samples_names)) %>% unlist(),
            cond=parsed_cond
        )
        plt_df_ref
    })
    
    plot_df_comp <- reactive({
        req(rv$rdf_comp(rv, input$dataset2))
        req(rv$ddf_comp(rv, input$dataset2))
        req(rv$samples(rv, input$dataset2))
        req(input$table_display_rows_selected)

        map_df <- rv$mapping_obj()$get_combined_dataset()
        ddf_comp <- rv$ddf_comp(rv, input$dataset2)
        ddf_comp$None <- "None"
        samples_comp <- rv$samples(rv, input$dataset2)
        cond_comp <- input$comp_cond
        comp_ind <- di_new(rv, input$dataset2)
        samples_names <- paste0(sprintf("d%s.", comp_ind), samples_comp)
        
        if (input$assign_numeric_as_factor) parsed_cond <- ddf_comp[[cond_comp]] %>% as.factor()
        else parsed_cond <- ddf_comp[[cond_comp]]
        
        plt_df_comp <- tibble(
            sample=samples_names,
            value=map_df %>% filter(comb_id == sprintf("C%s", input$table_display_rows_selected)) %>% dplyr::select(all_of(samples_names)) %>% unlist(),
            cond=parsed_cond
        )
        plt_df_comp
    })

    make_spotcheck_plot <- function(plot_df, target_row, show_boxplot, show_scatter, show_violin) {
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
            plt
        }
        
        plt_ref_base <- ggplot(plot_df, aes(x=cond, y=value, color=cond, label=sample)) + 
            ggtitle(sprintf("Spot check feature: %s", target_row)) +
            xlab("Condition") +
            ylab("Abundance")
        
        plt_ref <- add_geoms(plt_ref_base, show_boxplot, show_scatter, show_violin, show_labels)
        plt_ref
    }
        
    output$spot_display_ref <- renderPlotly({
        req(plot_df_ref())

        target_row <- input$table_display_rows_selected
        make_spotcheck_plot(
            plot_df_ref(),
            target_row,
            input$show_boxplot,
            input$show_scatter,
            input$show_violin
        ) %>% ggplotly()
    })
    
    output$spot_display_comp <- renderPlotly({
        
        req(plot_df_comp())
        
        target_row <- input$table_display_rows_selected
        make_spotcheck_plot(
            plot_df_comp(),
            target_row,
            input$show_boxplot,
            input$show_scatter,
            input$show_violin
        ) %>% ggplotly()
    })
    
    output$warnings <- renderUI({
        
        error_vect <- c()
        if (is.null(rv$filename_1())) {
            error_vect <- c(error_vect, "No filename_1 found, upload dataset at Setup page")
        }
        else if (is.null(rv$samples(rv, input$dataset1)) || length(rv$samples(rv, input$dataset1)) == 0) {
            error_vect <- c(error_vect, "No mapped samples found, perform sample mapping at Setup page")
        }
        
        if (!is.null(rv$filename_2()) && (is.null(rv$samples(rv, input$dataset2)) || length(rv$samples(rv, input$dataset2)) == 0)) {
            error_vect <- c(error_vect, "No mapped samples found for second dataset, perform mapping at Setup page to show second plot")
        }
        
        if (is.null(rv$design_1())) {
            error_vect <- c(error_vect, "No design_1 found, upload dataset at Setup page")
        }
        
        total_text <- paste(error_vect, collapse="<br>")
        HTML(sprintf("<b><font size='5' color='red'>%s</font></b>", total_text))
    })
}










