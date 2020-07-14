setup_spotcheck_ui <- function(id) {
    ns <- shiny::NS(id)
    tabPanel(
        id,
        fluidPage(
            bar_w_help_and_download("Spotcheck", ns("help"), ns("download_settings")),
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
                        checkboxInput(ns("more_settings"), "Show advanced settings", value=FALSE),
                        conditionalPanel(
                            sprintf("input['%s'] == 1", ns("more_settings")),
                            fluidRow(
                                column(6, numericInput(ns("text_size"), "Text size", value=10)),
                                column(6, numericInput(ns("text_angle"), "Axis x text angle", value=0))
                            ),
                            fluidRow(
                                column(6, checkboxInput(ns("assign_numeric_as_factor"), "Numeric as factor", value=TRUE)),
                                column(6, selectInput(ns("multiselect"), "Feature selection mode", choices=c("single", "multiple"), selected="single"))
                            ),
                            fluidRow(
                                column(6, textInput(ns("ref_title"), "Ref. title")),
                                column(6, textInput(ns("comp_title"), "Comp. title"))
                            )
                        )
                    ),
                    fluidRow(
                        column(6, 
                               fluidRow(
                                   actionButton(ns("update_spotcheck"), "Visualize selected features"),
                                   downloadButton(ns("download_table"), "Download table")
                               ),
                               fluidRow(DT::DTOutput(ns("table_display")), style="overflow-x:scroll;")
                        ),
                        column(6, 
                               plotlyOutput(ns("spot_display_ref")) %>% withSpinner(),
                               plotlyOutput(ns("spot_display_comp")) %>% withSpinner()
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

module_spotcheck_server <- function(input, output, session, rv, module_name) {
    
    output$download_table <- downloadHandler(
        filename = function() {
            paste("spotcheck-", format(Sys.time(), "%y%m%d_%H%M%S"), ".tsv", sep="")
        },
        content = function(file) {
            write_tsv(rv$dt_parsed_data_raw(rv, rv$mapping_obj()$get_combined_dataset(include_non_matching=TRUE)), file)
        }
    )
    
    observeEvent(input$help, {
        shinyalert(
            title = "Help: Spot-check visuals",
            text = help_spotcheck, 
            html = TRUE
        )
    })
    
    output$download_settings <- settings_download_handler("spotcheck", input)
    
    observeEvent({
        rv$filedata_1()
        rv$filedata_2()}, {
        choices <- get_dataset_choices(rv)
        updateSelectInput(session, "dataset1", choices=choices, selected=choices[1])
        updateSelectInput(session, "dataset2", choices=choices, selected=choices[1])
    }, ignoreInit=TRUE, ignoreNULL=FALSE)
    
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
        shiny::validate(need(!is.null(rv$mapping_obj()), "No mapping object found, are samples mapped at the Setup page?"))
        shiny::validate(need(!is.null(rv$mapping_obj()$get_combined_dataset()), "No combined dataset found, are samples mapped at the Setup page?"))
        
        rv$dt_parsed_data(rv, rv$mapping_obj()$get_combined_dataset(include_non_matching=TRUE), selection_mode=input$multiselect)
    })
    
    plot_df_ref <- reactive({
        shiny::validate(need(!is.null(rv$rdf_ref(rv, input$dataset1)), "No data matrix found, is it loaded at the Setup page?"))
        shiny::validate(need(!is.null(rv$ddf_ref(rv, input$dataset1)), "No design matrix found, is it loaded at the Setup page?"))
        shiny::validate(need(!is.null(rv$samples(rv, input$dataset1)), "No mapped samples found, are they mapped at the Setup page?"))
        shiny::validate(need(!is.null(input$table_display_rows_selected), "No rows to display found, something seems to be wrong"))

        map_df <- rv$mapping_obj()$get_combined_dataset(include_non_matching=TRUE)
        ddf_ref <- rv$ddf_ref(rv, input$dataset1)
        ddf_ref$None <- "None"
        samples_ref <- rv$samples(rv, input$dataset1)
        cond_ref <- input$ref_cond
        ref_ind <- di_new(rv, input$dataset1)
        samples_names <- paste0(sprintf("d%s.", ref_ind), samples_ref)
        annot_col <- sprintf("d%s.%s", ref_ind, rv$rdf_annotcol_ref(rv, input$dataset1))
        
        if (input$assign_numeric_as_factor) parsed_cond <- ddf_ref[[cond_ref]] %>% as.factor()
        else parsed_cond <- ddf_ref[[cond_ref]]
        
        plt_df_ref <- map_df %>% 
            dplyr::filter(.data$comb_id %in% sprintf("C%s", input$table_display_rows_selected)) %>%
            dplyr::select(.data$comb_id, map_id=annot_col, all_of(samples_names)) %>%
            tidyr::pivot_longer(all_of(samples_names), names_to="sample") %>%
            dplyr::mutate(cond=rep(parsed_cond, length(input$table_display_rows_selected)))
        
        plt_df_ref
    })
    
    plot_df_comp <- reactive({
        shiny::validate(need(!is.null(rv$rdf_ref(rv, input$dataset2)), "No data matrix found, is it loaded at the Setup page?"))
        shiny::validate(need(!is.null(rv$ddf_ref(rv, input$dataset2)), "No design matrix found, is it loaded at the Setup page?"))
        shiny::validate(need(!is.null(rv$samples(rv, input$dataset2)), "No mapped samples found, are they mapped at the Setup page?"))
        shiny::validate(need(!is.null(input$table_display_rows_selected), "No rows to display found, something seems to be wrong"))

        map_df <- rv$mapping_obj()$get_combined_dataset(include_non_matching=TRUE)
        ddf_comp <- rv$ddf_comp(rv, input$dataset2)
        ddf_comp$None <- "None"
        samples_comp <- rv$samples(rv, input$dataset2)
        cond_comp <- input$comp_cond
        comp_ind <- di_new(rv, input$dataset2)
        samples_names <- paste0(sprintf("d%s.", comp_ind), samples_comp)
        annot_col <- sprintf("d%s.%s", comp_ind, rv$rdf_annotcol_comp(rv, input$dataset2))
        
        if (input$assign_numeric_as_factor) parsed_cond <- ddf_comp[[cond_comp]] %>% as.factor()
        else parsed_cond <- ddf_comp[[cond_comp]]

        plt_df_comp <- map_df %>% 
            dplyr::filter(.data$comb_id %in% sprintf("C%s", input$table_display_rows_selected)) %>%
            dplyr::select(.data$comb_id, map_id=annot_col, all_of(samples_names)) %>%
            tidyr::pivot_longer(all_of(samples_names), names_to="sample") %>%
            dplyr::mutate(cond=rep(parsed_cond, length(input$table_display_rows_selected)))
        
        plt_df_comp
    })

    
    make_spotcheck_plot <- function(plot_df, target_row, show_boxplot, show_scatter, show_violin, title=NULL, text_size=10, text_angle=90, text_vjust=0.5) {
        add_geoms <- function(plt, show_box, show_scatter, show_violin) {
            if (show_violin) {
                plt <- plt + geom_violin(na.rm = TRUE)
            }
            if (show_box) {
                plt <- plt + geom_boxplot(na.rm = TRUE)
            }
            if (show_scatter) {
                plt <- plt + geom_point(na.rm = TRUE, position=position_dodge(width=0.75))
            }
            plt
        }
        
        if (length(unique(plot_df$comb_id)) == 1) {
            plt_ref_base <- ggplot(plot_df, aes(x=.data$cond, y=.data$value, color=.data$cond, label=.data$sample))
        }
        else {
            plt_ref_base <- ggplot(plot_df, aes(x=.data$cond, y=.data$value, color=.data$comb_id, group=.data$comb_id, label=.data$sample))
        }
        
        if (is.null(title) || title == "") {
            if (length(unique(plot_df$map_id)) != 1) {
                stop("Unknown state for map_id, expected one unique value, received: ", paste(unique(plot_df$map_id), collapse=","))
            }
            title <- sprintf("%s (C%s)", plot_df$map_id[1], paste(target_row, collapse=","))
        }
        
        plt_ref_base <- plt_ref_base +
            ggtitle(title) +
            xlab("Condition") +
            ylab("Abundance")
        
        plt_ref <- add_geoms(plt_ref_base, show_boxplot, show_scatter, show_violin)
        plt_ref + theme_bw() + theme(text=element_text(size=text_size), axis.text.x=element_text(vjust = text_vjust, angle = text_angle), legend.title = element_blank())
    }
        
    output$spot_display_ref <- renderPlotly({

        shiny::validate(need(!is.null(plot_df_ref()), "Reference plot data frame needed but not found, something went wrong!"))

        target_rows <- input$table_display_rows_selected
        plt <- make_spotcheck_plot(
            plot_df_ref(),
            target_rows,
            input$show_boxplot,
            input$show_scatter,
            input$show_violin,
            title=input$ref_title,
            text_size=input$text_size,
            text_angle=input$text_angle,
            text_vjust=input$text_vjust
        ) %>% ggplotly()
        
        if (input$multiselect == "multiple") {
            plt <- plt %>% plotly::layout(boxmode="group")
        }
        
        plt <- plt %>% 
            plotly::layout(xaxis=list(tickangle=input$text_angle)) %>% 
            assign_fig_settings(rv)
        plt
    })
    
    output$spot_display_comp <- renderPlotly({
        
        shiny::validate(need(!is.null(plot_df_ref()), "Comparison plot data frame needed but not found, something went wrong!"))
        
        target_row <- input$table_display_rows_selected
        plt <- make_spotcheck_plot(
            plot_df_comp(),
            target_row,
            input$show_boxplot,
            input$show_scatter,
            input$show_violin,
            title=input$comp_title,
            text_size=input$text_size,
            text_angle=input$text_angle,
            text_vjust=input$text_vjust
        ) %>% ggplotly()
        
        if (input$multiselect == "multiple") {
            plt <- plt %>% plotly::layout(boxmode="group")
        }
        
        plt <- plt %>% 
            plotly::layout(xaxis=list(tickangle=input$text_angle)) %>% 
            assign_fig_settings(rv)
        plt
    })
}
