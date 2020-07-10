setup_quality_ui <- function(id) {
    ns <- NS(id)
    tabPanel(
        id,
        fluidPage(
            bar_w_help("Quality", ns("help")),
            fluidRow(
                column(
                    12,
                    wellPanel(
                        fluidRow(
                            column(6,
                                   selectInput(ns("dataset1"), "Reference dataset", choices = c(""), selected = ""),
                                   selectInput(ns("dataset2"), "Comparison dataset", choices = c(""), selected = "")
                            ),
                            column(6,
                                   selectInput(ns("color_data_ref"), "Color", choices=c("")),
                                   selectInput(ns("color_data_comp"), "Color", choices=c(""))
                            )
                        )
                    ),
                    wellPanel(
                        conditionalPanel(
                            sprintf("input['%s'] == 'Boxplots'", ns("plot_tabs")),
                            fluidRow(
                                column(3, checkboxInput(ns("do_violin"), "Do violin", value=FALSE)),
                                column(3, checkboxInput(ns("rotate_label"), "Rotate label", value=TRUE)),
                                column(3, checkboxInput(ns("order_on_cond"), "Order on condition", value=FALSE)),
                                column(3, checkboxInput(ns("numeric_as_category"), "Numeric as category", value=FALSE))
                            )
                        ),
                        conditionalPanel(
                            sprintf("input['%s'] == 'Density'", ns("plot_tabs")),
                            p("No settings")
                        ),
                        conditionalPanel(
                            sprintf("input['%s'] == 'Barplots'", ns("plot_tabs")),
                            fluidRow(
                                column(4, checkboxInput(ns("show_missing_ref"), "Show missing ref.", value=FALSE)),
                                column(4, checkboxInput(ns("show_missing_comp"), "Show missing comp.", value=FALSE)),
                                column(4, checkboxInput(ns("rotate_label_barplot"), "Rotate label", value=TRUE))
                            )
                        ),
                        conditionalPanel(
                            sprintf("input['%s'] == 'Histograms'", ns("plot_tabs")),
                            fluidRow(
                                column(6,
                                       selectInput(ns("data_num_col_ref"), "Histogram column ref", choices=c("")),
                                       selectInput(ns("data_cat_col_ref"), "Fill column ref", choices=c(""))
                                ),
                                column(6,
                                       selectInput(ns("data_num_col_comp"), "Histogram column comp", choices=c("")),
                                       selectInput(ns("data_cat_col_comp"), "Fill column comp", choices=c(""))
                                )
                            ),
                            fluidRow(
                                column(4, numericInput(ns("max_color_cats"), "Maximum color cats.", min=1, step=1, value=5)),
                                column(4, numericInput(ns("hist_bins"), "Histogram bins.", min=10, step=5, value=50)),
                                column(4, numericInput(ns("numeric_color_bins"), "Numeric color bins", min=1, step=1, value=4))
                            )
                        ),
                        conditionalPanel(
                            sprintf("input['%s'] == 'Dendrograms'", ns("plot_tabs")),
                            numericInput(ns("dendrogram_height"), "Dendrogram plot height (inactive, requires UI render)", value=500, min = 50, step = 50),
                            numericInput(ns("dendrogram_textsize"), "Dendrogram text size", value=3, min=1, step=1)
                        ),
                        checkboxInput(ns("show_more_settings"), "Show more settings", value = FALSE),
                        conditionalPanel(
                            sprintf("input['%s'] == 1", ns("show_more_settings")),
                            fluidRow(
                                column(6, textInput(ns("custom_title1"), "Custom title 1", value = "")),
                                column(6, textInput(ns("custom_title2"), "Custom title 2", value = ""))
                            ),
                            fluidRow(
                                column(6, numericInput(ns("text_size"), "Text size", value=10)),
                                column(3, downloadButton(ns("ggplot_download_ref"), "Download static (ref)"), p("(Download interactive by hover option)")),
                                column(3, downloadButton(ns("ggplot_download_comp"), "Download static (comp)"))
                            )
                        )
                    ),
                    # htmlOutput(ns("warnings")),
                    tabsetPanel(
                        id = ns("plot_tabs"),
                        type = "tabs",
                        tabPanel("Boxplots", 
                                 plotOutput(ns("boxplots_ref")) %>% withSpinner(),
                                 plotOutput(ns("boxplots_comp")) %>% withSpinner()
                        ),
                        tabPanel("Density", 
                                 plotlyOutput(ns("density_ref_plotly")) %>% withSpinner(),
                                 plotlyOutput(ns("density_comp_plotly")) %>% withSpinner()
                        ),
                        tabPanel("Barplots", 
                                 plotlyOutput(ns("bars_ref")) %>% withSpinner(),
                                 plotlyOutput(ns("bars_comp")) %>% withSpinner()
                        ),
                        tabPanel("Dendrograms",
                                 fluidRow(
                                     column(6, plotOutput(ns("dendrogram_ref")) %>% withSpinner()),
                                     column(6, plotOutput(ns("dendrogram_comp")) %>% withSpinner())
                                 )
                        ),
                        tabPanel("Histograms", 
                                 plotOutput(ns("histogram_ref")) %>% withSpinner(),
                                 plotOutput(ns("histogram_comp")) %>% withSpinner()
                        )
                    )
                )
            )
        )
    )
}

module_quality_server <- function(input, output, session, rv, module_name) {
    
    observeEvent(input$help, {
        shinyalert(
            title = "Help: Quality visuals",
            text = help_quality, 
            html = TRUE
        )
    })
    
    # Observers
    observeEvent({
        rv$filedata_1()
        rv$filedata_2()}, {

        choices <- get_dataset_choices(rv)
        updateSelectInput(session, "dataset1", choices=choices, selected=choices[1])
        updateSelectInput(session, "dataset2", choices=choices, selected=choices[1])
    }, ignoreInit=TRUE, ignoreNULL=FALSE)
    
    # observeEvent(rv$filedata_2(), {
    #     message("observeEvent(rv$filedata_2()")
    #     choices <- get_dataset_choices(rv)
    #     updateSelectInput(session, "dataset1", choices=choices, selected=choices[1])
    #     updateSelectInput(session, "dataset2", choices=choices, selected=choices[1])
    # })
    
    ggplot_download <- function(file, target) {
        
        dpi <- rv$figure_save_dpi()
        if (input$plot_tabs == "Boxplots") {
            plot_func <- plot_functions[[sprintf("boxplot_%s", target)]]
        }
        else if (input$plot_tabs == "Dendrograms") {
            plot_func <- plot_functions[[sprintf("dendrogram_%s", target)]]
        }
        else if (input$plot_tabs == "Histograms") {
            plot_func <- plot_functions[[sprintf("histogram_%s", target)]]
        }
        else {
            stop(sprintf("Unknown state for input$plot_tabs: %s", input$plot_tabs))
        }
        
        ggsave(
            file, 
            plot = plot_func(),
            width = rv$figure_save_width() / dpi, 
            height = rv$figure_save_height() / dpi, 
            units = "in", 
            dpi = dpi)
    }
    
    output$ggplot_download_ref <- downloadHandler(
        filename = function() {
            sprintf('ref-%s-%s.%s', tolower(input$plot_tabs), Sys.Date(), rv$figure_save_format())
        },
        content = function(file) {
            ggplot_download(file, "ref")
        }
    )
    
    output$ggplot_download_comp <- downloadHandler(
        filename = function() {
            sprintf('comp-%s-%s.%s', tolower(input$plot_tabs), Sys.Date(), rv$figure_save_format())
        },
        content = function(file) {
            ggplot_download(file, "comp")
        }
    )
    
    sync_param_choices <- function() {
        
        validate(need(!is.null(rv$ddf_ref(rv, input$dataset1)), "Didn't find any design for dataset 1 while syncing input choices"))
        validate(need(!is.null(rv$ddf_comp(rv, input$dataset2)), "Didn't find any design for dataset 2 while syncing input choices"))
        
        set_if_new <- function(prev_val, new_values, new_val_selected) {
            if (is.null(prev_val)) new_val_selected
            else if (prev_val %in% new_values) prev_val
            else new_val_selected
        }
        
        ref_choices <- c("None", rv$ddf_cols_ref(rv, input$dataset1))
        comp_choices <- c("None", rv$ddf_cols_comp(rv, input$dataset2))
        
        updateSelectInput(session, "color_data_ref", choices = ref_choices, selected=set_if_new(input$color_data1, ref_choices, rv$ddf_condcol_ref(rv, input$dataset1)))
        updateSelectInput(session, "color_data_comp", choices = comp_choices, selected=set_if_new(input$color_data2, comp_choices, rv$ddf_condcol_comp(rv, input$dataset2)))
        
        updateSelectInput(session, "sample_data1", choices = ref_choices, selected=set_if_new(input$sample_data1, ref_choices, ref_choices[1]))
        updateSelectInput(session, "sample_data2", choices = comp_choices, selected=set_if_new(input$sample_data2, comp_choices, comp_choices[1]))
        
        ref_data_choices <- c("None", rv$rdf_cols_ref(rv, input$dataset1))
        comp_data_choices <- c("None", rv$rdf_cols_comp(rv, input$dataset2))
        updateSelectInput(session, "data_num_col_ref", choices = ref_data_choices, selected=set_if_new(input$data_num_col_ref, ref_data_choices, ref_data_choices[1]))
        updateSelectInput(session, "data_cat_col_ref", choices = ref_data_choices, selected=set_if_new(input$data_cat_col_ref, ref_data_choices, ref_data_choices[1]))
        updateSelectInput(session, "data_num_col_comp", choices = comp_data_choices, selected=set_if_new(input$data_num_col_comp, comp_data_choices, comp_data_choices[1]))
        updateSelectInput(session, "data_cat_col_comp", choices = comp_data_choices, selected=set_if_new(input$data_cat_col_comp, comp_data_choices, comp_data_choices[1]))
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
    
    get_long <- function(data_ind, rv, ddf_samplecol) {
        
        validate(need(
            !is.null(rv$mapping_obj()[[sprintf("dataset%s", data_ind)]]), 
            "Did not find dataset mapping, have you loaded the data at the Setup page?"))
        validate(need(
            !is.null(rv$mapping_obj()[[sprintf("samples%s", data_ind)]]), 
            "Did not find sample columns, have you mapped your samples at the Setup page?"))
        
        dataset <- rv$mapping_obj()[[sprintf("dataset%s", data_ind)]]
        sample_cols <- rv$mapping_obj()[[sprintf("samples%s", data_ind)]]
        sdf <- dataset[, sample_cols]
        ddf <- rv[[sprintf("design_%s", data_ind)]]()
        ddf$name <- ddf[[ddf_samplecol]]
        
        # join_by <- c("name"=ddf_samplecol)
        long_sdf <- sdf %>%
            pivot_longer(all_of(sample_cols)) %>%
            inner_join(ddf, by="name")
        long_sdf
    }
    
    reactive_long_sdf_ref <- reactive({
        long_df <- get_long(di_new(rv, input$dataset1, 1), rv, ref_ddf_samplecol())
        if (input$numeric_as_category) {
            long_df[[ref_color()]] <- as.factor(long_df[[ref_color()]])
        }
        long_df
    })
    
    reactive_long_sdf_comp <- reactive({
        long_df <- get_long(di_new(rv, input$dataset2, 2), rv, comp_ddf_samplecol())
        if (input$numeric_as_category) {
            long_df[[comp_color()]] <- as.factor(long_df[[comp_color()]])
        }
        long_df
    })
    
    ref_sdf <- reactive({
        rv$rdf_ref(rv, input$dataset1)[, rv$samples(rv, input$dataset1)]
    })
    
    comp_sdf <- reactive({
        rv$rdf_comp(rv, input$dataset2)[, rv$samples(rv, input$dataset2)]
    })
    
    ref_ddf_samplecol <- reactive({
        rv[[sprintf("design_samplecol_%s", di_new(rv, input$dataset1, 1))]]()
    })

    comp_ddf_samplecol <- reactive({
        rv[[sprintf("design_samplecol_%s", di_new(rv, input$dataset2, 2))]]()
    })
    
    # Illustrations
    ref_color <- reactive({
        if (input$color_data_ref != "None") { 
            input$color_data_ref
        }
        else { NULL }
    })
    comp_color <- reactive({
        if (input$color_data_comp != "None") { 
            input$color_data_comp
        }
        else { NULL }
    })
    
    output$bars_ref <- renderPlotly({ 
        
        validate(need(rv$ddf_ref(rv, input$dataset1), "No design matrix found, please upload at the Setup page"))
        validate(need(reactive_long_sdf_ref(), "No data matrix found, please upload at the Setup page"))
        
        make_barplot(
            reactive_long_sdf_ref(), 
            "value", 
            rv$ddf_ref(rv, input$dataset1), 
            ref_ddf_samplecol(), 
            input$dataset1, 
            ref_color(), 
            show_missing=input$show_missing_ref, 
            input$rotate_label_barplot,
            title=input$custom_title1,
            text_size=input$text_size) %>%
                plotly::ggplotly() %>% 
                assign_fig_settings(rv)
    })
    
    output$bars_comp <- renderPlotly({
        
        # req(rv$ddf_comp(rv, input$dataset2))
        # req(reactive_long_sdf_comp())
        validate(need(rv$ddf_comp(rv, input$dataset2), "No design matrix found, please upload at the Setup page"))
        validate(need(reactive_long_sdf_comp(), "No data matrix found, please upload at the Setup page"))
        
        make_barplot(
            reactive_long_sdf_comp(), 
            "value", 
            rv$ddf_comp(rv, input$dataset2), 
            comp_ddf_samplecol(), 
            input$dataset2, 
            comp_color(), 
            show_missing=input$show_missing_comp, 
            input$rotate_label_barplot,
            title=input$custom_title2,
            text_size=input$text_size) %>%
                plotly::ggplotly() %>% 
                assign_fig_settings(rv)
    })

    plot_functions <- list()
    plot_functions$boxplot_ref <- reactive({
        plt_ref <- ggplot(
            reactive_long_sdf_ref(), 
            aes_string(x="name", y="value", color=ref_color()))
        
        if (input$custom_title1 == "") plt_ref <- plt_ref + ggtitle(sprintf("Dataset: %s Color: %s", input$dataset1, ref_color()))
        else plt_ref <- plt_ref + ggtitle(input$custom_title1)
        
        adjust_boxplot(
            plt_ref, 
            input$do_violin, 
            input$rotate_label, 
            input$order_on_cond,
            rv$ddf_ref(rv, input$dataset1),
            rv$ddf_samplecol_ref(rv, input$dataset1),
            input$color_data_ref,
            text_size=input$text_size
        )
    })
    
    plot_functions$boxplot_comp <- reactive({
        plt_comp <- ggplot(
            reactive_long_sdf_comp(), 
            aes_string(x="name", y="value", color=comp_color()))
        
        if (input$custom_title2 == "") plt_comp <- plt_comp + ggtitle(sprintf("Dataset: %s Color: %s", input$dataset2, comp_color()))
        else plt_comp <- plt_comp + ggtitle(input$custom_title2)
        
        adjust_boxplot(
            plt_comp, 
            input$do_violin, 
            input$rotate_label, 
            input$order_on_cond,
            rv$ddf_comp(rv, input$dataset2),
            rv$ddf_samplecol_comp(rv, input$dataset2),
            input$color_data_comp,
            text_size=input$text_size
        )
    })
    
    output$boxplots_ref <- renderPlot({ 
        
        validate(need(rv$ddf_ref(rv, input$dataset1), "No design matrix found, please upload at the Setup page"))
        validate(need(reactive_long_sdf_ref(), "No data matrix found, please upload at the Setup page"))
        
        plot_functions$boxplot_ref()
    })

    output$boxplots_comp <- renderPlot({ 
        
        validate(need(rv$ddf_comp(rv, input$dataset2), "No design matrix found, please upload at the Setup page"))
        validate(need(reactive_long_sdf_comp(), "No data matrix found, please upload at the Setup page"))
        plot_functions$boxplot_comp()
    })
    
    output$density_ref_plotly <- renderPlotly({
        
        validate(need(rv$ddf_ref(rv, input$dataset1), "No design matrix found, please upload at the Setup page"))
        validate(need(reactive_long_sdf_ref(), "No data matrix found, please upload at the Setup page"))
        
        make_density_plot(
            reactive_long_sdf_ref(),
            ref_color(),
            curr_dataset=input$dataset1,
            title=input$custom_title1,
            text_size=input$text_size
        ) %>% assign_fig_settings(rv)
    })
    
    output$density_comp_plotly <- renderPlotly({

        validate(need(rv$ddf_comp(rv, input$dataset2), "No design matrix found, please upload at the Setup page"))
        validate(need(reactive_long_sdf_comp(), "No data matrix found, please upload at the Setup page"))
        
        make_density_plot(
            reactive_long_sdf_comp(),
            comp_color(),
            curr_dataset=input$dataset2,
            title=input$custom_title2,
            text_size=input$text_size
        ) %>% config(toImageButtonOptions=list(
                format=rv$figure_save_format(),
                width=rv$figure_save_width(), 
                height=rv$figure_save_height()
            ))
    })
    
    plot_functions$dendrogram_ref <- function() {
        plt <- do_dendrogram(
            ref_sdf(),
            rv$ddf_ref(rv, input$dataset1)[[ref_color()]],
            labels=rv$ddf_ref(rv, input$dataset1)[[ref_ddf_samplecol()]], 
            legend_title = ref_color(), 
            text_size=input$dendrogram_textsize
        ) + ggtitle(sprintf("Dataset: %s Color: %s", input$dataset1, ref_color()))
        
        if (input$custom_title1 != "") {
            plt <- plt + ggtitle(input$custom_title1)
        }
        
        plt
    }
    
    plot_functions$dendrogram_comp <- function() {
        plt <- do_dendrogram(
            comp_sdf(),
            rv$ddf_comp(rv, input$dataset2)[[comp_color()]],
            labels=rv$ddf_comp(rv, input$dataset2)[[comp_ddf_samplecol()]],
            legend_title = comp_color(),
            text_size=input$dendrogram_textsize
        ) + ggtitle(sprintf("Dataset: %s Color: %s", input$dataset2, comp_color()))
        if (input$custom_title2 != "") {
            plt <- plt + ggtitle(input$custom_title2)
        }
        
        plt
    }
    
    
    output$dendrogram_ref <- renderPlot({

        validate(need(rv$ddf_ref(rv, input$dataset1), "No design matrix found, please upload at the Setup page"))
        validate(need(reactive_long_sdf_ref(), "No data matrix found, please upload at the Setup page"))
        plot_functions$dendrogram_ref()
    })
    
    output$dendrogram_comp <- renderPlot({

        validate(need(rv$ddf_comp(rv, input$dataset2), "No design matrix found, please upload at the Setup page"))
        validate(need(reactive_long_sdf_comp(), "No data matrix found, please upload at the Setup page"))
        plot_functions$dendrogram_comp()
    })

    plot_functions$histogram_ref <- function() {
        if (input$data_num_col_ref != "None") {
            rdf_ref <- rv$rdf_ref(rv, input$dataset1)
            target_color <- NULL
            if (input$data_cat_col_ref != "None") {
                rdf_ref <- factor_prep_color_col(rdf_ref, input$data_cat_col_ref, input$max_color_cats, input$numeric_color_bins)
                target_color <- input$data_cat_col_ref
            }
            plt_ref <- ggplot(rdf_ref, aes_string(x=input$data_num_col_ref, fill=target_color)) + 
                geom_histogram(na.rm=TRUE, bins=input$hist_bins)
            
            if (input$custom_title1 == "") plot_title <- sprintf("Dataset: %s Column: %s Fill: %s", input$dataset1, input$data_num_col_ref, input$data_cat_col_ref)
            else plot_title <- input$custom_title1
            plt_ref <- plt_ref + ggtitle(plot_title)
        }
        else {
            plt_ref <- ggplot() + ggtitle("Empty histogram")
        }
        
        plt_ref + ylab("Count") + xlab(input$data_num_col_ref) + theme(text=element_text(size=input$text_size))
    }
    
    plot_functions$histogram_comp <- function() {
        
        if (input$data_num_col_comp != "None") {
            rdf_comp <- rv$rdf_comp(rv, input$dataset2)
            target_color <- NULL
            if (input$data_cat_col_comp != "None") {
                rdf_comp <- factor_prep_color_col(rdf_comp, input$data_cat_col_comp, input$max_color_cats, input$numeric_color_bins)
                target_color <- input$data_cat_col_comp
            }
            plt_comp <- ggplot(rdf_comp, aes_string(x=input$data_num_col_comp, fill=target_color)) + 
                geom_histogram(na.rm=TRUE, bins=input$hist_bins)
            
            if (input$custom_title2 == "") plot_title <- sprintf("Dataset: %s Column: %s Fill: %s", input$dataset2, input$data_num_col_comp, input$data_cat_col_comp)
            else plot_title <- input$custom_title2
            plt_ref <- plt_comp + ggtitle(plot_title)
        }
        else {
            plt_comp <- ggplot() + ggtitle("Empty histogram")
        }
        
        plt_comp + ylab("Count") + xlab(input$data_num_col_comp) + theme(text=element_text(size=input$text_size))
    }
    
    output$histogram_ref <- renderPlot({ 
        
        validate(need(rv$ddf_ref(rv, input$dataset1), "No design matrix found, please upload at the Setup page"))
        validate(need(reactive_long_sdf_ref(), "No data matrix found, please upload at the Setup page"))
        plot_functions$histogram_ref()
    })
    
    output$histogram_comp <- renderPlot({ 
        
        validate(need(rv$ddf_comp(rv, input$dataset2), "No design matrix found, please upload at the Setup page"))
        validate(need(reactive_long_sdf_comp(), "No data matrix found, please upload at the Setup page"))
        plot_functions$histogram_comp()
    })
}
