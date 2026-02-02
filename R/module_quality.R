setup_quality_ui <- function(id) {
    ns <- shiny::NS(id)
    tabPanel(
        id,
        fluidPage(
            bar_w_help_and_download("Quality", ns("help"), ns("download_settings"), ns("download_report")),
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
	                            sprintf("input['%s'] == 'Design matrix'", ns("plot_tabs")),
	                            fluidRow(
	                                column(6, selectInput(ns("design_num_col_ref"), "Design numeric column ref", choices=c("None"))),
	                                column(6, selectInput(ns("design_num_col_comp"), "Design numeric column comp", choices=c("None")))
	                            ),
	                            fluidRow(
	                                column(6, selectInput(ns("design_plot_type"), "Design plot", choices=c("Boxplot", "Density"), selected="Boxplot")),
	                                column(6, checkboxInput(ns("design_rotate_label"), "Rotate label", value=TRUE))
	                            )
	                        ),
		                        conditionalPanel(
		                            sprintf("input['%s'] == 'Dendrograms'", ns("plot_tabs")),
		                            numericInput(ns("dendrogram_height"), "Dendrogram plot height", value=500, min = 50, step = 50),
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
                            ),
                            fluidRow(
                                column(6, textInput(ns("custom_xlab"), "Custom x-label", value="")),
                                column(6, textInput(ns("custom_ylab"), "Custom y-label", value=""))
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
	                                     column(6, uiOutput(ns("dendrogram_ref_ui"))),
	                                     column(6, uiOutput(ns("dendrogram_comp_ui")))
	                                 )
	                        ),
	                        tabPanel("Histograms", 
	                                 plotOutput(ns("histogram_ref")) %>% withSpinner(),
	                                 plotOutput(ns("histogram_comp")) %>% withSpinner()
	                        ),
	                        tabPanel("Design matrix",
	                                 fluidRow(
	                                     column(6, plotlyOutput(ns("design_ref_plotly")) %>% withSpinner()),
	                                     column(6, plotlyOutput(ns("design_comp_plotly")) %>% withSpinner())
	                                 )
	                        )
	                    )
	                )
	            )
        )
    )
}

module_quality_server <- function(id, rv, module_name) {
    moduleServer(id, function(input, output, session) {

    observeEvent(input$help, {
        shinyalert(
            title = "Help: Quality visuals",
            text = help_quality, 
            html = TRUE
        )
    })
    
    output$download_settings <- settings_download_handler("quality", input)
    
    output$download_report <- report_generation_handler("quality", params=list(
            input=input,
            setup_input=rv$setup_input(),
            make_ref_barplot=make_ref_barplot,
            make_comp_barplot=make_comp_barplot,
            make_ref_boxplot=plot_functions$boxplot_ref,
            make_comp_boxplot=plot_functions$boxplot_comp,
            make_ref_density=make_ref_density,
            make_comp_density=make_comp_density,
            make_ref_dendrogram=plot_functions$dendrogram_ref,
            make_comp_dendrogram=plot_functions$dendrogram_comp
        ))
        
    # Observers
    observeEvent({
        rv$filedata_1()
        rv$filedata_2()}, {

        choices <- get_dataset_choices(rv)
        updateSelectInput(session, "dataset1", choices=choices, selected=choices[1])
        updateSelectInput(session, "dataset2", choices=choices, selected=choices[1])
    }, ignoreInit=TRUE, ignoreNULL=FALSE)
    
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
            sprintf('ref-%s-%s.%s', tolower(input$plot_tabs), format(Sys.time(), "%y%m%d_%H%M%S"), rv$figure_save_format())
        },
        content = function(file) {
            ggplot_download(file, "ref")
        }
    )
    
    output$ggplot_download_comp <- downloadHandler(
        filename = function() {
            sprintf('comp-%s-%s.%s', tolower(input$plot_tabs), format(Sys.time(), "%y%m%d_%H%M%S"), rv$figure_save_format())
        },
        content = function(file) {
            ggplot_download(file, "comp")
        }
    )
    
    sync_param_choices <- function() {
        
        shiny::validate(need(!is.null(rv$ddf_ref(rv, input$dataset1)), "Didn't find any design for dataset 1 while syncing input choices"))
        shiny::validate(need(!is.null(rv$ddf_comp(rv, input$dataset2)), "Didn't find any design for dataset 2 while syncing input choices"))
        
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

	        ddf_ref <- rv$ddf_ref(rv, input$dataset1)
	        ddf_comp <- rv$ddf_comp(rv, input$dataset2)
	        ref_numeric_cols <- names(ddf_ref)[vapply(ddf_ref, is.numeric, logical(1))]
	        comp_numeric_cols <- names(ddf_comp)[vapply(ddf_comp, is.numeric, logical(1))]
	        ref_design_num_choices <- c("None", ref_numeric_cols)
	        comp_design_num_choices <- c("None", comp_numeric_cols)
	        default_ref_design_num <- if (length(ref_design_num_choices) > 1) ref_design_num_choices[2] else "None"
	        default_comp_design_num <- if (length(comp_design_num_choices) > 1) comp_design_num_choices[2] else "None"
	        updateSelectInput(session, "design_num_col_ref", choices = ref_design_num_choices, selected=set_if_new(input$design_num_col_ref, ref_design_num_choices, default_ref_design_num))
	        updateSelectInput(session, "design_num_col_comp", choices = comp_design_num_choices, selected=set_if_new(input$design_num_col_comp, comp_design_num_choices, default_comp_design_num))
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
        
        shiny::validate(need(
            !is.null(rv$mapping_obj()[[sprintf("dataset%s", data_ind)]]), 
            "Did not find dataset mapping, have you loaded the data at the Setup page?"))
        shiny::validate(need(
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
        target_color <- ref_color()
        if (input$numeric_as_category && !is.null(target_color)) {
            long_df[[target_color]] <- as.factor(long_df[[target_color]])
        }
        long_df
    })
    
    reactive_long_sdf_comp <- reactive({
        long_df <- get_long(di_new(rv, input$dataset2, 2), rv, comp_ddf_samplecol())
        target_color <- comp_color()
        if (input$numeric_as_category && !is.null(target_color)) {
            long_df[[target_color]] <- as.factor(long_df[[target_color]])
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
    
    make_ref_barplot <- function() {
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
            text_size=input$text_size,
            xlab=input$custom_xlab,
            ylab=input$custom_ylab
        ) %>%
            plotly::ggplotly() %>% 
            assign_fig_settings(rv)
    }
    
    output$bars_ref <- renderPlotly({ 
        shiny::validate(need(rv$ddf_ref(rv, input$dataset1), "No design matrix found, please upload at the Setup page"))
        shiny::validate(need(reactive_long_sdf_ref(), "No data matrix found, please upload at the Setup page"))
        
        make_ref_barplot()
    })
    
    make_comp_barplot <- function() {
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
            text_size=input$text_size,
            xlab=input$custom_xlab,
            ylab=input$custom_ylab
        ) %>%
            plotly::ggplotly() %>% 
            assign_fig_settings(rv)
    }
    
    output$bars_comp <- renderPlotly({
        shiny::validate(need(rv$ddf_comp(rv, input$dataset2), "No design matrix found, please upload at the Setup page"))
        shiny::validate(need(reactive_long_sdf_comp(), "No data matrix found, please upload at the Setup page"))
        
        make_comp_barplot()
    })

    plot_functions <- list()
    plot_functions$boxplot_ref <- reactive({
        target_color <- ref_color()
        plt_ref <- ggplot(reactive_long_sdf_ref(), aes(x=.data[["name"]], y=.data[["value"]]))
        if (!is.null(target_color)) {
            plt_ref <- plt_ref + aes(color=.data[[target_color]])
        }
        
        if (input$custom_title1 == "") {
            if (!is.null(target_color)) {
                plt_ref <- plt_ref + ggtitle(sprintf("Dataset: %s Color: %s", input$dataset1, target_color))
            } else {
                plt_ref <- plt_ref + ggtitle(sprintf("Dataset: %s", input$dataset1))
            }
        } else {
            plt_ref <- plt_ref + ggtitle(input$custom_title1)
        }
        
        adjust_boxplot(
            plt_ref, 
            input$do_violin, 
            input$rotate_label, 
            input$order_on_cond,
            rv$ddf_ref(rv, input$dataset1),
            rv$ddf_samplecol_ref(rv, input$dataset1),
            input$color_data_ref,
            text_size=input$text_size,
            xlab=input$custom_xlab,
            ylab=input$custom_ylab
        )
    })
    
    plot_functions$boxplot_comp <- reactive({
        target_color <- comp_color()
        plt_comp <- ggplot(reactive_long_sdf_comp(), aes(x=.data[["name"]], y=.data[["value"]]))
        if (!is.null(target_color)) {
            plt_comp <- plt_comp + aes(color=.data[[target_color]])
        }
        
        if (input$custom_title2 == "") {
            if (!is.null(target_color)) {
                plt_comp <- plt_comp + ggtitle(sprintf("Dataset: %s Color: %s", input$dataset2, target_color))
            } else {
                plt_comp <- plt_comp + ggtitle(sprintf("Dataset: %s", input$dataset2))
            }
        } else {
            plt_comp <- plt_comp + ggtitle(input$custom_title2)
        }
        
        adjust_boxplot(
            plt_comp, 
            input$do_violin, 
            input$rotate_label, 
            input$order_on_cond,
            rv$ddf_comp(rv, input$dataset2),
            rv$ddf_samplecol_comp(rv, input$dataset2),
            input$color_data_comp,
            text_size=input$text_size,
            xlab=input$custom_xlab,
            ylab=input$custom_ylab
        )
    })
    
    output$boxplots_ref <- renderPlot({ 
        
        shiny::validate(need(rv$ddf_ref(rv, input$dataset1), "No design matrix found, please upload at the Setup page"))
        shiny::validate(need(reactive_long_sdf_ref(), "No data matrix found, please upload at the Setup page"))
        
        plot_functions$boxplot_ref()
    })

    output$boxplots_comp <- renderPlot({ 
        
        shiny::validate(need(rv$ddf_comp(rv, input$dataset2), "No design matrix found, please upload at the Setup page"))
        shiny::validate(need(reactive_long_sdf_comp(), "No data matrix found, please upload at the Setup page"))
        plot_functions$boxplot_comp()
    })
    
    make_ref_density <- function() {
        make_density_plot(
            reactive_long_sdf_ref(),
            ref_color(),
            curr_dataset=input$dataset1,
            title=input$custom_title1,
            text_size=input$text_size,
            xlab=input$custom_xlab,
            ylab=input$custom_ylab
        ) %>% assign_fig_settings(rv)
    }
    
    output$density_ref_plotly <- renderPlotly({
        
        shiny::validate(need(rv$ddf_ref(rv, input$dataset1), "No design matrix found, please upload at the Setup page"))
        shiny::validate(need(reactive_long_sdf_ref(), "No data matrix found, please upload at the Setup page"))
        
        make_ref_density()
    })
    
    make_comp_density <- function() {
        make_density_plot(
            reactive_long_sdf_comp(),
            comp_color(),
            curr_dataset=input$dataset2,
            title=input$custom_title2,
            text_size=input$text_size,
            xlab=input$custom_xlab,
            ylab=input$custom_ylab
        ) %>% plotly::config(toImageButtonOptions=list(
            format=rv$figure_save_format(),
            width=rv$figure_save_width(), 
            height=rv$figure_save_height()
        ))        
    }
    
    output$density_comp_plotly <- renderPlotly({

        shiny::validate(need(rv$ddf_comp(rv, input$dataset2), "No design matrix found, please upload at the Setup page"))
        shiny::validate(need(reactive_long_sdf_comp(), "No data matrix found, please upload at the Setup page"))
        
        make_comp_density()
    })
    
    plot_functions$dendrogram_ref <- function() {
        ddf <- rv$ddf_ref(rv, input$dataset1)
        sample_col <- ref_ddf_samplecol()
        samples <- colnames(ref_sdf())
        target_color <- ref_color()
        
        labels <- ddf[[sample_col]][match(samples, ddf[[sample_col]])]
        if (!is.null(target_color) && target_color %in% colnames(ddf)) {
            color_levels <- ddf[[target_color]]
            names(color_levels) <- ddf[[sample_col]]
            color_levels <- color_levels[samples]
            legend_title <- target_color
        } else {
            color_levels <- rep("None", length(samples))
            legend_title <- NULL
        }
        
        plt <- do_dendrogram(
            ref_sdf(),
            color_levels,
            labels = labels,
            legend_title = legend_title,
            text_size = input$dendrogram_textsize
        )
        if (!is.null(target_color)) {
            plt <- plt + ggtitle(sprintf("Dataset: %s Color: %s", input$dataset1, target_color))
        } else {
            plt <- plt + ggtitle(sprintf("Dataset: %s", input$dataset1))
        }
        
        if (input$custom_title1 != "") {
            plt <- plt + ggtitle(input$custom_title1)
        }
        
        plt
    }
    
	    plot_functions$dendrogram_comp <- function() {
        ddf <- rv$ddf_comp(rv, input$dataset2)
        sample_col <- comp_ddf_samplecol()
        samples <- colnames(comp_sdf())
        target_color <- comp_color()
        
        labels <- ddf[[sample_col]][match(samples, ddf[[sample_col]])]
        if (!is.null(target_color) && target_color %in% colnames(ddf)) {
            color_levels <- ddf[[target_color]]
            names(color_levels) <- ddf[[sample_col]]
            color_levels <- color_levels[samples]
            legend_title <- target_color
        } else {
            color_levels <- rep("None", length(samples))
            legend_title <- NULL
        }
        
        plt <- do_dendrogram(
            comp_sdf(),
            color_levels,
            labels = labels,
            legend_title = legend_title,
            text_size = input$dendrogram_textsize
        )
        if (!is.null(target_color)) {
            plt <- plt + ggtitle(sprintf("Dataset: %s Color: %s", input$dataset2, target_color))
        } else {
            plt <- plt + ggtitle(sprintf("Dataset: %s", input$dataset2))
        }
        if (input$custom_title2 != "") {
            plt <- plt + ggtitle(input$custom_title2)
        }
        
	        plt
	    }
	    
	    output$dendrogram_ref_ui <- renderUI({
	        height_px <- input$dendrogram_height
	        if (is.null(height_px) || is.na(height_px) || height_px < 50) {
	            height_px <- 500
	        }
	        plotOutput(session$ns("dendrogram_ref"), height = paste0(height_px, "px")) %>% withSpinner()
	    })

	    output$dendrogram_comp_ui <- renderUI({
	        height_px <- input$dendrogram_height
	        if (is.null(height_px) || is.na(height_px) || height_px < 50) {
	            height_px <- 500
	        }
	        plotOutput(session$ns("dendrogram_comp"), height = paste0(height_px, "px")) %>% withSpinner()
	    })
	    
	    
	    output$dendrogram_ref <- renderPlot({

	        shiny::validate(need(rv$ddf_ref(rv, input$dataset1), "No design matrix found, please upload at the Setup page"))
	        shiny::validate(need(reactive_long_sdf_ref(), "No data matrix found, please upload at the Setup page"))
        plot_functions$dendrogram_ref()
    })
    
    output$dendrogram_comp <- renderPlot({

        shiny::validate(need(rv$ddf_comp(rv, input$dataset2), "No design matrix found, please upload at the Setup page"))
        shiny::validate(need(reactive_long_sdf_comp(), "No data matrix found, please upload at the Setup page"))
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
            plt_ref <- ggplot(rdf_ref, aes(x=.data[[input$data_num_col_ref]]))
            if (!is.null(target_color)) {
                plt_ref <- plt_ref + aes(fill=.data[[target_color]])
            }
            plt_ref <- plt_ref + geom_histogram(na.rm=TRUE, bins=input$hist_bins)
            
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
            plt_comp <- ggplot(rdf_comp, aes(x=.data[[input$data_num_col_comp]]))
            if (!is.null(target_color)) {
                plt_comp <- plt_comp + aes(fill=.data[[target_color]])
            }
            plt_comp <- plt_comp + geom_histogram(na.rm=TRUE, bins=input$hist_bins)
            
            if (input$custom_title2 == "") plot_title <- sprintf("Dataset: %s Column: %s Fill: %s", input$dataset2, input$data_num_col_comp, input$data_cat_col_comp)
            else plot_title <- input$custom_title2
            plt_comp <- plt_comp + ggtitle(plot_title)
        }
        else {
            plt_comp <- ggplot() + ggtitle("Empty histogram")
        }
        
        plt_comp + ylab("Count") + xlab(input$data_num_col_comp) + theme(text=element_text(size=input$text_size))
    }
    
    output$histogram_ref <- renderPlot({ 
        
        shiny::validate(need(rv$ddf_ref(rv, input$dataset1), "No design matrix found, please upload at the Setup page"))
        shiny::validate(need(reactive_long_sdf_ref(), "No data matrix found, please upload at the Setup page"))
        plot_functions$histogram_ref()
    })
    
	    output$histogram_comp <- renderPlot({ 
	        
	        shiny::validate(need(rv$ddf_comp(rv, input$dataset2), "No design matrix found, please upload at the Setup page"))
	        shiny::validate(need(reactive_long_sdf_comp(), "No data matrix found, please upload at the Setup page"))
	        plot_functions$histogram_comp()
	    })

	    make_design_plot <- function(ddf, dataset_label, value_col, group_col, plot_type, custom_title, rotate_labels) {
	        shiny::validate(need(value_col %in% names(ddf), sprintf("Design column '%s' not found", value_col)))
	        shiny::validate(need(is.numeric(ddf[[value_col]]), sprintf("Design column '%s' must be numeric", value_col)))

	        plot_df <- ddf %>%
	            dplyr::mutate(
	                .value = .data[[value_col]]
	            )

	        if (!is.null(group_col) && group_col %in% names(plot_df)) {
	            group_vals <- plot_df[[group_col]]
	            if (is.numeric(group_vals)) {
	                uniq_count <- length(unique(group_vals[!is.na(group_vals)]))
	                if (uniq_count > 20) {
	                    group_vals <- cut(group_vals, 4)
	                }
	            }
	            plot_df$.group <- as.factor(group_vals)
	        } else {
	            plot_df$.group <- as.factor("All")
	        }

	        title <- custom_title
	        if (is.null(title) || title == "") {
	            if (!is.null(group_col) && group_col %in% names(plot_df) && !identical(plot_df$.group, factor("All"))) {
	                title <- sprintf("Design: %s - %s (group: %s)", dataset_label, value_col, group_col)
	            } else {
	                title <- sprintf("Design: %s - %s", dataset_label, value_col)
	            }
	        }

	        if (plot_type == "Boxplot") {
	            xlab <- if (input$custom_xlab != "") input$custom_xlab else if (!is.null(group_col) && group_col %in% names(plot_df)) group_col else "Group"
	            ylab <- if (input$custom_ylab != "") input$custom_ylab else value_col
	            plt <- ggplot(plot_df, aes(x = .data$.group, y = .data$.value, fill = .data$.group)) +
	                geom_boxplot(na.rm = TRUE) +
	                ggtitle(title) +
	                xlab(xlab) +
	                ylab(ylab) +
	                theme_bw() +
	                theme(text = element_text(size = input$text_size), legend.title = element_blank())
	            if (isTRUE(rotate_labels)) {
	                plt <- plt + theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
	            }
	            if (length(levels(plot_df$.group)) == 1 && identical(levels(plot_df$.group), "All")) {
	                plt <- plt + theme(legend.position = "none")
	            }
	        } else if (plot_type == "Density") {
	            xlab <- if (input$custom_xlab != "") input$custom_xlab else value_col
	            ylab <- if (input$custom_ylab != "") input$custom_ylab else "Density"
	            plt <- ggplot(plot_df, aes(x = .data$.value, color = .data$.group)) +
	                geom_density(na.rm = TRUE) +
	                ggtitle(title) +
	                xlab(xlab) +
	                ylab(ylab) +
	                theme_bw() +
	                theme(text = element_text(size = input$text_size), legend.title = element_blank())
	            if (length(levels(plot_df$.group)) == 1 && identical(levels(plot_df$.group), "All")) {
	                plt <- plt + theme(legend.position = "none")
	            }
	        } else {
	            stop(sprintf("Unknown design plot type: %s", plot_type))
	        }

	        plt %>% plotly::ggplotly() %>% assign_fig_settings(rv)
	    }

	    output$design_ref_plotly <- renderPlotly({
	        ddf <- rv$ddf_ref(rv, input$dataset1)
	        shiny::validate(need(!is.null(ddf), "No design matrix found, please upload at the Setup page"))
	        shiny::validate(need(!is.null(input$design_num_col_ref), "Select a numeric design column to plot"))

	        if (input$design_num_col_ref == "None") {
	            return(plotly::ggplotly(ggplot() + ggtitle("Select a numeric design column")) %>% assign_fig_settings(rv))
	        }

	        make_design_plot(
	            ddf = ddf,
	            dataset_label = input$dataset1,
	            value_col = input$design_num_col_ref,
	            group_col = ref_color(),
	            plot_type = input$design_plot_type,
	            custom_title = input$custom_title1,
	            rotate_labels = input$design_rotate_label
	        )
	    })

	    output$design_comp_plotly <- renderPlotly({
	        ddf <- rv$ddf_comp(rv, input$dataset2)
	        shiny::validate(need(!is.null(ddf), "No design matrix found, please upload at the Setup page"))
	        shiny::validate(need(!is.null(input$design_num_col_comp), "Select a numeric design column to plot"))

	        if (input$design_num_col_comp == "None") {
	            return(plotly::ggplotly(ggplot() + ggtitle("Select a numeric design column")) %>% assign_fig_settings(rv))
	        }

	        make_design_plot(
	            ddf = ddf,
	            dataset_label = input$dataset2,
	            value_col = input$design_num_col_comp,
	            group_col = comp_color(),
	            plot_type = input$design_plot_type,
	            custom_title = input$custom_title2,
	            rotate_labels = input$design_rotate_label
	        )
	    })
	    })
	}
