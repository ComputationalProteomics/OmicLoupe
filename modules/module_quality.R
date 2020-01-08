setup_quality_ui <- function(id) {
    ns <- NS(id)
    tabPanel(
        id,
        fluidPage(
            top_bar_w_help("Quality", ns("help")),
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
                                column(4, checkboxInput(ns("do_violin"), "Do violin", value=FALSE)),
                                column(4, checkboxInput(ns("rotate_label"), "Rotate label", value=TRUE)),
                                column(4, checkboxInput(ns("order_on_cond"), "Order on condition", value=FALSE))
                            )
                        ),
                        conditionalPanel(
                            sprintf("input['%s'] == 'Density'", ns("plot_tabs")),
                            p("No settings")
                        ),
                        conditionalPanel(
                            sprintf("input['%s'] == 'Barplots'", ns("plot_tabs")),
                            checkboxInput(ns("show_missing"), "Show missing", value=FALSE)
                        ),
                        conditionalPanel(
                            sprintf("input['%s'] == 'Histograms'", ns("plot_tabs")),
                            fluidRow(
                                column(6,
                                       selectInput(ns("data_num_col_ref"), "Histogram column ref", choices=c("")),
                                       selectInput(ns("data_cat_col_ref"), "Fill column ref", choices=c("")),
                                       numericInput(ns("max_color_cats"), "Maximum color cats.", min=1, step=1, value=5)
                                ),
                                column(6,
                                       selectInput(ns("data_num_col_comp"), "Histogram column comp", choices=c("")),
                                       selectInput(ns("data_cat_col_comp"), "Fill column comp", choices=c("")),
                                       numericInput(ns("hist_bins"), "Histogram bins.", min=10, step=5, value=50)
                                )
                            )
                        )
                    ),
                    htmlOutput(ns("warnings")),
                    tabsetPanel(
                        id = ns("plot_tabs"),
                        type = "tabs",
                        tabPanel("Boxplots", 
                                 plotOutput(ns("boxplots_ref")) %>% withSpinner(),
                                 plotOutput(ns("boxplots_comp")) %>% withSpinner()
                        ),
                        tabPanel("Density", 
                                 plotOutput(ns("density_ref")) %>% withSpinner(),
                                 plotOutput(ns("density_comp")) %>% withSpinner()
                        ),
                        tabPanel("Barplots", 
                                 plotOutput(ns("bars_ref")) %>% withSpinner(),
                                 plotOutput(ns("bars_comp")) %>% withSpinner()
                        ),
                        tabPanel("Histograms", 
                                 plotOutput(ns("histograms_ref")) %>% withSpinner(),
                                 plotOutput(ns("histograms_comp")) %>% withSpinner()
                        )
                    )
                )
            )
        )
    )
}

module_quality_server <- function(input, output, session, rv, module_name) {
    
    # Observers
    observeEvent(rv$filedata_1(), {
        message("observeEvent(rv$filedata_1()")
        choices <- get_dataset_choices(rv)
        updateSelectInput(session, "dataset1", choices=choices, selected=choices[1])
        updateSelectInput(session, "dataset2", choices=choices, selected=choices[1])
    })
    
    observeEvent(rv$filedata_2(), {
        message("observeEvent(rv$filedata_2()")
        choices <- get_dataset_choices(rv)
        updateSelectInput(session, "dataset1", choices=choices, selected=choices[1])
        updateSelectInput(session, "dataset2", choices=choices, selected=choices[1])
    })
    
    sync_param_choices <- function() {
        
        req(rv$ddf_ref(rv, input$dataset1))
        req(rv$ddf_comp(rv, input$dataset2))

        ref_choices <- c("None", rv$ddf_cols_ref(rv, input$dataset1))
        comp_choices <- c("None", rv$ddf_cols_comp(rv, input$dataset2))
        
        updateSelectInput(session, "color_data_ref", choices = ref_choices, selected=rv$ddf_condcol_ref(rv, input$dataset1))
        updateSelectInput(session, "color_data_comp", choices = comp_choices, selected=rv$ddf_condcol_ref(rv, input$dataset2))
        
        updateSelectInput(session, "sample_data1", choices = ref_choices, selected=ref_choices[1])
        updateSelectInput(session, "sample_data2", choices = comp_choices, selected=comp_choices[1])
        
        ref_data_choices <- c("None", rv$rdf_cols_ref(rv, input$dataset1))
        comp_data_choices <- c("None", rv$rdf_cols_comp(rv, input$dataset2))
        updateSelectInput(session, "data_num_col_ref", choices = ref_data_choices, selected=ref_data_choices[1])
        updateSelectInput(session, "data_cat_col_ref", choices = ref_data_choices, selected=ref_data_choices[1])
        updateSelectInput(session, "data_num_col_comp", choices = comp_data_choices, selected=comp_data_choices[1])
        updateSelectInput(session, "data_cat_col_comp", choices = comp_data_choices, selected=comp_data_choices[1])
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
        
        if (is.null(rv$mapping_obj()[[sprintf("dataset%s", data_ind)]])) {
            stop("Datasets not properly mapped, stopping")
        }
        
        if (is.null(rv$mapping_obj()[[sprintf("samples%s", data_ind)]])) {
            stop("Samples not properly mapped, stopping")
        }
        
        dataset <- rv$mapping_obj()[[sprintf("dataset%s", data_ind)]]
        sample_cols <- rv$mapping_obj()[[sprintf("samples%s", data_ind)]]
        sdf <- dataset[, sample_cols]
        ddf <- rv[[sprintf("design_%s", data_ind)]]()
        ddf$name <- ddf[[ddf_samplecol]]
        
        # join_by <- c("name"=ddf_samplecol)
        long_sdf <- sdf %>%
            pivot_longer(sample_cols) %>%
            inner_join(ddf, by="name")
        long_sdf
    }
    
    reactive_long_sdf_ref <- reactive({
        get_long(di_new(rv, input$dataset1, 1), rv, ref_ddf_samplecol())
    })
    
    reactive_long_sdf_comp <- reactive({
        get_long(di_new(rv, input$dataset2, 2), rv, comp_ddf_samplecol())
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
        if (input$color_data_comp != "None") { input$color_data_comp }
        else { NULL }
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
    
    output$bars_ref <- renderPlot({ 
        
        req(rv$ddf_ref(rv, input$dataset1))
        req(reactive_long_sdf_ref())
        
        join_by_ref <- c("name"=ref_ddf_samplecol())
        ddf_ref <- rv$ddf_ref(rv, input$dataset1)
        long_sdf_ref <- reactive_long_sdf_ref()
        
        if (!input$show_missing) {
            summed_data_ref <- long_sdf_ref %>% 
                filter(!is.infinite(long_sdf_ref$value)) %>%
                group_by(name) %>%
                summarize_at("value", sum, na.rm=TRUE) %>% inner_join(ddf_ref, by=join_by_ref)
        }
        else {
            summed_data_ref <- long_sdf_ref %>%
                filter(!is.infinite(long_sdf_ref$value)) %>%
                mutate(value=is.na(value)) %>%
                group_by(name) %>%
                summarize_at("value", sum, na.rm=TRUE) %>% inner_join(ddf_ref, by=join_by_ref)
        }
        
        plt_ref <- ggplot(
            summed_data_ref, 
            aes_string(x="name", y="value", fill=ref_color())) + 
            geom_col() + 
            ggtitle("Barplot ref.")
        
        plt_ref
    })
    
    output$bars_comp <- renderPlot({
        req(rv$ddf_comp(rv, input$dataset2))
        req(reactive_long_sdf_comp())
        
        join_by_comp <- c("name"=comp_ddf_samplecol())
        ddf_comp <- rv$ddf_comp(rv, input$dataset2)
        long_sdf_comp <- reactive_long_sdf_comp()
        
        if (!input$show_missing) {
            summed_data_comp <- long_sdf_comp %>% 
                filter(!is.infinite(long_sdf_comp$value)) %>%
                group_by(name) %>%
                summarize_at("value", sum, na.rm=TRUE) %>% inner_join(ddf_comp, by=join_by_comp)
        }
        else {
            summed_data_comp <- long_sdf_comp %>%
                filter(!is.infinite(long_sdf_comp$value)) %>%
                mutate(value=is.na(value)) %>%
                group_by(name) %>%
                summarize_at("value", sum, na.rm=TRUE) %>% inner_join(ddf_comp, by=join_by_comp)
        }
        
        plt_comp <- ggplot(
            summed_data_comp, 
            aes_string(x="name", y="value", fill=comp_color())) + 
            geom_col() + 
            ggtitle("Barplot comp.")
        
        plt_comp
    })

    adjust_boxplot <- function(plt, do_violin, rotate_x_labels, order_on_condition, ddf, ddf_sample_col, ddf_cond_col) {
        
        if (!do_violin) {
            target_geom <- geom_boxplot
        }
        else {
            target_geom <- geom_violin
        }
        
        if (rotate_x_labels) {
            plt <- plt + theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
        }
        
        if (order_on_condition) {
            plt <- plt + scale_x_discrete(
                limits=ddf %>% 
                arrange(UQ(as.name(ddf_cond_col))) %>% 
                dplyr::select(ddf_sample_col) %>% 
                unlist()
            )
        }
        
        plt <- plt + target_geom(na.rm=TRUE)
        plt
    }
    
    output$boxplots_ref <- renderPlot({ 
        
        # browser()
        
        req(rv$ddf_ref(rv, input$dataset1))
        req(reactive_long_sdf_ref())
        
        plt_ref <- ggplot(
            reactive_long_sdf_ref(), 
            aes_string(x="name", y="value", color=ref_color())) + 
            ggtitle("Boxplot ref.")

        adjust_boxplot(
            plt_ref, 
            input$do_violin, 
            input$rotate_label, 
            input$order_on_cond,
            rv$ddf_ref(rv, input$dataset1),
            rv$ddf_samplecol_ref(rv, input$dataset1),
            input$color_data_ref
            # rv$ddf_condcol_ref(rv, input$dataset1)
        )
    })

    output$boxplots_comp <- renderPlot({ 
        
        req(rv$ddf_comp(rv, input$dataset2))
        req(reactive_long_sdf_comp())
        
        plt_comp <- ggplot(
            reactive_long_sdf_comp(), 
            aes_string(x="name", y="value", color=comp_color())) + 
            ggtitle("Boxplot comp.")

        adjust_boxplot(
            plt_comp, 
            input$do_violin, 
            input$rotate_label, 
            input$order_on_cond,
            rv$ddf_comp(rv, input$dataset2),
            rv$ddf_samplecol_comp(rv, input$dataset2),
            input$color_data_comp
            # rv$ddf_condcol_comp(rv, input$dataset2)
        )
    })
    
    output$density_ref <- renderPlot({ 
        req(rv$ddf_ref(rv, input$dataset1))
        req(reactive_long_sdf_ref())
        
        plt_ref <- ggplot(
            reactive_long_sdf_ref(), 
            aes_string(x="value", group="name", color=ref_color())) + 
            geom_density(na.rm=TRUE) + 
            ggtitle("Density ref.")

        plt_ref
    })
    
    output$density_comp <- renderPlot({ 
        req(rv$ddf_comp(rv, input$dataset2))
        req(reactive_long_sdf_comp())
        
        plt_comp <- ggplot(
            reactive_long_sdf_comp(), 
            aes_string(x="value", group="name", color=comp_color())) + 
            geom_density(na.rm=TRUE) + 
            ggtitle("Density comp.")
        plt_comp
    })
    
    factor_prep_color_col <- function(rdf, adf_color_col_ref, retain_count) {
        rdf[[adf_color_col_ref]] <- as.factor(rdf[[adf_color_col_ref]])
        color_freq_table <- table(rdf[[adf_color_col_ref]])
        combine_names <- names(color_freq_table)[!names(color_freq_table) %in% names(sort(color_freq_table, decreasing = TRUE))[1:retain_count]]
        rdf[[adf_color_col_ref]] <- rdf[[adf_color_col_ref]] %>% fct_collapse(other=combine_names)
        rdf
    }
    
    output$histograms_ref <- renderPlot({ 
        
        req(rv$ddf_ref(rv, input$dataset1))
        req(reactive_long_sdf_ref())
        
        if (input$data_num_col_ref != "None") {
            rdf_ref <- rv$rdf_ref(rv, input$dataset1)
            target_color <- NULL
            if (input$data_cat_col_ref != "None") {
                rdf_ref <- factor_prep_color_col(rdf_ref, input$data_cat_col_ref, input$max_color_cats)
                target_color <- input$data_cat_col_ref
            }
            plt_ref <- ggplot(rdf_ref, aes_string(x=input$data_num_col_ref, fill=target_color)) + 
                geom_histogram(na.rm=TRUE, bins=input$hist_bins) + ggtitle("Histogram ref.")
        }
        else {
            plt_ref <- ggplot() + ggtitle("Empty histogram ref.")
        }

        plt_ref
    })
    
    output$histograms_comp <- renderPlot({ 
        
        req(rv$ddf_ref(rv, input$dataset2))
        req(reactive_long_sdf_comp())
        
        if (input$data_num_col_comp != "None") {
            rdf_comp <- rv$rdf_comp(rv, input$dataset2)
            target_color <- NULL
            if (input$data_cat_col_comp != "None") {
                rdf_comp <- factor_prep_color_col(rdf_comp, input$data_cat_col_comp, input$max_color_cats)
                target_color <- input$data_cat_col_comp
            }
            plt_comp <- ggplot(rdf_comp, aes_string(x=input$data_num_col_comp, fill=target_color)) + 
                geom_histogram(na.rm=TRUE, bins=input$hist_bins) + ggtitle("Histogram comp.")
        }
        else {
            plt_comp <- ggplot() + ggtitle("Empty histogram comp.")
        }
        
        plt_comp
    })
}


















