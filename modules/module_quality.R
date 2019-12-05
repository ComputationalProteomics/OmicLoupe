setup_quality_ui <- function(id) {
    ns <- NS(id)
    tabPanel(
        id,
        fluidPage(
            fluidRow(
                column(
                    4,
                    wellPanel(
                        selectInput(ns("dataset1"), "Reference dataset", choices = c(""), selected = ""),
                        selectInput(ns("dataset2"), "Comparison dataset", choices = c(""), selected = ""),
                        selectInput(ns("color_data1"), "Color", choices=c("")),
                        selectInput(ns("color_data2"), "Color", choices=c(""))
                    ),
                    wellPanel(
                        p("some text"),
                        conditionalPanel(
                            sprintf("input['%s'] == 'Boxplots'", ns("plot_tabs")),
                            checkboxInput(ns("do_violin"), "Do violin", value=FALSE)
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
                            selectInput(ns("data_num_col1"), "Histogram column ref", choices=c("")),
                            selectInput(ns("data_cat_col1"), "Fill column ref", choices=c("")),
                            selectInput(ns("data_num_col2"), "Histogram column comp", choices=c("")),
                            selectInput(ns("data_cat_col2"), "Fill column comp", choices=c("")),
                            numericInput(ns("max_color_cats"), "Maximum color cats.", min=1, step=1, value=5)
                        )
                    )
                ),
                column(
                    8,
                    tabsetPanel(
                        id = ns("plot_tabs"),
                        type = "tabs",
                        tabPanel("Boxplots", plotOutput(ns("boxplots"))),
                        tabPanel("Density", plotOutput(ns("density"))),
                        tabPanel("Barplots", plotOutput(ns("bars"))),
                        tabPanel("Histograms", plotOutput(ns("histograms")))
                    )
                )
            )
        )
    )
}

module_quality_server <- function(input, output, session, rv) {
    
    # Observers
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

    design_cols_ref <- reactive({
        colnames(design_ref())
    })
    
    design_cols_comp <- reactive({
        colnames(design_comp())
    })
    
    data_cols_ref <- reactive({
        colnames(data_ref())
    })
    
    data_cols_comp <- reactive({
        colnames(data_comp())
    })

    sync_param_choices <- function() {
        ref_choices <- c("None", design_cols_ref())
        comp_choices <- c("None", design_cols_comp())
        updateSelectInput(session, "color_data1", choices = ref_choices, selected=ref_choices[1])
        updateSelectInput(session, "sample_data1", choices = ref_choices, selected=ref_choices[1])
        updateSelectInput(session, "color_data2", choices = comp_choices, selected=comp_choices[1])
        updateSelectInput(session, "sample_data2", choices = comp_choices, selected=comp_choices[1])
        
        ref_data_choices <- c("None", data_cols_ref())
        comp_data_choices <- c("None", data_cols_comp())
        updateSelectInput(session, "data_num_col1", choices = ref_data_choices, selected=ref_data_choices[1])
        updateSelectInput(session, "data_cat_col1", choices = ref_data_choices, selected=ref_data_choices[1])
        updateSelectInput(session, "data_num_col2", choices = comp_data_choices, selected=comp_data_choices[1])
        updateSelectInput(session, "data_cat_col2", choices = comp_data_choices, selected=comp_data_choices[1])
    }
    
    observeEvent(design_ref(), {
        sync_param_choices()
    })
    
    observeEvent(design_comp(), {
        sync_param_choices()
    })
    
    # Reactivity
    design_ref <- reactive({ 
        if (!is.null(dataset_ind(rv, input, 1))) rv[[sprintf("design_%s", dataset_ind(rv, input, 1))]]()
        else NULL
    })
    design_comp <- reactive({ 
        if (!is.null(dataset_ind(rv, input, 2))) rv[[sprintf("design_%s", dataset_ind(rv, input, 2))]]() 
        else NULL
    })
    data_ref <- reactive({ 
        if (!is.null(dataset_ind(rv, input, 1))) rv[[sprintf("filedata_%s", dataset_ind(rv, input, 1))]]() 
        else NULL
    })
    data_comp <- reactive({ 
        if (!is.null(dataset_ind(rv, input, 2))) rv[[sprintf("filedata_%s", dataset_ind(rv, input, 2))]]() 
        else NULL
    })
    
    reactive_long_sdf1 <- reactive({
        dataset1 <- rv$mapping_obj()$dataset1
        sample_cols1 <- rv$mapping_obj()$samples1
        sdf1 <- dataset1[, sample_cols1]
        ddf1 <- rv$design_1()
        target_ddf_samplecol1 <- "old_sample"
        join_by <- c("name"=target_ddf_samplecol1)
        long_sdf1 <- sdf1 %>%
            pivot_longer(sample_cols1) %>%
            inner_join(ddf1, by=join_by)
        long_sdf1
    })
    
    # Illustrations
    ref_color <- reactive({
        if (input$color_data1 != "None") { input$color_data1 }
        else { NULL }
    })
    comp_color <- reactive({
        if (input$color_data2 != "None") { input$color_data2 }
        else { NULL }
    })
    
    output$bars <- renderPlot({ 
        
        target_ddf_samplecol1 <- "old_sample"
        join_by <- c("name"=target_ddf_samplecol1)
        ddf1 <- rv$design_1()
        
        long_sdf1 <- reactive_long_sdf1()

        if (!input$show_missing) {
            summed_data1 <- long_sdf1 %>% 
                group_by(name) %>%
                summarize_at("value", sum, na.rm=TRUE) %>% inner_join(ddf1, by=join_by)
        }
        else {
            summed_data1 <- long_sdf1 %>%
                mutate(value=is.na(value)) %>%
                group_by(name) %>%
                summarize_at("value", sum, na.rm=TRUE) %>% inner_join(ddf1, by=join_by)
        }
        
        plt1 <- ggplot(summed_data1, aes_string(x="name", y="value", fill=ref_color())) + geom_col() + ggtitle("Barplot 1")
        plt2 <- ggplot(summed_data1, aes_string(x="name", y="value", fill=comp_color())) + geom_col() + ggtitle("Barplot 2")
        
        return(ggarrange(plt1, plt2, nrow=2, ncol=1))
        
    })
    
    output$boxplots <- renderPlot({ 
        long_sdf1 <- reactive_long_sdf1()

        plt1 <- ggplot(long_sdf1, aes_string(x="name", y="value", color=ref_color())) + ggtitle("Boxplot 1")
        plt2 <- ggplot(long_sdf1, aes_string(x="name", y="value", color=comp_color())) + ggtitle("Boxplot 2")

        if (!input$do_violin) {
            target_geom <- geom_boxplot
        }
        else {
            target_geom <- geom_violin
        }

        plt1 <- plt1 + target_geom(na.rm=TRUE)
        plt2 <- plt2 + target_geom(na.rm=TRUE)
                
        ggarrange(plt1, plt2, nrow=2, ncol=1)
    })
    
    output$density <- renderPlot({ 
        long_sdf1 <- reactive_long_sdf1()

        plt1 <- ggplot(long_sdf1, aes_string(x="value", group="name", color=ref_color())) + 
            geom_density(na.rm=TRUE) + ggtitle("Density 1")
        plt2 <- ggplot(long_sdf1, aes_string(x="value", group="name", color=comp_color())) + 
            geom_density(na.rm=TRUE) + ggtitle("Density 2")
        
        ggarrange(plt1, plt2, nrow=2, ncol=1)
    })
    
    
    output$histograms <- renderPlot({ 
        
        rdf <- rv$mapping_obj()$dataset1
        
        adf_color_col <- input$data_cat_col1
        
        rdf[[adf_color_col]] <- as.factor(rdf[[adf_color_col]])
        
        color_freq_table <- table(rdf[[adf_color_col]])
        retain_count <- input$max_color_cats
        
        combine_names <- names(color_freq_table)[!names(color_freq_table) %in% names(sort(color_freq_table, decreasing = TRUE))[1:retain_count]]
        
        rdf[[adf_color_col]] <- rdf[[adf_color_col]] %>% fct_collapse(other=combine_names)
        
        plt1 <- ggplot(rdf, aes_string(x=input$data_num_col1, fill=adf_color_col)) + 
            geom_histogram(na.rm=TRUE, bins=50) + ggtitle("Histogram 1")
        plt2 <- ggplot(rdf, aes_string(x=input$data_num_col1, fill=adf_color_col)) + 
            geom_histogram(na.rm=TRUE, bins=50) + ggtitle("Histogram 2")
        
        ggarrange(plt1, plt2, nrow=2, ncol=1)    
    })
}


















