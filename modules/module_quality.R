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
                        selectInput(ns("color_data_ref"), "Color", choices=c("")),
                        selectInput(ns("color_data_comp"), "Color", choices=c(""))
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
                            selectInput(ns("data_num_col_ref"), "Histogram column ref", choices=c("")),
                            selectInput(ns("data_cat_col_ref"), "Fill column ref", choices=c("")),
                            selectInput(ns("data_num_col_comp"), "Histogram column comp", choices=c("")),
                            selectInput(ns("data_cat_col_comp"), "Fill column comp", choices=c("")),
                            numericInput(ns("max_color_cats"), "Maximum color cats.", min=1, step=1, value=5),
                            numericInput(ns("hist_bins"), "Histogram bins.", min=10, step=5, value=50)
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
        updateSelectInput(session, "color_data_ref", choices = ref_choices, selected=ref_choices[1])
        updateSelectInput(session, "sample_data1", choices = ref_choices, selected=ref_choices[1])
        updateSelectInput(session, "color_data_comp", choices = comp_choices, selected=comp_choices[1])
        updateSelectInput(session, "sample_data2", choices = comp_choices, selected=comp_choices[1])
        
        ref_data_choices <- c("None", data_cols_ref())
        comp_data_choices <- c("None", data_cols_comp())
        updateSelectInput(session, "data_num_col_ref", choices = ref_data_choices, selected=ref_data_choices[1])
        updateSelectInput(session, "data_cat_col_ref", choices = ref_data_choices, selected=ref_data_choices[1])
        updateSelectInput(session, "data_num_col_comp", choices = comp_data_choices, selected=comp_data_choices[1])
        updateSelectInput(session, "data_cat_col_comp", choices = comp_data_choices, selected=comp_data_choices[1])
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
    
    get_long <- function(data_ind, rv, ddf_samplecol) {
        
        dataset <- rv$mapping_obj()[[sprintf("dataset%s", data_ind)]]
        sample_cols <- rv$mapping_obj()[[sprintf("samples%s", data_ind)]]
        sdf <- dataset[, sample_cols]
        ddf <- rv[[sprintf("design_%s", data_ind)]]()
        
        join_by <- c("name"=ddf_samplecol)
        long_sdf <- sdf %>%
            pivot_longer(sample_cols) %>%
            inner_join(ddf, by=join_by)
        long_sdf
    }
    
    reactive_long_sdf_ref <- reactive({
        get_long(dataset_ind(rv, input, 1), rv, ref_ddf_samplecol())
    })
    
    reactive_long_sdf_comp <- reactive({
        get_long(dataset_ind(rv, input, 1), rv, ref_ddf_samplecol())
    })
    
    ref_ddf_samplecol <- reactive({
        rv[[sprintf("design_samplecol_%s", dataset_ind(rv, input, 1))]]()
    })
    
    comp_ddf_samplecol <- reactive({
        rv[[sprintf("design_samplecol_%s", dataset_ind(rv, input, 2))]]()
    })
    
    rdf_colorcol_ref <- reactive({
        rv[[sprintf("data_cat_col%s", dataset_ind(rv, input, 1))]]
    })
    
    rdf_colorcol_comp <- reactive({
        rv[[sprintf("data_cat_col%s", dataset_ind(rv, input, 2))]]
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
    
    output$bars <- renderPlot({ 
        
        # target_ddf_samplecol1 <- "old_sample"
        # target_ddf_samplecol1 <- ref_ddf_samplecol()
        
        join_by_ref <- c("name"=ref_ddf_samplecol())
        join_by_comp <- c("name"=comp_ddf_samplecol())
        
        ddf_ref <- design_ref()
        ddf_comp <- design_comp()
        
        long_sdf_ref <- reactive_long_sdf_ref()
        long_sdf_comp <- reactive_long_sdf_comp()
        
        if (!input$show_missing) {
            summed_data_ref <- long_sdf_ref %>% 
                group_by(name) %>%
                summarize_at("value", sum, na.rm=TRUE) %>% inner_join(ddf_ref, by=join_by_ref)
            summed_data_comp <- long_sdf_comp %>% 
                group_by(name) %>%
                summarize_at("value", sum, na.rm=TRUE) %>% inner_join(ddf_comp, by=join_by_ref)
        }
        else {
            summed_data_ref <- long_sdf_ref %>%
                mutate(value=is.na(value)) %>%
                group_by(name) %>%
                summarize_at("value", sum, na.rm=TRUE) %>% inner_join(ddf_ref, by=join_by_comp)
            summed_data_comp <- long_sdf_comp %>%
                mutate(value=is.na(value)) %>%
                group_by(name) %>%
                summarize_at("value", sum, na.rm=TRUE) %>% inner_join(ddf_comp, by=join_by_comp)
        }
        
        plt_ref <- ggplot(
            summed_data_ref, 
            aes_string(x="name", y="value", fill=ref_color())) + 
            geom_col() + 
            ggtitle("Barplot ref.")
        
        plt_comp <- ggplot(
            summed_data_comp, 
            aes_string(x="name", y="value", fill=comp_color())) + 
            geom_col() + 
            ggtitle("Barplot comp.")
        
        return(ggarrange(plt_ref, plt_comp, nrow=2, ncol=1))
        
    })
    
    output$boxplots <- renderPlot({ 
        
        plt_ref <- ggplot(
            reactive_long_sdf_ref(), 
            aes_string(x="name", y="value", color=ref_color())) + 
            ggtitle("Boxplot ref.")
        
        plt_comp <- ggplot(
            reactive_long_sdf_comp(), 
            aes_string(x="name", y="value", color=comp_color())) + 
            ggtitle("Boxplot comp.")
        
        if (!input$do_violin) {
            target_geom <- geom_boxplot
        }
        else {
            target_geom <- geom_violin
        }
        
        plt_ref <- plt_ref + target_geom(na.rm=TRUE)
        plt_comp <- plt_comp + target_geom(na.rm=TRUE)
        
        ggarrange(plt_ref, plt_comp, nrow=2, ncol=1)
    })
    
    output$density <- renderPlot({ 
        plt_ref <- ggplot(
            reactive_long_sdf_ref(), 
            aes_string(x="value", group="name", color=ref_color())) + 
            geom_density(na.rm=TRUE) + ggtitle("Density ref.")
        plt_comp <- ggplot(
            reactive_long_sdf_comp(), 
            aes_string(x="value", group="name", color=comp_color())) + 
            geom_density(na.rm=TRUE) + ggtitle("Density comp.")
        
        ggarrange(plt_ref, plt_comp, nrow=2, ncol=1)
    })
    
    factor_prep_color_col <- function(rdf, adf_color_col_ref, retain_count) {
        rdf[[adf_color_col_ref]] <- as.factor(rdf[[adf_color_col_ref]])
        color_freq_table <- table(rdf[[adf_color_col_ref]])
        combine_names <- names(color_freq_table)[!names(color_freq_table) %in% names(sort(color_freq_table, decreasing = TRUE))[1:retain_count]]
        rdf[[adf_color_col_ref]] <- rdf[[adf_color_col_ref]] %>% fct_collapse(other=combine_names)
        rdf
    }
    
    output$histograms <- renderPlot({ 

        if (input$data_num_col_ref != "None") {
            # browser()
            rdf_ref <- data_ref()
            target_color <- NULL
            if (input$data_cat_col_ref != "None") {
                rdf_ref <- factor_prep_color_col(rdf_ref, input$data_cat_col_ref, input$max_color_cats)
                target_color <- input$data_cat_col_ref
            }
            # ref_color <- data_color_ref(dataset_ind(rv, input, 1))
            plt_ref <- ggplot(rdf_ref, aes_string(x=input$data_num_col_ref, fill=target_color)) + 
                geom_histogram(na.rm=TRUE, bins=input$hist_bins) + ggtitle("Histogram ref.")
        }
        else {
            plt_ref <- ggplot() + ggtitle("Empty histogram ref.")
        }
        
        if (input$data_num_col_comp != "None") {
            rdf_comp <- data_comp()
            target_color <- NULL
            if (input$data_cat_col_comp != "None") {
                rdf_comp <- factor_prep_color_col(rdf_comp, input$data_cat_col_comp, input$max_color_cats)
                target_color <- input$data_cat_col_comp
            }
            
            # comp_color <- data_color_comp(dataset_ind(rv, input, 2))
            plt_comp <- ggplot(rdf_comp, aes_string(x=input$data_num_col_comp, fill=target_color)) + 
                geom_histogram(na.rm=TRUE, bins=input$hist_bins) + ggtitle("Histogram comp.")
        }
        else {
            plt_comp <- ggplot() + ggtitle("Empty histogram comp.")
        }
        
        ggarrange(plt_ref, plt_comp, nrow=2, ncol=1)    
    })
}


















