
library(ggplot2)
theme_set(theme_classic())
library(ggpubr)

source("R/vis_server_utils.R")
source("R/vis_server_plots.R")

MY_COLORS <- c("grey50", "blue", "red", "orange")

setup_plotly_ui <- function(id) {
    
    ns <- NS(id)
    tabPanel(
        id,
        fluidPage(
            fluidRow(
                column(4,
                       wellPanel(
                           selectInput(ns("color_type"), "Coloring type", choices=c("Threshold", "PCA", "Column"), selected="select"),
                           conditionalPanel(
                               sprintf("input['%s'] == 'PCA'", ns("color_type")),
                               fluidRow(
                                   column(4, numericInput(ns("plot_pc1"), "Plt1 PC", value=1, min=1, step=1)),
                                   column(4, numericInput(ns("plot_pc2"), "Plt2 PC", value=1, min=1, step=1)),
                                   column(4, actionButton(ns("color_pca"), "Plot"))
                               )
                           ),
                           conditionalPanel(
                               sprintf("input['%s'] == 'Column'", ns("color_type")),
                               fluidRow(
                                   column(6, selectInput(ns("color_col_1"), "Plt1 column", choices = c(""))),
                                   column(6, selectInput(ns("color_col_2"), "Plt2 column", choices = c("")))
                               )
                           ),
                           column(6,
                                  selectInput(ns("dataset1"), "Reference dataset", choices=c(), selected = ""),
                                  selectInput(ns("dataset2"), "Compare dataset", choices=c(), selected = "")
                           ),
                           column(6,
                                  selectInput(ns("stat_base1"), "Ref. Comparison", choices=c(), selected = ""),
                                  selectInput(ns("stat_base2"), "Comp. Comparison", choices=c(), selected = "")
                           ),
                           # selectInput(ns("reference_dataset"), "Reference dataset", choices=c(), selected = "dataset1"),
                           fluidRow(
                               column(9,
                                      sliderInput(ns("pvalue_cutoff"), "P-value cutoff", value=0.05, step=0.01, min=0, max=1)
                               ),
                               column(3,
                                      span(
                                          selectInput(ns("pvalue_type_select"), choices = c("P.Value", "adj.P.Val"), selected = "P.Value", label = "P-value type"),
                                          style="padding:20px"
                                      )
                               )
                           ),
                           sliderInput(ns("fold_cutoff"), "Fold cutoff", value=1, step=0.1, min=0, max=10),
                           sliderInput(ns("bin_count"), "Bin count", value=50, step=10, min=10, max=200),
                           checkboxInput(ns("toggle_minimaps"), "Display minimaps", value=FALSE)
                       )
                ),
                column(8,
                       p("Drag in figures to highlight features. Double click to unselect."),
                       column(6,
                              plotlyOutput(ns("plotly_volc1")),
                              plotlyOutput(ns("plotly_ma1")),
                              plotlyOutput(ns("plotly_hist1"))
                       ),
                       column(6,
                              plotlyOutput(ns("plotly_volc2")),
                              plotlyOutput(ns("plotly_ma2")),
                              plotlyOutput(ns("plotly_hist2"))
                       )
                )
            )
        )
    )
}


module_plotly_server <- function(input, output, session, reactive_vals) {
    
    dataset_ind <- function(field) {
        if (!is.null(reactive_vals$filename_1()) && input[[sprintf("dataset%s", field)]] == reactive_vals$filename_1()) {
            1
        }
        else if (!is.null(reactive_vals$filename_2()) && input[[sprintf("dataset%s", field)]] == reactive_vals$filename_2()) {
            2
        }
        else { 
            warning(sprintf("Unknown input$dataset_%s: ", field), input[[sprintf("dataset%s", field)]])
            NULL
        }
    }
    
    dataset_ref <- reactive({
        ind <- dataset_ind(1)
        reactive_vals[[sprintf("filedata_%s", ind)]]()
    })
    
    dataset_comp <- reactive({
        ind <- dataset_ind(2)
        reactive_vals[[sprintf("filedata_%s", ind)]]()
    })
    
    dataset_ref_cols <- reactive({
        
        ind <- dataset_ind(1)
        if (!is.null(ind)) {
            colnames(dataset_ref())
        }
        else {
            ""
        }
    })
    
    dataset_comp_cols <- reactive({
        ind <- dataset_ind(2)
        if (!is.null(ind)) {
            colnames(dataset_comp())
        }
        else {
            ""
        }
    })
    
    reactive_plot_df <- reactive({

        base_df <- get_pass_thres_annot_data(
            reactive_vals$mapping_obj()$get_combined_dataset(),
            reactive_ref_statcols(),
            reactive_comp_statcols(),
            input$pvalue_cutoff,
            input$fold_cutoff,
            input$pvalue_type_select
        )
        
        if (input$color_type == "Threshold") {
            base_df
            # get_pass_thres_annot_data(
            #     reactive_vals$mapping_obj()$get_combined_dataset(),
            #     reactive_ref_statcols(),
            #     reactive_comp_statcols(),
            #     input$pvalue_cutoff,
            #     input$fold_cutoff,
            #     input$pvalue_type_select
            # )
        }
        else if (input$color_type == "PCA") {
            
            ref_pca_df <- calculate_pca_obj(
                dataset_ref(),
                reactive_vals$selected_cols_obj()[[input$dataset1]]$samples,
                do_scale = TRUE,
                do_center = TRUE,
                var_cut = 0.4,
                return_df = TRUE
            )
            colnames(ref_pca_df) <- paste0("d1.", colnames(ref_pca_df))
            
            comp_pca_df <- calculate_pca_obj(
                dataset_comp(),
                reactive_vals$selected_cols_obj()[[input$dataset2]]$samples,
                do_scale = TRUE,
                do_center = TRUE,
                var_cut = 0.4,
                return_df = TRUE
            )
            colnames(comp_pca_df) <- paste0("d2.", colnames(comp_pca_df))
            
            pca_df <- cbind(ref_pca_df, comp_pca_df)
            warning("Temporary pass_threshold_data")
            pca_df$pass_threshold_data <- TRUE
        }
        else if (input$color_type == "Column") {
            # base_df <- get_pass_thres_annot_data(
            #     reactive_vals$mapping_obj()$get_combined_dataset(),
            #     reactive_ref_statcols(),
            #     reactive_comp_statcols(),
            #     input$pvalue_cutoff,
            #     input$fold_cutoff,
            #     input$pvalue_type_select
            # )
            base_df$d1.color_col <- dataset_ref()[[input$color_col_1]]
            base_df$d2.color_col <- dataset_comp()[[input$color_col_2]]
            base_df
        }
        else {
            warning("Unknown color_type !")
        }
    })
    
    get_plot_df <- function(target_statcols, feature_col="target_col1") {
        
        # browser()
        
        warning("Hover text is taken as d1.Protein, not the Setup input!")
        
        plot_df <- data.frame(
            fold = reactive_plot_df()[[target_statcols()$logFC]],
            sig = -log10(reactive_plot_df()[[target_statcols()$P.Value]]),
            lab = reactive_vals$mapping_obj()[[feature_col]],
            expr = reactive_plot_df()[[target_statcols()$AveExpr]],
            pval = reactive_plot_df()[[target_statcols()$P.Value]],
            pass_thres = reactive_plot_df()$pass_threshold_data
            # hover_text = paste0("ProteinID: ", reactive_plot_df()$d1.Protein)
        )
        
        if (input$color_type == "PCA") {
            plot_df$d1.PC <- reactive_plot_df()[[sprintf("d1.PC%s", input$plot_pc1)]]
            plot_df$d2.PC <- reactive_plot_df()[[sprintf("d2.PC%s", input$plot_pc2)]]
            warning("The pass_thres could be better calculated for histograms also in PCA")
            plot_df$pass_thres <- TRUE
        }
        
        if (input$color_type == "Column") {
            plot_df$d1.color_col <- reactive_plot_df()[["d1.color_col"]]
            plot_df$d2.color_col <- reactive_plot_df()[["d2.color_col"]]
        }
        
        plot_df$key <- row.names(plot_df)
        plot_df
    }
    
    plot_ref_df <- reactive({
        get_plot_df(reactive_ref_statcols)
    })
    
    plot_comp_df <- reactive({
        get_plot_df(reactive_comp_statcols)
    })
    
    reactive_ref_statcols <- reactive({
        if (input$dataset1 != "" && input$stat_base1 != "") {
            parse_stat_cols_for_visuals(
                reactive_vals$filename_1(), 
                reactive_vals$filename_2(), 
                reactive_vals$selected_cols_obj(), 
                input$dataset1, 
                input$stat_base1
            )
        }
    })
    
    reactive_comp_statcols <- reactive({
        if (input$dataset2 != "" && input$stat_base2 != "") {
            parse_stat_cols_for_visuals(
                reactive_vals$filename_1(), 
                reactive_vals$filename_2(), 
                reactive_vals$selected_cols_obj(), 
                input$dataset2, 
                input$stat_base2
            )
        }
    })
    

    
    observe(
        if (input$dataset1 != "" && input$stat_base1 != "") {
            reactive_comp_statcols()
        }
    )
    
    observeEvent(input$pvalue_type_select, {
        updateSliderInput(session, inputId="pvalue_cutoff", label=sprintf("%s cutoff", input$pvalue_type_select), 
                          value=input$pvalue_cutoff, min=0, max=1, step=0.01)
    })
    
    observeEvent({
        reactive_vals$selected_cols_obj() 
        input$dataset1 
        input$dataset2}, {
            update_stat_inputs(session, reactive_vals, input$dataset1, input$dataset2)
        })
    
    observeEvent(reactive_vals$filedata_1(), {
        choices <- get_dataset_choices(reactive_vals)
        updateSelectInput(session, "dataset1", choices=choices, selected=choices[1])
        updateSelectInput(session, "dataset2", choices=choices, selected=choices[1])
    })
    
    observeEvent(input$dataset1, {
        ref_data_cols <- dataset_ref_cols()
        comp_data_cols <- dataset_comp_cols()
        updateSelectInput(session, "color_col_1", choices=ref_data_cols, selected="pep_count")
        updateSelectInput(session, "color_col_2", choices=comp_data_cols, selected="pep_count")
    })
    
    observeEvent(reactive_vals$filedata_2(), {
        choices <- get_dataset_choices(reactive_vals)
        updateSelectInput(session, "dataset1", choices=choices, selected=choices[1])
        updateSelectInput(session, "dataset2", choices=choices, selected=choices[1])
    })
    
    ########## Plotly functions #############
    
    make_scatter <- function(plot_df, x_col, y_col, color_col, key, hover_text="hover_text", title="", manual_scale=TRUE) {
        
        max_levels <- 10
        df_color <- plot_df[[color_col]]
        if ((is.factor(df_color) || is.character(df_color)) && length(unique(df_color)) > max_levels) {
            warning("Maxlevels color color exceeded (", length(unique(df_color)), ") assigning no color")
            color_col <- NULL
        }
        
        plt <- ggplot(plot_df, aes_string(x=x_col, y=y_col, color=color_col, key=key, text=hover_text)) + 
            geom_point(alpha=0.5) + 
            ggtitle(title)
        if (manual_scale) {
            plt <- plt + scale_color_manual(values=MY_COLORS)
        }
        plt
    }
    
    make_histogram <- function(plot_df, x_col, fill_col, key_vals, title="") {
        plot_ly(
            plot_df,
            x = ~get(x_col),
            color = ~get(fill_col),
            type = "histogram", 
            colors = MY_COLORS, 
            alpha = 0.6,
            nbinsx = input$bin_count,
            source = "subset",
            key = key_vals
        ) %>% layout(title = title, xaxis=list(title="P.Value"))
    }
    
    output$plotly_volc1 <- renderPlotly({
        
        plot_df <- plot_ref_df()
        event.data <- event_data("plotly_selected", source = "subset")
        if (!is.null(event.data) == TRUE) {
            plot_df$selected <- row.names(plot_df) %in% event.data$key
            color_col <- "selected"
            manual_scale <- TRUE
        }
        else if(input$color_type == "Threshold") {
            color_col <- "pass_thres"
            manual_scale <- TRUE
        }
        else if (input$color_type == "PCA") {
            color_col <- "d1.PC"
            manual_scale <- FALSE
        }
        else if (input$color_type == "Column") {
            color_col <- "d1.color_col"
            manual_scale <- FALSE
        }
        else {
            warning("Unknown input$color_type: ", input$color_type)
        }
        
        make_scatter(
            plot_df, 
            x_col="fold", 
            y_col="sig", 
            color_col=color_col, 
            key="key", 
            title="Volcano: Reference dataset", 
            manual_scale = manual_scale) %>% 
                ggplotly(source="subset") %>%
                layout(dragmode="select") %>%
                toWebGL()
    })
    
    output$plotly_volc2 <- renderPlotly({
        
        plot_df <- plot_comp_df()
        event.data <- event_data("plotly_selected", source = "subset")
        
        if (!is.null(event.data) == TRUE) {
            plot_df$selected <- row.names(plot_df) %in% event.data$key
            color_col <- "selected"
            manual_scale <- TRUE
        }
        else if(input$color_type == "Threshold") {
            color_col <- "pass_thres"
            manual_scale <- TRUE
        }
        else if (input$color_type == "PCA") {
            color_col <- "d2.PC"
            manual_scale <- FALSE
        }
        else if (input$color_type == "Column") {
            color_col <- "d2.color_col"
            manual_scale <- FALSE
        }
        else {
            warning("Unknown input$color_type: ", input$color_type)
        }
        make_scatter(plot_df, x_col="fold", y_col="sig", color_col=color_col, key="key", 
                     title="Volcano: Compare dataset", manual_scale = manual_scale) %>% 
            ggplotly(source="subset") %>% 
            layout(dragmode="select") %>%
            toWebGL()
    })
    
    output$plotly_ma1 <- renderPlotly({
        
        plot_df <- plot_ref_df()
        event.data <- event_data("plotly_selected", source = "subset")
        if (!is.null(event.data) == TRUE) {
            plot_df$selected <- row.names(plot_df) %in% event.data$key
            color_col <- "selected"
            manual_scale <- TRUE
        }
        else if(input$color_type == "Threshold") {
            color_col <- "pass_thres"
            manual_scale <- TRUE
        }
        else if (input$color_type == "PCA") {
            color_col <- "d1.PC"
            manual_scale <- FALSE
        }
        else if (input$color_type == "Column") {
            color_col <- "d1.color_col"
            manual_scale <- FALSE
        }
        else {
            warning("Unknown input$color_type: ", input$color_type)
        }
        ggplt <- make_scatter(plot_df, x_col="expr", y_col="fold", color_col=color_col, 
                              key="key", title="MA: Reference dataset", manual_scale = manual_scale) %>% 
            ggplotly(source="subset") %>% 
            layout(dragmode="select") %>%
            toWebGL()
    })
    
    output$plotly_ma2 <- renderPlotly({
        
        plot_df <- plot_comp_df()
        event.data <- event_data("plotly_selected", source = "subset")
        
        if (!is.null(event.data) == TRUE) {
            plot_df$selected <- row.names(plot_df) %in% event.data$key
            color_col <- "selected"
            manual_scale <- TRUE
        }
        else if(input$color_type == "Threshold") {
            color_col <- "pass_thres"
            manual_scale <- TRUE
        }
        else if (input$color_type == "Column") {
            color_col <- "d2.color_col"
            manual_scale <- FALSE
        }
        else if (input$color_type == "PCA") {
            color_col <- "d2.PC"
            manual_scale <- FALSE
        }
        else {
            warning("Unknown input$color_type: ", input$color_type)
        }
        ggplt <- make_scatter(plot_df, x_col="expr", y_col="fold", color_col=color_col, key="key", 
                              title="MA: Compare dataset", manual_scale = manual_scale) %>% 
            ggplotly(source="subset") %>% 
            layout(dragmode="select") %>%
            toWebGL()
    })
    
    output$plotly_hist1 <- renderPlotly({
        
        plot_df <- plot_ref_df()
        event.data <- event_data("plotly_selected", source = "subset")
        
        if (!is.null(event.data) == TRUE) {
            plot_df$selected <- row.names(plot_df) %in% event.data$key
            color_col <- "selected"
        }
        else {
            color_col <- "pass_thres"
        }
        make_histogram(plot_df, x_col="pval", fill_col=color_col, key_vals=plot_df$key, title="P-value histogram: Reference dataset") %>% 
            layout(dragmode="none", barmode="stack") %>% 
            toWebGL()
    })
    
    output$plotly_hist2 <- renderPlotly({
        
        plot_df <- plot_comp_df()
        event.data <- event_data("plotly_selected", source = "subset")
        
        if (!is.null(event.data) == TRUE) {
            plot_df$selected <- row.names(plot_df) %in% event.data$key
            color_col <- "selected"
        }
        else {
            color_col <- "pass_thres"
        }
        make_histogram(plot_df, x_col="pval", fill_col=color_col, key_vals=plot_df$key, title="P-value histogram: Compare dataset") %>% 
            layout(dragmode="none", barmode="stack") %>% 
            toWebGL()
    })
    
}















