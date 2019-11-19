
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
                           sliderInput(ns("alpha"), "Alpha (0 - 1)", value=0.4, step=0.01, min=0, max=1)
                           # checkboxInput(ns("toggle_minimaps"), "Display minimaps", value=FALSE)
                       )
                ),
                column(8,
                       htmlOutput(ns("warnings")),
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
            ),
            DT::DTOutput(ns("target_data_dt"))
        )
    )
}


module_plotly_server <- function(input, output, session, reactive_vals) {
    
    # ---------------- REACTIVE ---------------- 
    
    dataset_ref <- reactive({
        reactive_vals[[sprintf("filedata_%s", dataset_ind(1))]]() 
    })
    dataset_comp <- reactive({ reactive_vals[[sprintf("filedata_%s", dataset_ind(2))]]() })
    samples_ref <- reactive({
        ind <- dataset_ind(1)
        paste0(
            sprintf("d%s.", ind),
            reactive_vals$selected_cols_obj()[[input[[sprintf("dataset%s", ind)]]]]$samples
        )
    })
    samples_comp <- reactive({
        ind <- dataset_ind(2)
        paste0(
            sprintf("d%s.", ind),
            reactive_vals$selected_cols_obj()[[input[[sprintf("dataset%s", ind)]]]]$samples
        )
    })

    design_ref <- reactive({ reactive_vals[[sprintf("design_%s", dataset_ind(1))]]() })
    design_comp <- reactive({ reactive_vals[[sprintf("design_%s", dataset_ind(2))]]() })
    
    dataset_ref_cols <- reactive({
        req(dataset_ref())
        colnames(dataset_ref())
    })
    
    dataset_comp_cols <- reactive({
        req(dataset_comp())
        colnames(dataset_comp())
    })
    
    reactive_plot_df <- reactive({
        
        req(reactive_ref_statcols())
        req(reactive_comp_statcols())
        
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
        }
        else if (input$color_type == "PCA") {
            
            req(samples_ref())
            req(samples_comp())
            
            warning("Should PCA parameters be linked to PCA page?")
            ref_pca_df <- calculate_pca_obj(
                base_df,
                samples_ref(),
                do_scale = TRUE,
                do_center = TRUE,
                var_cut = 0.4,
                return_df = TRUE,
                col_prefix="ref."
            )

            comp_pca_df <- calculate_pca_obj(
                base_df,
                samples_comp(),
                do_scale = TRUE,
                do_center = TRUE,
                var_cut = 0.4,
                return_df = TRUE,
                col_prefix="comp."
            )

            pca_df <- cbind(ref_pca_df, comp_pca_df)
            pca_df
        }
        else if (input$color_type == "Column") {
            base_df$ref.color_col <- dataset_ref()[[input$color_col_1]]
            base_df$comp.color_col <- dataset_comp()[[input$color_col_2]]
            browser()
            base_df
        }
        else {
            warning("Unknown color_type !")
        }
    })
    
    plot_ref_df <- reactive({
        get_plot_df(reactive_ref_statcols)
    })
    
    plot_comp_df <- reactive({
        get_plot_df(reactive_comp_statcols)
    })
    
    reactive_ref_statcols <- reactive({
        warning("Should the if statement be removed?")
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
        warning("Should the if statement be removed?")
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
    
    # ---------------- OBSERVERS ---------------- 
    
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
        updateSelectInput(session, "color_col_1", choices=ref_data_cols, selected=ref_data_cols[1])
        updateSelectInput(session, "color_col_2", choices=comp_data_cols, selected=comp_data_cols[1])
    })
    
    observeEvent(reactive_vals$filedata_2(), {
        choices <- get_dataset_choices(reactive_vals)
        updateSelectInput(session, "dataset1", choices=choices, selected=choices[1])
        updateSelectInput(session, "dataset2", choices=choices, selected=choices[1])
    })
    
    # ---------------- FUNCTIONS ---------------- 
    
    dataset_ind <- function(field) {
        
        req(reactive_vals$filename_1())
        
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
    
    get_plot_df <- function(target_statcols, feature_col="target_col1") {
        
        plot_df <- data.frame(
            fold = reactive_plot_df()[[target_statcols()$logFC]],
            sig = -log10(reactive_plot_df()[[target_statcols()$P.Value]]),
            lab = reactive_vals$mapping_obj()[[feature_col]],
            expr = reactive_plot_df()[[target_statcols()$AveExpr]],
            pval = reactive_plot_df()[[target_statcols()$P.Value]],
            pass_thres = reactive_plot_df()$pass_threshold_data,
            hover_text = reactive_plot_df()$comb_id,
            key = reactive_plot_df()$comb_id
        )
        
        if (input$color_type == "PCA") {
            # browser()
            plot_df$ref.PC <- reactive_plot_df()[[sprintf("%s.PC%s", "ref", input$plot_pc1)]]
            plot_df$comp.PC <- reactive_plot_df()[[sprintf("%s.PC%s", "comp", input$plot_pc2)]]
            warning("The pass_thres could be better calculated for histograms also in PCA")
            plot_df$pass_thres <- TRUE
        }
        
        if (input$color_type == "Column") {
            browser()
            plot_df$ref.color_col <- reactive_plot_df()[[sprintf("%s.color_col", "ref")]]
            plot_df$comp.color_col <- reactive_plot_df()[[sprintf("%s.color_col", "comp")]]
        }
        
        plot_df
    }
    
    make_scatter <- function(plot_df, x_col, y_col, color_col, key, hover_text="hover_text", title="", manual_scale=TRUE, alpha=0.5) {
        
        max_levels <- 10
        df_color <- plot_df[[color_col]]
        if ((is.factor(df_color) || is.character(df_color)) && length(unique(df_color)) > max_levels) {
            warning("Maxlevels color color exceeded (", length(unique(df_color)), ") assigning no color")
            color_col <- NULL
        }
        
        plt <- ggplot(plot_df, aes_string(x=x_col, y=y_col, color=color_col, key=key, text=hover_text)) + 
            geom_point(alpha=alpha) + 
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
    
    retrieve_color_col <- function(color_type, data_pattern) {
        if(color_type == "Threshold") {
            color_col <- "pass_thres"
        }
        else if (color_type == "PCA") {
            color_col <- sprintf("%s.PC", data_pattern)
        }
        else if (color_type == "Column") {
            color_col <- sprintf("%s.color_col", data_pattern)
        }
        else {
            warning("Unknown input$color_type: ", input$color_type)
        }
        color_col
    }
    
    # ---------------- OUTPUTS ---------------- 
    
    output$warnings <- renderUI({
        
        error_vect <- c()
        if (is.null(reactive_vals$filename_1())) {
            error_vect <- c(error_vect, "No filename_1 found, upload dataset at Setup page")
        }
        
        if (is.null(reactive_vals$design_1()) && input$color_type == "PCA") {
            error_vect <- c(error_vect, "No design_1 found, upload dataset at Setup page")
        }
        
        if (is.null(reactive_ref_statcols())) {
            error_vect <- c(error_vect, "No ref_statcols found, assign stat-columns at Setup page")
        }
        
        if ((is.null(samples_ref()) || is.null(samples_comp())) && input$color_type == "PCA") {
            error_vect <- c(error_vect, "No mapped samples found needed for PCA, map at Setup page")
        }
        
        total_text <- paste(error_vect, collapse="<br>")
        HTML(sprintf("<b><font size='5' color='red'>%s</font></b>", total_text))
    })
    
    output$plotly_volc1 <- renderPlotly({
        
        plot_df <- plot_ref_df()
        event.data <- event_data("plotly_selected", source = "subset")
        manual_scale <- TRUE
        if (!is.null(event.data) == TRUE) {
            plot_df$selected <- plot_df$key %in% event.data$key
            color_col <- "selected"
        } 
        else {
            color_col <- retrieve_color_col(input$color_type, "ref")
            if (input$color_type %in% c("PCA", "Column")) {
                manual_scale <- FALSE
            }
        }

        make_scatter(
            plot_df, 
            x_col="fold", 
            y_col="sig", 
            color_col=color_col, 
            key="key", 
            alpha=input$alpha,
            title="Volcano: Reference dataset", 
            manual_scale = manual_scale) %>% 
                ggplotly(source="subset") %>%
                layout(dragmode="select") %>%
                toWebGL()
    })
    
    output$plotly_volc2 <- renderPlotly({
        
        plot_df <- plot_comp_df()
        event.data <- event_data("plotly_selected", source = "subset")
        manual_scale <- TRUE
        if (!is.null(event.data) == TRUE) {
            plot_df$selected <- plot_df$key %in% event.data$key
            color_col <- "selected"
        } 
        else {
            color_col <- retrieve_color_col(input$color_type, "comp")
            if (input$color_type %in% c("PCA", "Column")) {
                manual_scale <- FALSE
            }
        }
        
        make_scatter(
            plot_df, 
            x_col="fold", 
            y_col="sig", 
            color_col=color_col, 
            key="key", 
            alpha=input$alpha,
            title="Volcano: Compare dataset", 
            manual_scale = manual_scale) %>% 
                ggplotly(source="subset") %>% 
                layout(dragmode="select") %>%
                toWebGL()
    })
    
    output$plotly_ma1 <- renderPlotly({
        
        plot_df <- plot_ref_df()
        event.data <- event_data("plotly_selected", source = "subset")
        manual_scale <- TRUE
        if (!is.null(event.data) == TRUE) {
            plot_df$selected <- plot_df$key %in% event.data$key
            color_col <- "selected"
        } 
        else {
            color_col <- retrieve_color_col(input$color_type, "ref")
            if (input$color_type %in% c("PCA", "Column")) {
                manual_scale <- FALSE
            }
        }
        ggplt <- make_scatter(
            plot_df, 
            x_col="expr", 
            y_col="fold", 
            color_col=color_col, 
            alpha=input$alpha,
            key="key", 
            title="MA: Reference dataset", 
            manual_scale = manual_scale) %>% 
                ggplotly(source="subset") %>% 
                layout(dragmode="select") %>%
                toWebGL()
    })
    
    output$plotly_ma2 <- renderPlotly({
        
        plot_df <- plot_comp_df()
        event.data <- event_data("plotly_selected", source = "subset")
        manual_scale <- TRUE
        if (!is.null(event.data) == TRUE) {
            plot_df$selected <- plot_df$key %in% event.data$key
            color_col <- "selected"
        } 
        else {
            color_col <- retrieve_color_col(input$color_type, "comp")
            if (input$color_type %in% c("PCA", "Column")) {
                manual_scale <- FALSE
            }
        }
        ggplt <- make_scatter(
            plot_df, 
            x_col="expr", 
            y_col="fold", 
            color_col=color_col, 
            alpha=input$alpha,
            key="key", 
            title="MA: Compare dataset", 
            manual_scale = manual_scale) %>% 
                ggplotly(source="subset") %>% 
                layout(dragmode="select") %>%
                toWebGL()
    })
    
    output$plotly_hist1 <- renderPlotly({
        
        plot_df <- plot_ref_df()
        event.data <- event_data("plotly_selected", source = "subset")
        
        if (!is.null(event.data) == TRUE) {
            plot_df$selected <- plot_df$key %in% event.data$key
            color_col <- "selected"
        }
        else {
            color_col <- "pass_thres"
        }
        make_histogram(
            plot_df, 
            x_col="pval", 
            fill_col=color_col, 
            key_vals=plot_df$key,
            title="P-value histogram: Reference dataset") %>% 
                layout(dragmode="none", barmode="stack") %>% 
                toWebGL()
    })
    
    output$plotly_hist2 <- renderPlotly({
        
        plot_df <- plot_comp_df()
        event.data <- event_data("plotly_selected", source = "subset")
        
        if (!is.null(event.data) == TRUE) {
            plot_df$selected <- plot_df$key %in% event.data$key
            color_col <- "selected"
        }
        else {
            color_col <- "pass_thres"
        }
        make_histogram(
            plot_df, 
            x_col="pval", 
            fill_col=color_col, 
            key_vals=plot_df$key, 
            title="P-value histogram: Compare dataset") %>% 
                layout(dragmode="none", barmode="stack") %>% 
                toWebGL()
    })
 
    output$target_data_dt = DT::renderDataTable({
        
        req(reactive_vals$mapping_obj())
        
        round_digits <- 3
        trunc_length <- 20
        
        if (!is.null(reactive_vals$mapping_obj()$get_combined_dataset())) {
            target_df <- reactive_vals$mapping_obj()$get_combined_dataset()
        }
        else {
            return()
        }
        
        event.data <- event_data("plotly_selected", source = "subset")
        if (!is.null(event.data) == TRUE) {
            target_df <- target_df[target_df$comb_id %in% event.data$key, ] 
        }
        
        target_df %>%
            mutate_if(
                is.character,
                ~str_trunc(., trunc_length)
            ) %>%
            mutate_if(
                is.numeric,
                ~round(., round_digits)
            )
    })
}

