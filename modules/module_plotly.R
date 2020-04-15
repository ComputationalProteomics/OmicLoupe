
library(ggplot2)
theme_set(theme_classic())
library(ggpubr)
library(shinycssloaders)

source("R/vis_server_utils.R")
source("R/vis_server_plots.R")

MY_COLORS <- c("grey50", "blue", "red", "orange")

MAX_DISCRETE_LEVELS <- 20

setup_plotly_ui <- function(id) {
    
    ns <- NS(id)
    tabPanel(
        id,
        fluidPage(
            fluidRow(
                span(
                    style="display: inline-block; vertical-align:top; padding-right:10px; margin-top; -50px;", 
                    h3("Statistical investigations")
                ),
                span(
                    style="display: inline-block; vertical-align:top; width: 30px; padding-top:25px; padding-bottom:30px;", 
                    actionButton(ns("help"), "", icon=icon("question"), style="padding-top:2px; font-size:70%;", class="btn-xs help")
                )
            ),
            fluidRow(
                column(4,
                       wellPanel(
                           selectInput(ns("color_type"), "Coloring type", choices=c("Threshold", "PCA", "Column"), selected="select"),
                           conditionalPanel(
                               sprintf("input['%s'] == 'PCA'", ns("color_type")),
                               fluidRow(
                                   column(4, numericInput(ns("plot_pc1"), "Plt1 PC", value=1, min=1, step=1)),
                                   column(4, numericInput(ns("plot_pc2"), "Plt2 PC", value=1, min=1, step=1))
                               )
                           ),
                           conditionalPanel(
                               sprintf("input['%s'] == 'Column'", ns("color_type")),
                               fluidRow(
                                   column(6, selectInput(ns("color_col_1"), "Ref. column", choices = c(""))),
                                   column(6, selectInput(ns("color_col_2"), "Comp. column", choices = c("")))
                               )
                           ),
                           column(6, fluidRow(
                               selectInput(ns("dataset1"), "Ref. data", choices=c(), selected = ""),
                               selectInput(ns("dataset2"), "Comp. data", choices=c(), selected = "")
                           )),
                           column(6, fluidRow(
                               selectInput(ns("stat_base1"), "Ref. Comparison", choices=c(), selected = ""),
                               selectInput(ns("stat_base2"), "Comp. Comparison", choices=c(), selected = "")
                           )),
                           fluidRow(
                               column(9,
                                      sliderInput(ns("pvalue_cutoff"), "P-value cutoff", value=0.05, step=0.01, min=0, max=1)
                               ), column(3, 
                                         span(
                                             selectInput(ns("pvalue_type_select"), 
                                                         choices = c("P.Value", "adj.P.Val"), 
                                                         selected = "P.Value", 
                                                         label = "P-value type"),
                                             style="padding:20px")
                               )),
                           sliderInput(ns("fold_cutoff"), "Fold cutoff", value=1, step=0.1, min=0, max=10),
                           sliderInput(ns("pca_variance_cutoff"), "PCA var. cut.", value=0.4, step=0.05, min=0, max=1),
                           checkboxInput(ns("set_same_axis"), "Set same axis ranges", value = FALSE),
                           textInput(ns("ref_custom_header"), "Custom ref. header", value=""),
                           textInput(ns("comp_custom_header"), "Custom comp. header", value=""),
                           checkboxInput(ns("more_settings"), "Show more settings", value = FALSE),
                           conditionalPanel(
                               sprintf("input['%s'] == 1", ns("more_settings")),
                               sliderInput(ns("bin_count"), "Bin count", value=100, step=10, min=10, max=200),
                               sliderInput(ns("alpha"), "Alpha (0 - 1)", value=0.4, step=0.01, min=0, max=1)
                           )
                       )
                ),
                column(8,
                       htmlOutput(ns("warnings")),
                       p("Drag in figures to highlight features. Double click to unselect."),
                       column(6,
                              plotlyOutput(ns("plotly_volc1")) %>% withSpinner(),
                              plotlyOutput(ns("plotly_ma1")) %>% withSpinner(),
                              plotlyOutput(ns("plotly_hist1")) %>% withSpinner()
                       ),
                       column(6,
                              plotlyOutput(ns("plotly_volc2")) %>% withSpinner(),
                              plotlyOutput(ns("plotly_ma2")) %>% withSpinner(),
                              plotlyOutput(ns("plotly_hist2")) %>% withSpinner()
                       )
                )
            ),
            downloadButton(ns("download_table"), "Download table"),
            DT::DTOutput(ns("table_display"))
        )
    )
}


module_plotly_server <- function(input, output, session, rv, module_name) {
    
    output$download_table <- downloadHandler(
        filename = function() {
            paste("comp_scatter-", Sys.Date(), ".tsv", sep="")
        },
        content = function(file) {
            
            # event.data <- event_data("plotly_selected", source = "subset")
            # if (!is.null(event.data) == TRUE) {
            target_df <- get_target_df(rv)
            # }
            # else {
            #     target_df <- get_pass_thres_df()
            # }
            dt_parsed_target <- rv$dt_parsed_data_raw(rv, target_df)
            write_tsv(rv$dt_parsed_data_raw(rv, dt_parsed_target), file)
        }
    )
    
    observeEvent(input$help, {
        shinyalert(
            title = "Help: Statistics visuals",
            text = help_statistics, 
            html = TRUE
        )
    })
    
    # ---------------- REACTIVE ---------------- 
    
    get_thres_pass_type_col <- function(df, stat_cols1, stat_cols2, pvalue_cut, fold_cut, stat_pattern) {
        
        pass_threshold_data1 <- df[[stat_cols1[[stat_pattern]]]] < pvalue_cut & abs(df[[stat_cols1$logFC]]) > fold_cut
        pass_threshold_data2 <- df[[stat_cols2[[stat_pattern]]]] < pvalue_cut & abs(df[[stat_cols2$logFC]]) > fold_cut
        pass_both <- pass_threshold_data1 & pass_threshold_data2
        
        pass_type <- rep("None", length(pass_both))
        pass_type[pass_threshold_data1] <- "Ref"
        pass_type[pass_threshold_data2] <- "Comp"
        pass_type[pass_both] <- "Both"
        pass_type_col <- factor(pass_type, levels = c("None", "Both", "Ref", "Comp"))
        
        pass_type_col
    }
    
    reactive_plot_df <- reactive({
        
        req(rv$statcols_ref(rv, input$dataset1, input$stat_base1))
        req(rv$statcols_comp(rv, input$dataset2, input$stat_base2))
        
        if (input$color_type == "PCA") {
            combined_dataset <- rv$mapping_obj()$get_combined_dataset(full_entries=TRUE)
        }
        else {
            combined_dataset <- rv$mapping_obj()$get_combined_dataset(full_entries=FALSE)
        }
        
        pass_thres_col <- get_thres_pass_type_col(
            combined_dataset,
            rv$statcols_ref(rv, input$dataset1, input$stat_base1),
            rv$statcols_comp(rv, input$dataset2, input$stat_base2),
            input$pvalue_cutoff,
            input$fold_cutoff,
            input$pvalue_type_select
        )
        
        target_statcol <- rv$statcols_ref(rv, input$dataset1, input$stat_base1)[[input$pvalue_type_select]]
        base_df <- cbind(
            combined_dataset, 
            pass_threshold_data=pass_thres_col,
            annot_ref=combined_dataset[, paste0(sprintf("d%s.", di(rv, input$dataset1, 1)), rv$rdf_annotcol_ref(rv, input$dataset1))],
            annot_comp=combined_dataset[, paste0(sprintf("d%s.", di(rv, input$dataset2, 2)), rv$rdf_annotcol_comp(rv, input$dataset2))]
        ) %>% arrange(desc(UQ(as.name(target_statcol))))
        
        if (input$color_type == "Threshold") {
            base_df
        }
        else if (input$color_type == "PCA") {
            
            req(rv$samples(rv, input$dataset1))
            req(rv$samples(rv, input$dataset2))
            
            ref_pca_df <- calculate_pca_obj(
                base_df,
                paste(sprintf("d%s", di(rv, input$dataset1, 1)), rv$samples(rv, input$dataset1), sep="."),
                do_scale = TRUE,
                do_center = TRUE,
                var_cut = input$pca_variance_cutoff,
                return_df = TRUE,
                col_prefix="ref."
            )
            
            comp_pca_df <- calculate_pca_obj(
                base_df,
                paste(sprintf("d%s", di(rv, input$dataset2, 2)), rv$samples(rv, input$dataset2), sep="."),
                do_scale = TRUE,
                do_center = TRUE,
                var_cut = input$pca_variance_cutoff,
                return_df = TRUE,
                col_prefix="comp."
            )
            
            pca_df <- cbind(ref_pca_df, comp_pca_df)
            pca_df
        }
        else if (input$color_type == "Column") {
            base_df$ref.color_col <- base_df[[sprintf("d%s.%s", di(rv, input$dataset1, 1), input$color_col_1)]]
            base_df$comp.color_col <- base_df[[sprintf("d%s.%s", di(rv, input$dataset2, 2), input$color_col_2)]]
            base_df %>% arrange(ref.color_col)
        }
        else {
            warning("Unknown color_type !")
        }
    })
    
    parse_plot_df <- function(target_statcols, feature_col="target_col1") {
        
        plot_df <- data.frame(
            fold = reactive_plot_df()[[target_statcols$logFC]],
            sig = -log10(reactive_plot_df()[[target_statcols$P.Value]]),
            lab = rv$mapping_obj()[[feature_col]],
            expr = reactive_plot_df()[[target_statcols$AveExpr]],
            pval = reactive_plot_df()[[target_statcols$P.Value]],
            pass_thres = reactive_plot_df()$pass_threshold_data,
            hover_text = reactive_plot_df()$comb_id,
            key = reactive_plot_df()$comb_id,
            annot_ref = reactive_plot_df()$annot_ref,
            annot_comp = reactive_plot_df()$annot_comp
        )
        
        plot_df$descr <- lapply(lapply(paste0(sprintf("%s: %s", plot_df$key, plot_df$annot_ref)), strwrap, width=30), paste, collapse="<br>")
        
        
        if (input$color_type == "PCA") {
            plot_df$ref.PC <- reactive_plot_df()[[sprintf("%s.PC%s", "ref", input$plot_pc1)]]
            plot_df$comp.PC <- reactive_plot_df()[[sprintf("%s.PC%s", "comp", input$plot_pc2)]]
            warning("The pass_thres could be better calculated for histograms also in PCA")
            plot_df$pass_thres <- TRUE
        }
        else if (input$color_type == "Column") {
            plot_df$ref.color_col <- reactive_plot_df()[[sprintf("%s.color_col", "ref")]]
            plot_df$comp.color_col <- reactive_plot_df()[[sprintf("%s.color_col", "comp")]]
        }
        
        plot_df
    }
    
    plot_ref_df <- reactive({
        req(rv$statcols_ref(rv, input$dataset1, input$stat_base1))
        parse_plot_df(rv$statcols_ref(rv, input$dataset1, input$stat_base1))
    })
    
    plot_comp_df <- reactive({
        req(rv$statcols_comp(rv, input$dataset2, input$stat_base2))
        parse_plot_df(rv$statcols_comp(rv, input$dataset2, input$stat_base2))
    })
    
    # ---------------- OBSERVERS ---------------- 
    
    observeEvent(input$pvalue_type_select, {
        updateSliderInput(session, inputId="pvalue_cutoff", label=sprintf("%s cutoff", input$pvalue_type_select), 
                          value=input$pvalue_cutoff, min=0, max=1, step=0.01)
    })
    
    observeEvent({
        rv$selected_cols_obj() 
        input$dataset1 
        input$dataset2}, {
            # update_stat_inputs(session, rv, input$dataset1, input$dataset2)
            if (is.null(rv$filename_1()) && is.null(rv$filename_2())) {
                return()
            }
            
            choices_1 <- rv$selected_cols_obj()[[input$dataset1]]$statpatterns
            choices_2 <- rv$selected_cols_obj()[[input$dataset2]]$statpatterns
            
            updateSelectInput(session, "stat_base1", choices=choices_1, selected=choices_1[1])
            updateSelectInput(session, "stat_base2", choices=choices_2, selected=choices_2[1])
        })
    
    observeEvent(rv$filedata_1(), {
        choices <- get_dataset_choices(rv)
        updateSelectInput(session, "dataset1", choices=choices, selected=choices[1])
        updateSelectInput(session, "dataset2", choices=choices, selected=choices[1])
    })
    
    observeEvent({
        input$dataset1
        input$dataset2}, {
            ref_data_cols <- rv$rdf_cols_ref(rv, input$dataset1)
            comp_data_cols <- rv$rdf_cols_comp(rv, input$dataset2)
            updateSelectInput(session, "color_col_1", choices=ref_data_cols, selected=ref_data_cols[1])
            updateSelectInput(session, "color_col_2", choices=comp_data_cols, selected=comp_data_cols[1])
        })
    
    observeEvent(rv$filedata_2(), {
        choices <- get_dataset_choices(rv)
        updateSelectInput(session, "dataset1", choices=choices, selected=choices[1])
        updateSelectInput(session, "dataset2", choices=choices, selected=choices[1])
    })
    
    # ---------------- FUNCTIONS ---------------- 
    
    
    
    # Inspired by: https://plot.ly/r/shiny-coupled-events/
    make_scatter <- function(plot_df, x_col, y_col, x_lab=NULL, y_lab=NULL, color_col, hover_text="hover_text", title="", 
                             manual_scale=TRUE, cont_scale=NULL, alpha=0.5) {
        
        plt <- ggplot(plot_df, aes_string(x=x_col, y=y_col, color=color_col, key=hover_text)) +
            # plt <- ggplot(plot_df, aes_string(x=x_col, y=y_col, color=color_col, key=key, text=hover_text)) +
            geom_point(alpha=alpha)
        
        if (!title != "") {
            plt <- plt + ggtitle(title)
        }
        
        if (!is.null(xlab)) {
            plt <- plt + xlab(x_lab)
        }
        
        if (!is.null(ylab)) {
            plt <- plt + ylab(y_lab)
        }
        
        # plt <- plot_ly(
        #     plot_df,
        #     x = ~get(x_col),
        #     y = ~get(y_col),
        #     color = ~get(color_col),
        #     source = "subset",
        #     key = plot_df[[key]]
        # )
        
        if (manual_scale) {
            plt <- plt + scale_color_manual(values=MY_COLORS)
        }
        else if (!is.null(cont_scale)) {
            plt <- plt + scale_color_gradient2(low="red", mid="grey", high="blue")
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
        ) %>% layout(title = title, xaxis=list(title="P-value"), yaxis=list(title="Count"))
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
        if (is.null(rv$filename_1())) {
            error_vect <- c(error_vect, "No filename_1 found, upload dataset at Setup page")
        }
        else {
            if ((is.null(rv$samples(rv, input$dataset1)) || is.null(rv$samples(rv, input$dataset2))) && input$color_type == "PCA") {
                error_vect <- c(error_vect, "No mapped samples found needed for PCA, map at Setup page")
            }
            if (is.null(rv$statcols_ref(rv, input$dataset1, input$stat_base1)) || is.null(rv$statcols_comp(rv, input$dataset2, input$stat_base2))) {
                error_vect <- c(error_vect, "ref_statcols or comp_statcols not found, assign stat-columns at Setup page")
            }
        }
        
        if (is.null(rv$design_1()) && input$color_type == "PCA") {
            error_vect <- c(error_vect, "No design_1 found, upload dataset at Setup page")
        }
        
        total_text <- paste(error_vect, collapse="<br>")
        HTML(sprintf("<b><font size='5' color='red'>%s</font></b>", total_text))
    })
    
    set_shared_max_lims <- function(plt, xcol, ycol, ref_df, comp_df) {
        min_fold <- min(c(ref_df[[xcol]], comp_df[[xcol]]), na.rm=TRUE)
        max_fold <- max(c(ref_df[[xcol]], comp_df[[xcol]]), na.rm=TRUE)
        min_sig <- min(c(ref_df[[ycol]], comp_df[[ycol]]), na.rm=TRUE)
        max_sig <- max(c(ref_df[[ycol]], comp_df[[ycol]]), na.rm=TRUE)
        plt + xlim(min_fold, max_fold) + ylim(min_sig, max_sig)
    }
    
    parse_event_key = function(event_data) {
        event_data$key %>% unlist() %>% strsplit(":") %>% lapply(function(elem){elem[[1]]}) %>% unlist()
    }
    
    build_plotly <- function(plt, title, dataset, stat_base, custom_header) {
        t <- list(family="sans serif", size=8)
        
        if (custom_header == "") title <- sprintf("Data: %s<br>Contrast: %s", dataset, stat_base)
        else title <- custom_header
        
        plt %>% 
            ggplotly(source="subset") %>%
            layout(title=title, dragmode="select", font=t) %>%
            toWebGL()
    }
    
    output$plotly_volc1 <- renderPlotly({
        
        req(rv$mapping_obj())
        
        plot_df <- plot_ref_df()
        event.data <- event_data("plotly_selected", source = "subset")
        manual_scale <- TRUE
        cont_scale <- NULL
        if (!is.null(event.data) == TRUE) {
            plot_df$selected <- plot_df$key %in% parse_event_key(event.data)
            color_col <- "selected"
        } 
        else {
            color_col <- retrieve_color_col(input$color_type, "ref")
            if (input$color_type %in% c("PCA", "Column")) {
                manual_scale <- FALSE
            }
            if (input$color_type == "PCA") {
                cont_scale <- TRUE
            }
        }
        
        validate(
            need(is.numeric(plot_df[[color_col]]) || length(unique(plot_df[[color_col]])) < MAX_DISCRETE_LEVELS, 
                 sprintf("The selected Ref. column needs to have a continuous variable or max %s discrete levels", MAX_DISCRETE_LEVELS))
        )
        
        base_plt <- make_scatter(
            plot_df, 
            x_col="fold", 
            y_col="sig", 
            x_lab="Fold change",
            y_lab="P-value (-log10)",
            color_col=color_col, 
            hover_text="descr", 
            alpha=input$alpha,
            cont_scale = cont_scale,
            manual_scale = manual_scale)
        
        if (input$set_same_axis) {
            base_plt <- set_shared_max_lims(base_plt, "fold", "sig", plot_ref_df(), plot_comp_df())
        }
        
        build_plotly(base_plt, title, input$dataset1, input$stat_base1, input$ref_custom_header)
    })
    
    output$plotly_volc2 <- renderPlotly({
        
        req(rv$mapping_obj())
        
        plot_df <- plot_comp_df()
        event.data <- event_data("plotly_selected", source = "subset")
        manual_scale <- TRUE
        cont_scale <- NULL
        if (!is.null(event.data) == TRUE) {
            plot_df$selected <- plot_df$key %in% parse_event_key(event.data)
            color_col <- "selected"
        } 
        else {
            color_col <- retrieve_color_col(input$color_type, "comp")
            if (input$color_type %in% c("PCA", "Column")) {
                manual_scale <- FALSE
            }
            if (input$color_type == "PCA") {
                cont_scale <- TRUE
            }        
        }
        
        validate(
            need(is.numeric(plot_df[[color_col]]) || length(unique(plot_df[[color_col]])) < MAX_DISCRETE_LEVELS, 
                 sprintf("The selected Comp. column needs to have a continuous variable or max %s discrete levels", MAX_DISCRETE_LEVELS))
        )
        
        # plot_df$descr <- lapply(lapply(paste0(sprintf("%s: %s", plot_df$key, plot_df$annot_comp)), strwrap, width=30), paste, collapse="<br>")
        
        base_plt <- make_scatter(
            plot_df, 
            x_col="fold", 
            y_col="sig", 
            x_lab="Fold change (log2)",
            y_lab="Significance (-log10)",
            color_col=color_col, 
            hover_text="descr", 
            alpha=input$alpha,
            cont_scale = cont_scale,
            manual_scale = manual_scale) 
        
        if (input$set_same_axis) {
            base_plt <- set_shared_max_lims(base_plt, "fold", "sig", plot_ref_df(), plot_comp_df())
        }
        
        build_plotly(base_plt, title, input$dataset2, input$stat_base2, input$comp_custom_header)
    })
    
    output$plotly_ma1 <- renderPlotly({
        
        req(rv$mapping_obj())
        
        plot_df <- plot_ref_df()
        event.data <- event_data("plotly_selected", source = "subset")
        manual_scale <- TRUE
        cont_scale <- NULL
        if (!is.null(event.data) == TRUE) {
            plot_df$selected <- plot_df$key %in% parse_event_key(event.data)
            color_col <- "selected"
        } 
        else {
            color_col <- retrieve_color_col(input$color_type, "ref")
            if (input$color_type %in% c("PCA", "Column")) {
                manual_scale <- FALSE
            }
            if (input$color_type == "PCA") {
                cont_scale <- TRUE
            }
        }        
        
        req(is.numeric(plot_df[[color_col]]) || length(unique(plot_df[[color_col]])) < MAX_DISCRETE_LEVELS)
        
        # plot_df$descr <- lapply(lapply(paste0(sprintf("%s: %s", plot_df$key, plot_df$annot_ref)), strwrap, width=30), paste, collapse="<br>")
        
        base_plt <- make_scatter(
            plot_df, 
            x_col="expr", 
            y_col="fold", 
            x_lab="Average expression",
            y_lab="Fold change (log2)",
            color_col=color_col, 
            alpha=input$alpha,
            hover_text="descr", 
            cont_scale = cont_scale,
            manual_scale = manual_scale)
        
        if (input$set_same_axis) {
            base_plt <- set_shared_max_lims(base_plt, "expr", "fold", plot_ref_df(), plot_comp_df())
        }
        
        build_plotly(base_plt, title, input$dataset1, input$stat_base1, input$ref_custom_header)
    })
    
    output$plotly_ma2 <- renderPlotly({
        
        req(rv$mapping_obj())
        
        plot_df <- plot_comp_df()
        event.data <- event_data("plotly_selected", source = "subset")
        manual_scale <- TRUE
        cont_scale <- NULL
        if (!is.null(event.data) == TRUE) {
            plot_df$selected <- plot_df$key %in% parse_event_key(event.data)
            color_col <- "selected"
        } 
        else {
            color_col <- retrieve_color_col(input$color_type, "comp")
            if (input$color_type %in% c("PCA", "Column")) {
                manual_scale <- FALSE
            }
            if (input$color_type == "PCA") {
                cont_scale <- TRUE
            }
        }
        
        req(is.numeric(plot_df[[color_col]]) || length(unique(plot_df[[color_col]])) < MAX_DISCRETE_LEVELS)
        
        # plot_df$descr <- lapply(lapply(paste0(sprintf("%s: %s", plot_df$key, plot_df$annot_comp)), strwrap, width=30), paste, collapse="<br>")
        
        base_plt <- make_scatter(
            plot_df, 
            x_col="expr", 
            y_col="fold", 
            x_lab="Average expression",
            y_lab="Fold change (log2)",
            color_col=color_col, 
            alpha=input$alpha,
            hover_text="descr", 
            cont_scale = cont_scale,
            manual_scale = manual_scale)
        
        if (input$set_same_axis) {
            base_plt <- set_shared_max_lims(base_plt, "expr", "fold", plot_ref_df(), plot_comp_df())
        }
        
        build_plotly(base_plt, title, input$dataset2, input$stat_base2, input$comp_custom_header)
    })
    
    output$plotly_hist1 <- renderPlotly({
        
        req(rv$mapping_obj())
        
        plot_df <- plot_ref_df()
        event.data <- event_data("plotly_selected", source = "subset")
        
        if (!is.null(event.data) == TRUE) {
            plot_df$selected <- plot_df$key %in% parse_event_key(event.data)
            color_col <- "selected"
        }
        else {
            color_col <- "pass_thres"
        }
        
        if (input$ref_custom_header == "") title <- sprintf("Dataset: %s", input$dataset1)
        else title <- input$ref_custom_header
        
        make_histogram(
            plot_df, 
            x_col="pval", 
            fill_col=color_col, 
            key_vals=plot_df$key,
            title=title) %>% 
            layout(dragmode="none", barmode="stack") %>% 
            toWebGL()
    })
    
    output$plotly_hist2 <- renderPlotly({
        
        req(rv$mapping_obj())
        
        plot_df <- plot_comp_df()
        event.data <- event_data("plotly_selected", source = "subset")
        
        if (!is.null(event.data) == TRUE) {
            plot_df$selected <- plot_df$key %in% parse_event_key(event.data)
            color_col <- "selected"
        }
        else {
            color_col <- "pass_thres"
        }
        
        if (input$comp_custom_header == "") title <- sprintf("Dataset: %s", input$dataset2)
        else title <- input$comp_custom_header
        
        make_histogram(
            plot_df, 
            x_col="pval", 
            fill_col=color_col, 
            key_vals=plot_df$key, 
            title=title) %>% 
            layout(dragmode="none", barmode="stack") %>% 
            toWebGL()
    })
    
    get_target_df <- function(rv) {
        combined_dataset <- rv$mapping_obj()$get_combined_dataset()
        pass_thres_col <- get_thres_pass_type_col(
            combined_dataset,
            rv$statcols_ref(rv, input$dataset1, input$stat_base1),
            rv$statcols_comp(rv, input$dataset2, input$stat_base2),
            input$pvalue_cutoff,
            input$fold_cutoff,
            input$pvalue_type_select
        )
        target_df <- cbind(pass_thres=pass_thres_col, combined_dataset)
        event.data <- event_data("plotly_selected", source = "subset")
        if (!is.null(event.data) == TRUE) {
            out_df <- target_df[target_df$comb_id %in% parse_event_key(event.data), ] 
        }
        else {
            out_df <- target_df %>% filter(pass_thres != "None")
        }
        out_df
    }
    
    selected_id_reactive <- reactive({
        get_target_df(rv)[input$table_display_rows_selected, ]$comb_id %>% as.character()
    })
    
    observeEvent(input$table_display_rows_selected, {
        rv$set_selected_feature(selected_id_reactive(), module_name)
    })
    
    output$table_display = DT::renderDataTable({
        
        req(rv$mapping_obj())
        req(rv$mapping_obj()$get_combined_dataset())
        
        target_df <- get_target_df(rv)
        rv$dt_parsed_data(rv, target_df, add_show_cols_first="pass_thres")
    })
}

