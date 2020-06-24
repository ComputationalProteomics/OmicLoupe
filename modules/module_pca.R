library(ggplot2)
# library(PCAtools)
library(ggthemes)

setup_pca_ui <- function(id) {
    ns <- NS(id)
    tabPanel(
        id,
        fluidPage(
            bar_w_help("Principal Component Analysis", ns("help")),
            fluidRow(
                column(4,
                       wellPanel(
                           selectInput(ns("dataset1"), "Reference dataset", choices = c(""), selected = ""),
                           selectInput(ns("dataset2"), "Comparison dataset", choices = c(""), selected = ""),
                           fluidRow(
                               column(6, checkboxInput(ns("pairplot"), "Show pairplot", value=FALSE)),
                               column(6, numericInput(ns("pairplot_pcs"), "Pairplot PCs", value=3, min=1, step=1))
                           )
                       ),
                       wellPanel(
                           tabsetPanel(
                               type = "tabs",
                               tabPanel(
                                   "Dataset 1", 
                                   numericInput(ns("pc_comp_1_data1"), "PC1", min=1, value=1, step=1),
                                   numericInput(ns("pc_comp_2_data1"), "PC2", min=1, value=2, step=1),
                                   # selectInput(ns("sample_data1"), "Sample", choices=c("")),
                                   fluidRow(
                                       column(8, selectInput(ns("color_data1"), "Color", choices=c(""))),
                                       column(4, checkboxInput(ns("data1_as_factor"), "As factor", value=FALSE), style="margin-top: 25px")
                                   ),
                                   fluidRow(column(8, selectInput(ns("shape_data1"), "Shape", choices=c("")))),
                                   checkboxInput(ns("do_filter_samples_data1"), "Filter samples", value=FALSE),
                                   conditionalPanel(
                                       sprintf("input['%s'] == 1", ns("do_filter_samples_data1")),
                                       selectInput(ns("filter_cond_data1"), "Filter condition", choices=c("")),
                                       selectInput(ns("display_levels_data1"), "Display levels", choices = c(""), selected="", multiple=TRUE)
                                   )
                               ),
                               tabPanel(
                                   "Dataset 2", 
                                   numericInput(ns("pc_comp_1_data2"), "PC1", min=1, value=1, step=1),
                                   numericInput(ns("pc_comp_2_data2"), "PC2", min=1, value=2, step=1),
                                   # selectInput(ns("sample_data2"), "Sample", choices=c("")),
                                   fluidRow(
                                       column(8, selectInput(ns("color_data2"), "Color", choices=c(""))),
                                       column(4, checkboxInput(ns("data2_as_factor"), "As factor", value=FALSE), style="margin-top: 25px")
                                   ),
                                   fluidRow(column(8, selectInput(ns("shape_data2"), "Shape", choices=c("")))),
                                   checkboxInput(ns("do_filter_samples_data2"), "Filter samples", value=FALSE),
                                   conditionalPanel(
                                       sprintf("input['%s'] == 1", ns("do_filter_samples_data2")),
                                       selectInput(ns("filter_cond_data2"), "Filter condition", choices=c("")),
                                       selectInput(ns("display_levels_data2"), "Display levels", choices = c(""), selected="", multiple=TRUE)
                                   )
                               ),
                               tabPanel(
                                   "Other settings", 
                                   numericInput(ns("dot_size"), "Dot size", min=1, value=3, step=1),
                                   checkboxInput(ns("scale_pca_data"), "Scale", value = TRUE),
                                   checkboxInput(ns("center_pca_data"), "Center", value = TRUE),
                                   checkboxInput(ns("show_labels_data"), "Show labels", value = FALSE),
                                   checkboxInput(ns("show_loadings"), "Show loadings", value = FALSE),
                                   numericInput(ns("variance_filter_data"), "Variance filter", min=0, max=1, step=0.01, value = 0.1),
                                   textInput(ns("custom_title1"), "Custom title 1", value=""),
                                   textInput(ns("custom_title2"), "Custom title 2", value=""),
                                   numericInput(ns("text_size"), "Text size", value=10)
                               )
                           )
                       )
                ),
                column(8,
                       htmlOutput(ns("warnings")),
                       conditionalPanel(
                           sprintf("input['%s'] == 1", ns("show_loadings")),
                           plotOutput(ns("loadings_plot1"), height = "200px")
                       ),
                       conditionalPanel(
                           sprintf("input['%s'] == 1", ns("pairplot")),
                           plotOutput(ns("pca_pair_plot1")) %>% withSpinner()
                       ),
                       conditionalPanel(
                           sprintf("input['%s'] == 0", ns("pairplot")),
                           plotlyOutput(ns("pca_plot1"), height = "400px") %>% withSpinner()
                       ),
                       conditionalPanel(
                           sprintf("input['%s'] == 1", ns("show_loadings")),
                           plotOutput(ns("loadings_plot2"), height = "200px")
                       ),
                       conditionalPanel(
                           sprintf("input['%s'] == 1", ns("pairplot")),
                           plotOutput(ns("pca_pair_plot2")) %>% withSpinner()
                       ),
                       conditionalPanel(
                           sprintf("input['%s'] == 0", ns("pairplot")),
                           plotlyOutput(ns("pca_plot2"), height = "400px") %>% withSpinner()
                       )
                )
            )
        )
    )
}

module_pca_server <- function(input, output, session, rv, module_name) {
    
    ########### REACTIVE ############
    
    observeEvent(input$help, {
        shinyalert(
            title = "Help: Principal component visuals",
            text = help_pca, 
            html = TRUE
        )
    })
    
    filtered_samples_ref <- reactive({
        validate(need(!is.null(rv$ddf_ref(rv, input$dataset1)), "No design matrix found for reference dataset"))
        validate(need(input$filter_cond_data1 != "", "Expecting the filtering option to be used here, but no filtering condition found"))
        
        if (input$filter_cond_data1 == "None") {
            rv$ddf_ref(rv, input$dataset1) %>%
                dplyr::select(UQ(as.name(rv$ddf_samplecol_ref(rv, input$dataset1)))) %>% 
                unlist()
        }
        else {
            filtered_samples <- rv$ddf_ref(rv, input$dataset1) %>% 
                dplyr::filter(UQ(as.name(input$filter_cond_data1)) %in% input$display_levels_data1) %>% 
                dplyr::select(UQ(as.name(rv$ddf_samplecol_ref(rv, input$dataset1)))) %>% 
                unlist()
            filtered_samples
        }
    })
    
    filtered_samples_comp <- reactive({
        validate(need(!is.null(rv$ddf_ref(rv, input$dataset2)), "No design matrix found for reference dataset"))
        validate(need(input$filter_cond_data2 != "", "Expecting the filtering option to be used here, but no filtering condition found"))
        
        
        if (input$filter_cond_data2 == "None") {
            rv$ddf_comp(rv, input$dataset2) %>%
                dplyr::select(UQ(as.name(rv$ddf_samplecol_comp(rv, input$dataset2)))) %>% 
                unlist()
        }
        else {
            filtered_samples <- rv$ddf_comp(rv, input$dataset2) %>% 
                dplyr::filter(UQ(as.name(input$filter_cond_data2)) %in% input$display_levels_data2) %>% 
                dplyr::select(UQ(as.name(rv$ddf_samplecol_comp(rv, input$dataset2)))) %>% 
                unlist()
            filtered_samples
        }
    })
    
    pca_ddf1 <- reactive({
        validate(need(!is.null(rv$ddf_ref(rv, input$dataset1)), "No design found for dataset 1 while generating design 1 for PCA"))
        validate(need(!is.null(rv$samples(rv, input$dataset1)), "No samples found for dataset 1 while generating design 1 for PCA"))
        
        target_samples <- filtered_samples_ref()
        rv$ddf_ref(rv, input$dataset1) %>% 
            dplyr::filter(UQ(as.name(rv$ddf_samplecol_ref(rv, input$dataset1))) %in% target_samples)
    })
    
    pca_ddf2 <- reactive({
        validate(need(!is.null(rv$ddf_ref(rv, input$dataset2)), "No design found for dataset 1 while generating design 2 for PCA"))
        validate(need(!is.null(rv$samples(rv, input$dataset2)), "No samples found for dataset 1 while generating design 2 for PCA"))
        
        target_samples <- filtered_samples_comp()
        rv$ddf_comp(rv, input$dataset2) %>% 
            dplyr::filter(UQ(as.name(rv$ddf_samplecol_comp(rv, input$dataset2))) %in% target_samples)
    })
    
    pca_obj1 <- reactive({
        
        validate(need(!is.null(rv$rdf_ref(rv, input$dataset1)), "No reference dataset found while building PCA 1"))
        validate(need(!is.null(rv$ddf_ref(rv, input$dataset1)), "No reference design found while building PCA 1"))
        validate(need(!is.null(rv$samples(rv, input$dataset1)), "No mapped samples found for reference while building PCA 1"))

        filtered_samples <- filtered_samples_ref()
        calculate_pca_obj(
            rv$rdf_ref(rv, input$dataset1),
            filtered_samples,
            do_scale = input$scale_pca_data,
            do_center = input$center_pca_data,
            var_cut = input$variance_filter_data
        )
    })
    
    pca_obj2 <- reactive({
        
        validate(need(!is.null(rv$rdf_ref(rv, input$dataset2)), "No reference dataset found while building PCA 2"))
        validate(need(!is.null(rv$ddf_ref(rv, input$dataset2)), "No reference design found while building PCA 2"))
        validate(need(!is.null(rv$samples(rv, input$dataset2)), "No mapped samples found for reference while building PCA 2"))
        
        filtered_samples <- filtered_samples_comp()
        calculate_pca_obj(
            rv$rdf_comp(rv, input$dataset2),
            filtered_samples,
            do_scale = input$scale_pca_data,
            do_center = input$center_pca_data,
            var_cut = input$variance_filter_data
        )
    })
    
    ########### OBSERVERS ############
    
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
        
        updateSelectInput(session, "color_data1", choices = ref_choices, selected=set_if_new(input$color_data1, ref_choices, rv$ddf_condcol_ref(rv, input$dataset1)))
        updateSelectInput(session, "color_data2", choices = comp_choices, selected=set_if_new(input$color_data2, comp_choices, rv$ddf_condcol_comp(rv, input$dataset2)))
        updateSelectInput(session, "shape_data1", choices = ref_choices, selected=set_if_new(input$shape_data1, ref_choices, ref_choices[1]))
        updateSelectInput(session, "shape_data2", choices = comp_choices, selected=set_if_new(input$shape_data2, comp_choices, comp_choices[1]))
        updateSelectInput(session, "sample_data1", choices = ref_choices, selected=set_if_new(input$sample_data1, ref_choices, ref_choices[1]))
        updateSelectInput(session, "sample_data2", choices = comp_choices, selected=set_if_new(input$sample_data2, comp_choices, comp_choices[1]))
        updateSelectInput(session, "filter_cond_data1", choices = ref_choices, selected=set_if_new(input$filter_cond_data1, ref_choices, ref_choices[1]))
        updateSelectInput(session, "filter_cond_data2", choices = comp_choices, selected=set_if_new(input$filter_cond_data2, comp_choices, comp_choices[1]))
        
        display_levels_data1 <- rv$ddf_ref(rv, input$dataset1)[[input$filter_cond_data1]]
        updateSelectInput(session, "display_levels_data1", choices = display_levels_data1, selected=display_levels_data1)
        display_levels_data2 <- rv$ddf_ref(rv, input$dataset2)[[input$filter_cond_data2]]
        updateSelectInput(session, "display_levels_data2", choices = display_levels_data2, selected=display_levels_data2)
    }
    
    observeEvent(input$filter_cond_data1, {
        display_levels_data1 <- rv$ddf_ref(rv, input$dataset1)[[input$filter_cond_data1]]
        updateSelectInput(session, "display_levels_data1", choices = display_levels_data1, selected=display_levels_data1)
    })
    
    observeEvent(input$filter_cond_data2, {
        display_levels_data2 <- rv$ddf_ref(rv, input$dataset2)[[input$filter_cond_data2]]
        updateSelectInput(session, "display_levels_data2", choices = display_levels_data2, selected=display_levels_data2)
    })
    
    observeEvent({
        rv$ddf_ref(rv, input$dataset1)
        rv$ddf_comp(rv, input$dataset2)
        rv$design_condcol_1()
        rv$design_condcol_2()
        input$dataset1
        input$dataset2}, {
            sync_param_choices()
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
    
    ########### FUNCTIONS ############
    
    make_pca_plt <- function(ddf, pca_obj, pc1, pc2, color, shape, sample, label_col, title_label="No title set", dot_size=3, show_labels=FALSE, color_as_fact=FALSE, text_size=10) {
        
        pc1_lab <- sprintf("PC%s", pc1)
        pc2_lab <- sprintf("PC%s", pc2)
        
        pc1_var <- pca_obj$sdev[pc1] ** 2 / sum(pca_obj$sdev ** 2)
        pc2_var <- pca_obj$sdev[pc2] ** 2 / sum(pca_obj$sdev ** 2)
        
        plt_df <- cbind(pca_obj$x, ddf)
        if (!is.null(shape)) {
            plt_df[[shape]] <- as.factor(plt_df[[shape]])
        }
        if (color_as_fact) {
            plt_df[[color]] <- as.factor(plt_df[[color]])
        }
        
        plt_base <- ggplot(plt_df, aes_string(x=pc1_lab, y=pc2_lab, colour=color, shape=shape, text=sample, label=label_col))
        if (!show_labels) {
            plt_base <- plt_base + geom_point(size=dot_size)
        }
        else {
            plt_base <- plt_base + geom_text(size=dot_size)
        }
        
        plt_base + 
            ggtitle(sprintf("Dataset: %s (dim: %s)", title_label, paste(dim(pca_obj$rotation), collapse=", "))) +
            xlab(sprintf("PC%s (%s %s)", pc1, round(pc1_var * 100, 2), "%")) +
            ylab(sprintf("PC%s (%s %s)", pc2, round(pc2_var * 100, 2), "%")) +
            theme(text=element_text(size=text_size))
    }
    
    make_pair_pca_plot <- function(ddf, pca_obj, color, color_as_fact=FALSE, pcs) {
        
        pc_vars <- pca_obj$sdev[1:pcs] ** 2 / sum(pca_obj$sdev ** 2)
        plt_df <- cbind(pca_obj$x, ddf)
        if (color_as_fact) {
            plt_df[[color]] <- as.factor(plt_df[[color]])
        }
        plt_df %>% dplyr::select(c(paste0("PC", 1:pcs), color)) %>% ggpairs(aes_string(color=color, alpha=0.5))
    }
    
    make_loadings_plot <- function(pca_obj, title, display_count) {
        vars <- pca_obj$sdev ** 2
        perc_vars <- vars / sum(vars)
        pcs <- paste0("PC", seq_len(length(perc_vars)))
        plot_df <- data.frame(PC=pcs, perc_var=perc_vars) %>% head(display_count)
        plot_df$PC <- factor(plot_df$PC, levels = head(pcs, display_count))
        ggplot(plot_df, aes(x=PC, y=perc_var)) + geom_col(fill="#000077") + ggtitle(title)
    }
    
    ########### OUTPUTS ############
    
    output$loadings_plot1 <- renderPlot({
        make_loadings_plot(pca_obj1(), "Loadings PCA 1", display_count=10)
    })
    
    output$loadings_plot2 <- renderPlot({
        make_loadings_plot(pca_obj2(), "Loadings PCA 2", display_count=10)
    })
    
    has_value <- function(design_col) {
        design_col != "None" && design_col != ""
    }
    
    output$pca_pair_plot1 <- renderPlot({
        if (has_value(input$color_data1)) color_col <- input$color_data1
        else color_col <- NULL
        make_pair_pca_plot(
            ddf=pca_ddf1(),
            pca_obj=pca_obj1(),
            color=color_col,
            color_as_fact=input$data1_as_factor,
            pcs=input$pairplot_pcs
        )
    })
    
    output$pca_pair_plot2 <- renderPlot({
        if (has_value(input$color_data2)) color_col <- input$color_data2
        else color_col <- NULL
        make_pair_pca_plot(
            ddf=pca_ddf2(),
            pca_obj=pca_obj2(),
            color=color_col,
            color_as_fact=input$data2_as_factor,
            pcs=input$pairplot_pcs
        )
    })
    
    output$pca_plot1 <- renderPlotly({
        
        # validate(need(length(plot_list) > 1, sprintf(sprintf("Number of contrasts need to be more than one, found: %s", length(plot_list)))))
        
        if (has_value(input$color_data1)) color_col <- input$color_data1
        else color_col <- NULL
        
        if (has_value(input$shape_data1)) shape_col <- input$shape_data1
        else shape_col <- NULL
        
        sample_col <- rv$ddf_samplecol_ref(rv, input$dataset1)
        # data <- rv$ddf_ref(rv, input$dataset1)
        data <- pca_ddf1()
        data$Sample <- data[[sample_col]]
        
        plt <- make_pca_plt(
            ddf=data,
            pca_obj=pca_obj1(),
            pc1=input$pc_comp_1_data1,
            pc2=input$pc_comp_2_data1,
            color=color_col,
            shape=shape_col,
            sample=sample_col,
            label_col="Sample",
            title_label=input$dataset1,
            dot_size=input$dot_size,
            show_labels = input$show_labels_data, 
            color_as_fact = input$data1_as_factor,
            text_size=input$text_size
        )
        
        if (input$custom_title1 != "") {
            plt <- plt + ggtitle(input$custom_title1)
        }
        
        plt %>% ggplotly(tooltip=c("label", "colour", "x", "y")) %>% plotly::layout(dragmode="select")
    })
    
    output$pca_plot2 <- renderPlotly({
        
        if (has_value(input$color_data2)) color_col <- input$color_data2
        else color_col <- NULL
        
        if (has_value(input$shape_data2)) shape_col <- input$shape_data2
        else shape_col <- NULL
        
        sample_col <- rv$ddf_samplecol_ref(rv, input$dataset2)
        data <- pca_ddf2()
        # data <- rv$ddf_ref(rv, input$dataset2)
        data$label <- data[[sample_col]]
        
        plt <- make_pca_plt(
            data,
            pca_obj2(),
            input$pc_comp_1_data2,
            input$pc_comp_2_data2,
            color_col,
            shape_col,
            sample_col,
            "label",
            input$dot_size,
            title_label=input$dataset2,
            show_labels=input$show_labels_data, 
            color_as_fact=input$data2_as_factor,
            text_size=input$text_size
        ) 
        
        if (input$custom_title2 != "") {
            plt <- plt + ggtitle(input$custom_title2)
        }
        
        plt %>% ggplotly(tooltip=c("label", "colour", "x", "y")) %>% plotly::layout(dragmode="select")
    })
}
