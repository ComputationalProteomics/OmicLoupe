library(ggplot2)
# library(PCAtools)
library(ggthemes)

setup_pca_ui <- function(id) {
    ns <- NS(id)
    tabPanel(
        id,
        fluidPage(
            fluidRow(
                column(4,
                       wellPanel(
                           selectInput(ns("dataset1"), "Reference dataset", choices = c(""), selected = ""),
                           selectInput(ns("dataset2"), "Comparison dataset", choices = c(""), selected = "")
                       ),
                       wellPanel(
                           tabsetPanel(
                               type = "tabs",
                               tabPanel(
                                   "Dataset 1", 
                                   numericInput(ns("pc_comp_1_data1"), "PC1", min=1, value=1, step=1),
                                   numericInput(ns("pc_comp_2_data1"), "PC2", min=1, value=2, step=1),
                                   selectInput(ns("sample_data1"), "Sample", choices=c("")),
                                   fluidRow(
                                       column(8, selectInput(ns("color_data1"), "Color", choices=c(""))),
                                       column(4, checkboxInput(ns("data1_as_factor"), "As factor", value=FALSE), style="margin-top: 25px")
                                   ),
                                   fluidRow(
                                       column(8, selectInput(ns("shape_data1"), "Shape", choices=c("")))
                                   )
                               ),
                               tabPanel(
                                   "Dataset 2", 
                                   numericInput(ns("pc_comp_1_data2"), "PC1", min=1, value=1, step=1),
                                   numericInput(ns("pc_comp_2_data2"), "PC2", min=1, value=2, step=1),
                                   selectInput(ns("sample_data2"), "Sample", choices=c("")),
                                   fluidRow(
                                       column(8, selectInput(ns("color_data2"), "Color", choices=c(""))),
                                       column(4, checkboxInput(ns("data2_as_factor"), "As factor", value=FALSE), style="margin-top: 25px")
                                   ),
                                   fluidRow(
                                       column(8, selectInput(ns("shape_data2"), "Shape", choices=c("")))
                                   )
                               ),
                               tabPanel(
                                   "Other", 
                                   numericInput(ns("dot_size"), "Dot size", min=1, value=3, step=1),
                                   checkboxInput(ns("scale_pca_data"), "Scale", value = TRUE),
                                   checkboxInput(ns("center_pca_data"), "Center", value = TRUE),
                                   checkboxInput(ns("show_labels_data"), "Show labels", value = FALSE),
                                   checkboxInput(ns("show_loadings"), "Show loadings", value = FALSE),
                                   numericInput(ns("variance_filter_data"), "Variance filter", min=0, max=1, step=0.01, value = 0.1)
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
                       plotlyOutput(ns("pca_plot1"), height = "400px") %>% withSpinner(),
                       conditionalPanel(
                           sprintf("input['%s'] == 1", ns("show_loadings")),
                           plotOutput(ns("loadings_plot2"), height = "200px")
                       ),
                       plotlyOutput(ns("pca_plot2"), height = "400px") %>% withSpinner()
                )
            )
        )
    )
}

module_pca_server <- function(input, output, session, reactive_vals) {
    
    ########### REACTIVE ############
    
    design_ref <- reactive({ 
        if (!is.null(dataset_ind(reactive_vals, input, 1))) {
            reactive_vals[[sprintf("design_%s", dataset_ind(reactive_vals, input, 1))]]() 
        }
        else {
            NULL
        }
    })
    design_comp <- reactive({ 
        if (!is.null(dataset_ind(reactive_vals, input, 2))) {
            reactive_vals[[sprintf("design_%s", dataset_ind(reactive_vals, input, 2))]]() 
        }
        else {
            NULL
        }
    })
    data_ref <- reactive({ 
        if (!is.null(dataset_ind(reactive_vals, input, 1))) {
            reactive_vals[[sprintf("filedata_%s", dataset_ind(reactive_vals, input, 1))]]() 
        }
        else {
            NULL
        }
    })
    data_comp <- reactive({ 
        if (!is.null(dataset_ind(reactive_vals, input, 2))) {
            reactive_vals[[sprintf("filedata_%s", dataset_ind(reactive_vals, input, 2))]]() 
        }
        else {
            NULL
        }
    })
    samples_ref <- reactive({ 
        reactive_vals$selected_cols_obj()[[input[[sprintf("dataset%s", dataset_ind(reactive_vals, input, 1))]]]]$samples 
    })
    samples_comp <- reactive({ 
        reactive_vals$selected_cols_obj()[[input[[sprintf("dataset%s", dataset_ind(reactive_vals, input, 2))]]]]$samples 
    })
    
    design_cols_ref <- reactive({
        colnames(design_ref())
    })
    
    design_cols_comp <- reactive({
        colnames(design_comp())
    })
    
    pca_obj1 <- reactive({
        
        req(data_ref())
        req(design_ref())
        req(samples_ref())
        
        calculate_pca_obj(
            data_ref(),
            samples_ref(),
            do_scale = input$scale_pca_data,
            do_center = input$center_pca_data,
            var_cut = input$variance_filter_data
        )
    })
    
    pca_obj2 <- reactive({
        
        req(data_comp())
        req(design_comp())
        req(samples_comp())
        
        calculate_pca_obj(
            data_comp(),
            samples_comp(),
            do_scale = input$scale_pca_data,
            do_center = input$center_pca_data,
            var_cut = input$variance_filter_data
        )
    })
    
    ########### OBSERVERS ############
    
    sync_pca_param_choices <- function() {
        ref_choices <- c("None", design_cols_ref())
        comp_choices <- c("None", design_cols_comp())
        updateSelectInput(session, "color_data1", choices = ref_choices, selected=ref_choices[1])
        updateSelectInput(session, "shape_data1", choices = ref_choices, selected=ref_choices[1])
        updateSelectInput(session, "sample_data1", choices = ref_choices, selected=ref_choices[1])
        updateSelectInput(session, "color_data2", choices = comp_choices, selected=comp_choices[1])
        updateSelectInput(session, "shape_data2", choices = comp_choices, selected=comp_choices[1])
        updateSelectInput(session, "sample_data2", choices = comp_choices, selected=comp_choices[1])
    }
    
    observeEvent(design_ref(), {
        print("Design ref changed")
        sync_pca_param_choices()
    })
    
    observeEvent(design_comp(), {
        print("Design comp changed")
        sync_pca_param_choices()
    })
    
    observeEvent(reactive_vals$filedata_1(), {
        choices <- get_dataset_choices(reactive_vals)
        updateSelectInput(session, "dataset1", choices=choices, selected=choices[1])
        updateSelectInput(session, "dataset2", choices=choices, selected=choices[1])
    })
    
    observeEvent(reactive_vals$filedata_2(), {
        choices <- get_dataset_choices(reactive_vals)
        updateSelectInput(session, "dataset1", choices=choices, selected=choices[1])
        updateSelectInput(session, "dataset2", choices=choices, selected=choices[1])
    })
    
    
    ########### FUNCTIONS ############
    
    # dataset_ind <- function(field) {
    #     
    #     if (is.null(reactive_vals$filename_1()) || reactive_vals$filename_1() == "") {
    #         NULL
    #     }
    #     else if (input[[sprintf("dataset%s", field)]] == reactive_vals$filename_1()) {
    #         1
    #     }
    #     else if (!is.null(reactive_vals$filename_2()) && input[[sprintf("dataset%s", field)]] == reactive_vals$filename_2()) {
    #         2
    #     }
    #     else { 
    #         NULL
    #     }
    # }
    
    make_pca_plt <- function(ddf, pca_obj, pc1, pc2, color, shape, sample, dot_size=3, show_labels=FALSE, color_as_fact=FALSE) {
        
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
        
        plt_base <- ggplot(plt_df, aes_string(x=pc1_lab, y=pc2_lab, color=color, shape=shape, text=sample, label=sample))
        if (!show_labels) {
            plt_base <- plt_base + geom_point(size=dot_size)
        }
        else {
            plt_base <- plt_base + geom_text(size=dot_size)
        }
        
        plt_base + 
            ggtitle(sprintf("Rotation dimensions: %s", paste(dim(pca_obj$rotation), collapse=", "))) +
            xlab(sprintf("PC%s (%s %s)", pc1, round(pc1_var * 100, 2), "%")) +
            ylab(sprintf("PC%s (%s %s)", pc2, round(pc2_var * 100, 2), "%"))
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
    
    output$warnings <- renderUI({
        
        error_vect <- c()
        if (is.null(reactive_vals$filename_1())) {
            error_vect <- c(error_vect, "No filename_1 found, upload dataset at Setup page")
        }
        else if (is.null(samples_ref()) || length(samples_ref()) == 0) {
            error_vect <- c(error_vect, "No mapped samples found, perform sample mapping at Setup page")
        }

        if (!is.null(reactive_vals$filename_2()) && (is.null(samples_comp()) || length(samples_comp()) == 0)) {
            error_vect <- c(error_vect, "No mapped samples found for second dataset, perform mapping at Setup page to show second plot")
        }

        if (is.null(reactive_vals$design_1())) {
            error_vect <- c(error_vect, "No design_1 found, upload dataset at Setup page")
        }
        
        total_text <- paste(error_vect, collapse="<br>")
        HTML(sprintf("<b><font size='5' color='red'>%s</font></b>", total_text))
    })

    output$loadings_plot1 <- renderPlot({
        make_loadings_plot(pca_obj1(), "Loadings PCA 1", display_count=10)
    })
    
    output$loadings_plot2 <- renderPlot({
        make_loadings_plot(pca_obj2(), "Loadings PCA 2", display_count=10)
    })
    
    has_value <- function(design_col) {
        design_col != "None" && design_col != ""
    }
    
    output$pca_plot1 <- renderPlotly({
        
        if (has_value(input$color_data1)) color_col <- input$color_data1
        else color_col <- NULL
        
        if (has_value(input$shape_data1)) shape_col <- input$shape_data1
        else shape_col <- NULL
        
        if (has_value(input$sample_data1)) sample_col <- input$sample_data1
        else sample_col <- NULL
        
        plt <- make_pca_plt(
            design_ref(),
            pca_obj1(),
            input$pc_comp_1_data1,
            input$pc_comp_2_data1,
            color_col,
            shape_col,
            sample_col,
            input$dot_size,
            show_labels = input$show_labels_data, 
            color_as_fact = input$data1_as_factor
        ) %>% ggplotly() %>% layout(dragmode="select")
        
        plt
    })
    
    output$pca_plot2 <- renderPlotly({
        
        if (has_value(input$color_data2)) color_col <- input$color_data2
        else color_col <- NULL
        
        if (has_value(input$shape_data2)) shape_col <- input$shape_data2
        else shape_col <- NULL
        
        if (has_value(input$sample_data2)) sample_col <- input$sample_data2
        else sample_col <- NULL
        
        plt <- make_pca_plt(
            design_comp(),
            pca_obj2(),
            input$pc_comp_1_data2,
            input$pc_comp_2_data2,
            color_col,
            shape_col,
            sample_col,
            input$dot_size,
            show_labels = input$show_labels_data, 
            color_as_fact = input$data2_as_factor
        ) %>% ggplotly() %>% layout(dragmode="select")
        plt
    })
}
