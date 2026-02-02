setup_pca_ui <- function(id) {
    ns <- shiny::NS(id)
    tabPanel(
        id,
        fluidPage(
            bar_w_help_and_download("Principal Component Analysis", ns("help"), ns("download_settings"), ns("download_report")),
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

module_pca_server <- function(id, rv, module_name) {
    moduleServer(id, function(input, output, session) {

    render_with_validation <- function(expr, prefix = NULL) {
        tryCatch(
            expr,
            error = function(e) {
                msg <- conditionMessage(e)
                if (!is.null(prefix) && nzchar(prefix)) {
                    msg <- sprintf("%s: %s", prefix, msg)
                }
                shiny::validate(shiny::need(FALSE, msg))
            }
        )
    }

    observeEvent(input$help, {
        shinyalert(
            title = "Help: Principal component visuals",
            text = help_pca, 
            html = TRUE
        )
    })
    
    output$download_settings <- settings_download_handler("pca", input)
    
    output$download_report <- report_generation_handler("pca", params=list(
        input=input,
        setup_input=rv$setup_input(),
        make_ref_pca=make_ref_pca_plot,
        make_comp_pca=make_comp_pca_plot
    ))
    
    filtered_samples_ref <- reactive({
        shiny::validate(need(!is.null(rv$ddf_ref(rv, input$dataset1)), "No design matrix found for reference dataset"))

        if (is.null(input$filter_cond_data1) || input$filter_cond_data1 == "" || input$filter_cond_data1 == "None") {
            rv$ddf_ref(rv, input$dataset1) %>%
                dplyr::select(dplyr::all_of(rv$ddf_samplecol_ref(rv, input$dataset1))) %>%
                unlist()
        }
        else {
            ddf <- rv$ddf_ref(rv, input$dataset1)
            shiny::validate(need(input$filter_cond_data1 %in% names(ddf),
                sprintf("Filter column '%s' not found in design matrix for dataset1", input$filter_cond_data1)))

            filtered_samples <- ddf %>%
                dplyr::filter(.data[[input$filter_cond_data1]] %in% input$display_levels_data1) %>%
                dplyr::select(dplyr::all_of(rv$ddf_samplecol_ref(rv, input$dataset1))) %>%
                unlist()
            filtered_samples
        }
    })
    
    filtered_samples_comp <- reactive({
        shiny::validate(need(!is.null(rv$ddf_comp(rv, input$dataset2)), "No design matrix found for comparison dataset"))
        
        if (is.null(input$filter_cond_data2) || input$filter_cond_data2 == "" || input$filter_cond_data2 == "None") {
            rv$ddf_comp(rv, input$dataset2) %>%
                dplyr::select(dplyr::all_of(rv$ddf_samplecol_comp(rv, input$dataset2))) %>%
                unlist()
        }
        else {
            ddf <- rv$ddf_comp(rv, input$dataset2)
            shiny::validate(need(input$filter_cond_data2 %in% names(ddf),
                sprintf("Filter column '%s' not found in design matrix for dataset2", input$filter_cond_data2)))

            filtered_samples <- ddf %>%
                dplyr::filter(.data[[input$filter_cond_data2]] %in% input$display_levels_data2) %>%
                dplyr::select(dplyr::all_of(rv$ddf_samplecol_comp(rv, input$dataset2))) %>%
                unlist()
            filtered_samples
        }
    })
    
    pca_ddf1 <- reactive({
        shiny::validate(need(!is.null(rv$ddf_ref(rv, input$dataset1)), "No design found for dataset 1 while generating design 1 for PCA"))
        shiny::validate(need(!is.null(rv$samples(rv, input$dataset1)), "No samples found for dataset 1 while generating design 1 for PCA"))

        target_samples <- filtered_samples_ref()
        rv$ddf_ref(rv, input$dataset1) %>%
            dplyr::filter(.data[[rv$ddf_samplecol_ref(rv, input$dataset1)]] %in% target_samples)
    })
    
    pca_ddf2 <- reactive({
        shiny::validate(need(!is.null(rv$ddf_comp(rv, input$dataset2)), "No design found for dataset 2 while generating design 2 for PCA"))
        shiny::validate(need(!is.null(rv$samples(rv, input$dataset2)), "No samples found for dataset 2 while generating design 2 for PCA"))

        target_samples <- filtered_samples_comp()
        rv$ddf_comp(rv, input$dataset2) %>%
            dplyr::filter(.data[[rv$ddf_samplecol_comp(rv, input$dataset2)]] %in% target_samples)
    })
    
    pca_obj1 <- reactive({
        
        shiny::validate(need(!is.null(rv$rdf_ref(rv, input$dataset1)), "No reference dataset found while building PCA 1"))
        shiny::validate(need(!is.null(rv$ddf_ref(rv, input$dataset1)), "No reference design found while building PCA 1"))
        shiny::validate(need(!is.null(rv$samples(rv, input$dataset1)), "No mapped samples found for reference while building PCA 1"))

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
        
        shiny::validate(need(!is.null(rv$rdf_comp(rv, input$dataset2)), "No dataset found while building PCA 2"))
        shiny::validate(need(!is.null(rv$ddf_comp(rv, input$dataset2)), "No design found while building PCA 2"))
        shiny::validate(need(!is.null(rv$samples(rv, input$dataset2)), "No mapped samples found while building PCA 2"))
        
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
        display_levels_data2 <- rv$ddf_comp(rv, input$dataset2)[[input$filter_cond_data2]]
        updateSelectInput(session, "display_levels_data2", choices = display_levels_data2, selected=display_levels_data2)
    }
    
    observeEvent(input$filter_cond_data1, {
        display_levels_data1 <- rv$ddf_ref(rv, input$dataset1)[[input$filter_cond_data1]]
        updateSelectInput(session, "display_levels_data1", choices = display_levels_data1, selected=display_levels_data1)
    })
    
    observeEvent(input$filter_cond_data2, {
        display_levels_data2 <- rv$ddf_comp(rv, input$dataset2)[[input$filter_cond_data2]]
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
    
    observeEvent({
        rv$filedata_1()
        rv$filedata_2()}, {
        choices <- get_dataset_choices(rv)
        updateSelectInput(session, "dataset1", choices=choices, selected=choices[1])
        updateSelectInput(session, "dataset2", choices=choices, selected=choices[1])
    }, ignoreInit=TRUE, ignoreNULL=FALSE)
    
    # observeEvent(rv$filedata_2(), {
    #     choices <- get_dataset_choices(rv)
    #     updateSelectInput(session, "dataset1", choices=choices, selected=choices[1])
    #     updateSelectInput(session, "dataset2", choices=choices, selected=choices[1])
    # })
    
    ########### FUNCTIONS ############

    
    make_loadings_plot <- function(pca_obj, title, display_count) {
        vars <- pca_obj$sdev ** 2
        perc_vars <- vars / sum(vars)
        pcs <- paste0("PC", seq_len(length(perc_vars)))
        plot_df <- data.frame(PC=pcs, perc_var=perc_vars) %>% head(display_count)
        plot_df$PC <- factor(plot_df$PC, levels = head(pcs, display_count))
        ggplot(plot_df, aes(x=.data$PC, y=.data$perc_var)) + geom_col(fill="#000077") + ggtitle(title)
    }
    
    ########### OUTPUTS ############
    
    output$loadings_plot1 <- renderPlot({
        render_with_validation(make_loadings_plot(pca_obj1(), "Loadings PCA 1", display_count=10), prefix="Loadings plot 1")
    })
    
    output$loadings_plot2 <- renderPlot({
        render_with_validation(make_loadings_plot(pca_obj2(), "Loadings PCA 2", display_count=10), prefix="Loadings plot 2")
    })
    
    has_value <- function(design_col) {
        design_col != "None" && design_col != ""
    }
    
    output$pca_pair_plot1 <- renderPlot({
        render_with_validation({
            if (has_value(input$color_data1)) color_col <- input$color_data1
            else color_col <- NULL
            make_pair_pca_plot(
                ddf=pca_ddf1(),
                pca_obj=pca_obj1(),
                color=color_col,
                color_as_fact=input$data1_as_factor,
                pcs=input$pairplot_pcs
            )
        }, prefix="PCA pairplot 1")
    })
    
    output$pca_pair_plot2 <- renderPlot({
        render_with_validation({
            if (has_value(input$color_data2)) color_col <- input$color_data2
            else color_col <- NULL
            make_pair_pca_plot(
                ddf=pca_ddf2(),
                pca_obj=pca_obj2(),
                color=color_col,
                color_as_fact=input$data2_as_factor,
                pcs=input$pairplot_pcs
            )
        }, prefix="PCA pairplot 2")
    })
    
    make_ref_pca_plot <- function() {
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
        
        plt %>% 
            ggplotly(tooltip=c("label", "colour", "x", "y")) %>% 
            plotly::layout(dragmode="select") %>% 
            assign_fig_settings(rv)        
    }
    
    output$pca_plot1 <- renderPlotly({
        render_with_validation(make_ref_pca_plot(), prefix="PCA plot 1")
    })
    
    make_comp_pca_plot <- function() {
        if (has_value(input$color_data2)) color_col <- input$color_data2
        else color_col <- NULL
        
        if (has_value(input$shape_data2)) shape_col <- input$shape_data2
        else shape_col <- NULL
        
        sample_col <- rv$ddf_samplecol_comp(rv, input$dataset2)
        data <- pca_ddf2()
        # data <- rv$ddf_ref(rv, input$dataset2)
        data$Sample <- data[[sample_col]]
        
        plt <- make_pca_plt(
            ddf=data,
            pca_obj=pca_obj2(),
            pc1=input$pc_comp_1_data2,
            pc2=input$pc_comp_2_data2,
            color=color_col,
            shape=shape_col,
            sample=sample_col,
            label_col="Sample",
            title_label=input$dataset2,
            dot_size=input$dot_size,
            show_labels=input$show_labels_data,
            color_as_fact=input$data2_as_factor,
            text_size=input$text_size
        ) 
        
        if (input$custom_title2 != "") {
            plt <- plt + ggtitle(input$custom_title2)
        }
        
        plt %>% 
            ggplotly(tooltip=c("label", "colour", "x", "y")) %>% 
            plotly::layout(dragmode="select") %>% 
            assign_fig_settings(rv)
    }
    
    output$pca_plot2 <- renderPlotly({
        render_with_validation(make_comp_pca_plot(), prefix="PCA plot 2")
    })
    })
}
