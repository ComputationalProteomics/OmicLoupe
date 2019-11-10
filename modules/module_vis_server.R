library(ggplot2)
theme_set(theme_classic())
library(ggpubr)

source("R/vis_server_utils.R")
source("R/vis_server_plots.R")

module_visual_server <- function(input, output, session, reactive_vals) {
    
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
        if (input$dataset1 != "" && input$stat_base1 != "") {
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
    
    reactive_plot_df <- reactive({
        stat_cols <- reactive_ref_statcols()
        get_pass_thres_annot_data(
            reactive_vals$mapping_obj()$get_combined_dataset(),
            stat_cols,
            input$pvalue_cutoff, 
            input$fold_cutoff,
            input$pvalue_type_select
        )
    })
    
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
    
    observeEvent(reactive_vals$filedata_2(), {
        choices <- get_dataset_choices(reactive_vals)
        updateSelectInput(session, "dataset1", choices=choices, selected=choices[1])
        updateSelectInput(session, "dataset2", choices=choices, selected=choices[1])
    })
    
    ########## Visualization functions #############
    
    output$correlation_vals <- renderText({
        
        out_string <- calculate_correlation_vals_string(
            reactive_vals$mapping_obj()$get_combined_dataset(), 
            reactive_ref_statcols(), 
            reactive_comp_statcols(),
            input$pvalue_cutoff,
            input$fold_cutoff,
            input$pvalue_type_select
        )
        out_string
    })
    
    output$mini_pvalue_dist <- renderPlot({
        target_stat_col <- input$pvalue_type_select
        minimap_hist(
            reactive_vals$mapping_obj()$get_combined_dataset(), 
            reactive_ref_statcols()[[target_stat_col]],
            input$pvalue_cutoff, 
            input$bin_count, 
            target_stat_col
        )
    })
    
    output$mini_fold_dist <- renderPlot({
        target_stat_col <- "logFC"
        minimap_hist(
            reactive_vals$mapping_obj()$get_combined_dataset(), 
            reactive_ref_statcols()[[target_stat_col]],      
            c(-input$fold_cutoff, input$fold_cutoff), 
            input$bin_count, 
            target_stat_col
        )
    })
    
    output$pvalhists <- renderPlot({
        
        pvaluehists(
            reactive_plot_df(),
            stat_cols1=reactive_ref_statcols(),
            stat_cols2=reactive_comp_statcols(),
            stat_base1=input$stat_base1,
            stat_base2=input$stat_base2,
            bin_count=input$bin_count
        )
    })
    
    output$maplots <- renderPlot({
        plt_list <- scatterplots(
            reactive_plot_df(),
            stat_cols1=reactive_ref_statcols(),
            stat_cols2=reactive_comp_statcols(),
            input$stat_base1,
            input$stat_base2,
            mode="ma"
        )
        ggarrange(plotlist = plt_list, nrow=2, ncol=1)
    })
    
    output$volcanos <- renderPlot({
        plt_list <- scatterplots(
            reactive_plot_df(),
            stat_cols1=reactive_ref_statcols(),
            stat_cols2=reactive_comp_statcols(),
            input$stat_base1,
            input$stat_base2,
            mode="volcano"
        )
        ggarrange(plotlist = plt_list, nrow=2, ncol=1)
    })
    
    output$custom_comparison <- renderPlot({
        
        rdf <- reactive_vals$mapping_obj()$get_combined_dataset()
        custom_comp_plot(
            rdf, 
            reactive_ref_statcols(), 
            reactive_comp_statcols(), 
            input$pvalue_type_select, 
            input$pvalue_cutoff
        )
    })
    
    output$exact_fold_comparison <- renderPlot({
        
        rdf <- reactive_vals$mapping_obj()$get_combined_dataset()
        
        exact_fold_comp_plot(
            rdf,
            reactive_ref_statcols(), 
            reactive_comp_statcols(), 
            input$dataset1,
            input$stat_base1,
            input$stat_base2,
            input$pvalue_type_select,
            input$pvalue_cutoff
        )
    })
}















