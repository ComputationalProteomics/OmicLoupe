setup_correlation_ui <- function(id) {
    ns <- NS(id)
    tabPanel(
        id,
        fluidPage(
            bar_w_help("Correlation", ns("help")),
            fluidRow(
                htmlOutput(ns("warnings")),
                plotOutput(ns("correlation_histograms"))
            )
        )
    )
}

module_correlation_server <- function(input, output, session, rv, module_name) {
    
    observeEvent(input$help, {
        shinyalert(
            title = "Help: Correlation visuals",
            text = help_correlation, 
            html = TRUE
        )
    })
    
    output$correlation_histograms <- renderPlot({
        
        req(rv$mapping_obj())
        req(rv$mapping_obj()$has_correlations())
        
        comb_df <- rv$mapping_obj()$get_combined_dataset(full_entries=FALSE)
        
        make_corr_hist <- function(target_df, aes_x, title, bins=100) {
            mean_corr <- mean(target_df[[aes_x]], na.rm=TRUE)
            ggplot(target_df, aes_string(x=aes_x)) +
                geom_histogram(bins=bins, na.rm=TRUE) +
                geom_vline(xintercept = mean_corr, na.rm=TRUE) +
                ggtitle(sprintf("%s (mean %s)", title, round(mean_corr, 3))) +
                xlim(-1, 1) + xlab("Correlation") + ylab("Count")
        }

        # Spotcheck correlation
        # loess_samp <- rv$selected_cols_obj()$prot_loess_rdf_1819_matched_2out.tsv$samples
        # log2_samp <- rv$selected_cols_obj()$prot_log2_rdf_1819_matched_2out.tsv$samples
        # loess_sdf <- comb_df %>% dplyr::select(paste0("d1.", loess_samp))
        # log2_sdf <- comb_df %>% dplyr::select(paste0("d2.", log2_samp))
        # plt <- ggplot(data.frame(loess=loess_sdf[1, ] %>% unlist(), log2=log2_sdf[1, ] %>% unlist()), aes(x=log2, y=loess)) + geom_point()
        
        ggpubr::ggarrange(
            make_corr_hist(comb_df, "d2.pearson", "Pearson"),
            make_corr_hist(comb_df, "d2.spearman", "Spearman"),
            make_corr_hist(comb_df, "d2.kendall", "Kendall"),
            nrow=3
        )
    }, height = 800)
    
    output$warnings <- renderUI({
        
        error_vect <- c()
        if (is.null(rv$mapping_obj())) {
            error_vect <- c(error_vect, "No loaded data found, load datasets under the 'Setup' tab")
        }
        else if (!rv$mapping_obj()$has_correlations()) {
            error_vect <- c(error_vect, 
                "No correlations assigned, this requires two matched datasets and that the 'Matched samples' setting in 'Setup' is assigned.")
        }
        
        total_text <- paste(error_vect, collapse="<br>")
        HTML(sprintf("<b><font size='5' color='red'>%s</font></b>", total_text))
    })
}


