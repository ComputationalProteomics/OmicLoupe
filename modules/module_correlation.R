setup_correlation_ui <- function(id) {
    ns <- NS(id)
    tabPanel(
        id,
        fluidPage(
            bar_w_help("Correlation", ns("help")),
            fluidRow(
                column(6, sliderInput(ns("pthres"), "P-value threshold", min=0, max=1, value=0.05)),
                column(6, sliderInput(ns("fdrthres"), "FDR threshold", min=0, max=1, value=0.05))
            ),
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
        
        make_corr_hist <- function(target_df, corr_base, title, has_sig=FALSE, bins=100) {

            cor_str <- sprintf("%s.cor", corr_base)
            if (has_sig) {
                p_str <- sprintf("%s.pval", corr_base)
                fdr_str <- sprintf("%s.fdr", corr_base)
                target_df$sig_type <- ifelse(
                    target_df[[fdr_str]] < input$fdrthres, 
                    "FDR<0.05", 
                    ifelse(target_df[[p_str]] < input$pthres, 
                           "P<0.05", 
                           "Not sig.")
                    )
            }
            else {
                target_df$sig_type <- "NA"
            }
            mean_corr <- mean(target_df[[cor_str]], na.rm=TRUE)
            ggplot(target_df, aes_string(x=cor_str, fill="sig_type")) +
                geom_histogram(bins=bins, na.rm=TRUE) +
                geom_vline(xintercept = mean_corr, na.rm=TRUE) +
                ggtitle(sprintf("%s (mean %s)", title, round(mean_corr, 3))) +
                xlim(-1, 1) + xlab("Correlation") + ylab("Count") + scale_fill_manual(values=c("#aaaaaa", "#990000", "#000099"))
        }

        ggpubr::ggarrange(
            make_corr_hist(comb_df, "d2.pearson", "Pearson", has_sig=TRUE),
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


